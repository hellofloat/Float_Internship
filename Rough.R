
library(dplyr)

setwd('D:/Float/20160712')

load('usum.Rdata')
load('RiskView.Rdata')

a <- count(usum,'age')  

# observed people betweena ges 19 to 36 are significant

usum_age <- usum %>% filter(age>=19,age<=36)

plot(count(usum_age,'age'))

charts_bank <- usum_age %>% group_by(approved) %>% summarise(avg_income = mean(income.averageMonthly),
                                                        avg_months = mean(months),
                                                        avg_RVscore = mean(RVbankCard),
                                                        avg_spending = mean(spending.averageMonthly),
                                                        avg_overdrafts_month = mean(overdrafts.count.monthly))


# used complete.cases to keep only rows which are complete

usum1 = usum[,-c(2:4,6:8,10:13 )]

# counting number of nas in each column so as to eliminate those which has significant nas

na_count <- apply(usum1, 2, function(x) sum(length(which(is.na(x)))))

#column nos with na's less than 100

Col <- match(names(na_count[na_count  <= 12]),names(usum1))

usum1 = usum1[,Col]

# removing rows with NAs

usum2 = usum1[complete.cases(usum1),]

setwd('D:/Float/Clustering/Wordcloud')
data_total <- read.csv('people_export_2016_06_15.csv')

# selecing only required columns
data_select <- select(data_total, X.distinct_id, campaign, media_source, city,state,Daily_Balance,X.region)

colnames(data_select)[1] <- colnames(usum2)[1]

usum3 <- inner_join(usum2, data_select, by = c('id'))

count(usum3,media_source)

hist(usum3$age,col = usum3$media_source, labels = TRUE)

table(usum3$age,usum3$media_source)


?hist

a <- usum3 %>% group_by(media_source) %>% summarise(avg = mean(age), no = length(age) )


usum4 <- usum3 %>% filter(RVbankCard>=500)

#accounts.balances.depository <10000, accounts.balances.depository > -5000, accounts.credit_info.available < 10000, income.averageMonthly < 10000,  income.averageMonthly != 0)

usum5 <- usum4 %>% select(#RVbankCard,
                          age,months) 

                          #,accounts.balances.depository
                          #,accounts.credit_info.available
                          #,months
                          #,income.averageMonthly
                          #,income.transactions.monthly
                          #,spending.averageMonthly
                          



# Normalization

m <- apply(usum5,2,mean)
s <- apply(usum5,2,sd)
Z <- scale(usum5,m,s)

# calculating euclidean distance
distance <- dist(Z)

# cluster dendogram with complete linkage

hc.c <- hclust(distance)
plot(hc.c)

# Cluster Membership

member.c <- cutree(hc.c,3)
table(member.c)


# Cluster means

aggregate(Z,list(member.c),mean)
clusters <- aggregate(usum4,list(member.c),mean)
clusters

plot(usum3$age,usum3$history.days)

#silhouette plot
plot(silhouette(cutree(hc.c,3),distance))

#k-means
kc <- kmeans(Z,3)

kc$size
#attributes(kc)
member.kc <- kc$cluster
clusters <- aggregate(usum4,list(member.kc),mean)
clusters

kc$centers

cbind(usum3,kc$cluster)
plot(usum4$months,usum4$RVbankCard,col = kc$cluster)
usum2$RVbankCard <- ifelse(usum2$RVbankCard < 660, "Rejected", "Approved")
table(usum2$RVbankCard,kc$cluster)
