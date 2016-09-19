
##################Underwriting Approval Function#######################

#A program to bring back the data on a single user in a review friendly format

library(dplyr)
###############################Load Data from Local ETL##################

###############################Add your working directory######################
setwd('D:/Float/Internship Materials/Data')
load('uval2.Rdata')

# prepare the data

data <- uval2[,13:130]

# Run PCA

PC <- princomp(data, corre=TRUE , score=TRUE)

summary(PC)

# component 1 plus component 2 can give us 88% variation

# plot the variances using bars

plot(PC)

# scree plot or line chart

screeplot(PC,type="line",main="screeplot")

# plot between comp 1 and comp 2

biplot(PC)

# 
PC$scores[1:10]

a <- PC$loadings

write.csv(a,'a.csv')

