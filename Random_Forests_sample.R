
# Random forests - As ensemble learning method for classification and regression operate
# by constructing a multitude of decision trees

# why use random forests?
# Reasonable fast but very easy to use
# handles sparse data/missing data
# overcome problems with overfitting

# techniques used
# true bagging - random sample with replacement
# random subset of the features
# voting - allows different trees


data(iris)

library(randomForest)

# gives 100 samples between 1 to 150

s <- sample(150,100)

iris_train <- iris[s,]
iris_test <- iris[-s,]

# default no fo trees are 500, if we want to increase use ntree = 600

rfm <- randomForest(Species~.,iris_train)

p <- predict(rfm,iris_test)
table(iris_test[,5],p)
mean(iris_test[,5]==p)

# getting the importance of individual factor. So can be used as feature selection when using other algorithms

importance(rfm)

# if you want to know the structure of tree no 2

getTree(rfm,2)


# using float data

library(dplyr)
###############################Load Data from Local ETL##################

###############################Add your working directory######################

setwd('D:/Float/20160712')
#load('RiskView.Rdata')
#load('ubank.Rdata')
load('usum.Rdata')

# gives 500 samples between 1 to 664

s <- sample(dim(uval2)[1],dim(uval2)[1]*3/4)

uval2_dtree <- uval2[,-1]

uval2_dtree$approved <- as.factor(uval2_dtree$approved)

uval2_dtree$approved[uval2_dtree$approved == "TRUE"] <- c("Approved") 
uval2_dtree$approved[uval2_dtree$approved == "FALSE"] <- c("NoTApproved") 

uval_train <- uval2_dtree[s,-c(2:11)]
uval_test <- uval2_dtree[-s,-c(2:11)]

# default no fo trees are 500, if we want to increase use ntree = 600

str(uval_train)

rfm <- randomForest(approved ~ .,uval_train)

p <- predict(rfm,uval_test)

table(uval_test[,1],p)
mean(uval_test[,1]==p)

# getting the importance of individual factor. So can be used as feature selection when using other algorithms

a <- importance(rfm)

write.csv(a,'importance.csv')
# if you want to know the structure of tree no 2

getTree(rfm,2)



library(dplyr)
###############################Load Data from Local ETL##################

###############################Add your working directory######################
setwd('D:/Float/Internship Materials/Data')
load('uval2.Rdata')

# gives 500 samples between 1 to 664

s <- sample(dim(uval2)[1],dim(uval2)[1]*3/4)

uval2_dtree <- uval2[,-1]

uval2_dtree$approved <- as.factor(uval2_dtree$approved)

uval2_dtree$approved[uval2_dtree$approved == "TRUE"] <- c("Approved") 
uval2_dtree$approved[uval2_dtree$approved == "FALSE"] <- c("NoTApproved") 

uval_train <- uval2_dtree[s,-c(2:11)]
uval_test <- uval2_dtree[-s,-c(2:11)]

# default no fo trees are 500, if we want to increase use ntree = 600

str(uval_train)

rfm <- randomForest(approved ~ .,uval_train)

p <- predict(rfm,uval_test)

table(uval_test[,1],p)
mean(uval_test[,1]==p)

# getting the importance of individual factor. So can be used as feature selection when using other algorithms

a <- importance(rfm)

write.csv(a,'importance.csv')
# if you want to know the structure of tree no 2

getTree(rfm,2)


#Regression

library(dplyr)
###############################Load Data from Local ETL##################

###############################Add your working directory######################
setwd('D:/Float/Internship Materials/Data')
load('uval2.Rdata')

# gives 500 samples between 1 to 664

s <- sample(dim(uval2)[1],dim(uval2)[1]*3/4)

uval2_dtree <- uval2[,-c(1:4,6:12)]

uval_train <- uval2_dtree[s,-c(2:11)]
uval_test <- uval2_dtree[-s,-c(2:11)]

# default no fo trees are 500, if we want to increase use ntree = 600

str(uval_train)

rfm <- randomForest(approved ~ .,uval_train)

p <- predict(rfm,uval_test)

table(uval_test[,1],p)
mean(uval_test[,1]==p)

# getting the importance of individual factor. So can be used as feature selection when using other algorithms

a <- importance(rfm)

write.csv(a,'importance.csv')
# if you want to know the structure of tree no 2

getTree(rfm,2)
