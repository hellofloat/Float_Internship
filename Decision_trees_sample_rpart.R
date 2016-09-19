
# decision trees using rpart

data(iris)
dim(iris)
library(rpart)

# gives 100 samples between 1 to 150

s <- sample(150,100)

iris_train <- iris[s,]
iris_test <- iris[-s,]

dtm <- rpart(Species~.,iris_train, method = "class")

plot(dtm)

# but it doesn't give text
# to add text

text(dtm)

# still it is bad. hence we can use library rpart.plot which gives better picture

library(rpart.plot)

rpart.plot(dtm)

# lets predict using the formula

p <- predict(dtm, iris_test, type = "class")

table(iris_test[,5],p)

# limitations
# Good representative train data
# Fairly simple
# Other ensemble techniques like random forest more elegant and sophisticated amchine learnign algorithms

# using float data

library(dplyr)
###############################Load Data from Local ETL##################

###############################Add your working directory######################
setwd('D:/Float/Internship Materials/Data')
load('uval2.Rdata')

# gives 500 samples between 1 to 664

s <- sample(length(uval2),length(uval2)*3/4)

uval2_dtree <- uval2[,-1]

uval_train <- uval2_dtree[s,-c(2:11)]
uval_test <- uval2_dtree[-s,-c(2:11)]

dtm_uval <- rpart(approved ~ .,uval_train, method = "class")

rpart.plot(dtm_uval, type = 4, extra = 101)

p <- predict(dtm_uval, uval_test, type = "class")

table(uval_test[,1],p)
mean(uval_test[,1] == p)