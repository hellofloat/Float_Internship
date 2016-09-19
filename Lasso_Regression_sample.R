
# Lasso (Least Absolute Shrinkage and Selection Operator)
# Method for automatically penalizing extra features (can set coefficient of feature to be zero)
# performs both variable selection and regularization
# ridge regression can't zero coefficients
# Another type of regularization method is ElasticNet, it is hybrid of lasso and ridge regression both. 
## It is trained with L1 and L2 prior as regularizer. 
### A practical advantage of trading-off between Lasso and Ridge is that, it allows Elastic-Net to inherit some of Ridge's stability under rotation

# in regular regression we just minimize SSE whereas in lasso regression we minimize SSE + lambda*modulus of beta

# overfitting can be adressed by 2 options 1) reduce no of features 2) regularization - keep all features but reduce magnitude of parameters

# installing glmnet package

# loading glmnet package

library(glmnet)

# glmnet perfroms elasticnet regression whihc is combination lasso and ridge we can only perform lasso by just specifying lasso
# glmnet works with matrices

library(dplyr)

setwd('D:/Float/20160712')

load('usum.Rdata')

# used complete.cases to keep only rows which are complete

usum1 = usum[,-c(1:4,6:13)]

# counting number of nas in each column so as to eliminate those which has significant nas

na_count <- apply(usum1, 2, function(x) sum(length(which(is.na(x)))))

#column nos with na's less than 100

Col <- match(names(na_count[na_count  <= 40]),names(usum1))

usum1 = usum1[,Col]

# removing rows with NAs

usum2 = usum1[complete.cases(usum1),]

# removing high negative balace

usum2 <- usum2[usum2$accounts.balances.checking > -5000, ]

X = usum2[,-1]
X = as.matrix(X)
Y = usum2$RVbankCard

#a <- summary(lm(formula =  RVbankCard ~ ., data = usum2))
#b <- a$coefficients

# Cross-validation to find optimal lambda

CV = cv.glmnet(x = X,y = Y,family = "gaussian",type.measure = "deviance", alpha = 1, nlambda = 100)
plot(CV)

# To know the attributes

attributes(CV)

# calcualte the minimum lambda

min_lambda = CV$lambda.min

# Construct lasso model

lasso.model = glmnet(x= X,y = Y,family = "gaussian",type.gaussian = "covariance", alpha = 1, lambda = min_lambda)

attributes(lasso.model)

coeff <- lasso.model$beta[,1]

sign_coeff <- coeff[coeff!=0]

print(sign_coeff)



# model using logistic regression

X = usum2[,-1]
X = as.matrix(X)
Y = usum2$RVbankCard
Y[Y < 660] = 0
Y[Y >= 660] = 1

# Cross-validation to find optimal lambda

CV = cv.glmnet(x = X,y = Y,family = "binomial",type.measure = "class", alpha = 1, nlambda = 100)
plot(CV)

# calcualte the minimum lambda

min_lambda = CV$lambda.min

# Construct lasso model

lasso.model = glmnet(x= X,y = Y,family = "binomial",type.logistic = "modified.Newton", alpha = 1, lambda = min_lambda)

attributes(lasso.model)

coeff <- lasso.model$beta[,1]

sign_coeff <- coeff[coeff!=0]

print(sign_coeff)

write.csv(sign_coeff,'coeff_lass.csv')
