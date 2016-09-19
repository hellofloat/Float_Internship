
#######Configure Connection

library(RODBC)
library(glmnet)
library(Matrix)

c <- odbcConnect("PostgreSQL30")

transaction_class <<- sqlQuery(c,paste0("SELECT *,
                                        CASE WHEN (cat_1 = 'Transfer' AND cat_2 = 'Payroll') OR 
                                        lower(name) LIKE '%payroll%' OR 
                                        lower(name) LIKE '%payrll%' OR
                                        (lower(name) LIKE '%apple inc.%' AND amount >= 400) OR
                                        (lower(name) LIKE '%salary%' AND amount >= 400) OR
                                        (lower(name) LIKE '%gap inc net pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%dfas-in ind in af pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%uc san francisco des:uc pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%remedy temp pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%tgt pay target%' AND amount >= 400) OR
                                        (lower(name) LIKE '%housing auth directpay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%gusto pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%aetna life insur aetna pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%berge mazda pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%brinker intl pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%family options l net=pay %' AND amount >= 400) OR
                                        (lower(name) LIKE '%mac incorporated mac pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%mlg pay%' AND amount >= 400) OR
                                        (lower(name) LIKE '%dfas-cleveland%' AND amount >= 400) OR
                                        lower(name) LIKE '%direct dep%' THEN 'Payroll'
                                        WHEN lower(name) LIKE '%payday%' OR
                                        lower(name) LIKE '%1st money center%' OR
                                        lower(name) LIKE '%check into cash%' OR
                                        lower(name) LIKE '%express money center%' OR
                                        lower(name) LIKE '%lend up%' OR
                                        lower(name) LIKE '%lendup%' OR
                                        lower(name) LIKE '%loan by phone%' OR
                                        lower(name) LIKE '%quik check%' OR
                                        lower(name) LIKE '%payday%' OR
                                        lower(name) LIKE '%lending%' OR
                                        lower(name) LIKE '%gateway one lend%' OR
                                        lower(name) LIKE '%sierra lend%' OR
                                        lower(name) LIKE '%maxlend%' OR
                                        lower(name) LIKE '%lend up ca%' OR
                                        lower(name) LIKE '%lend me now%' OR
                                        lower(name) LIKE '%plain green%' OR
                                        lower(name) LIKE '%plaingreen%' OR
                                        lower(name) LIKE '%rapidloan%' OR
                                        lower(name) LIKE '%rapid loan%' OR
                                        lower(name) LIKE '%hollywood check%' OR
                                        lower(name) LIKE '%opploans%' OR
                                        lower(name) LIKE '%sawbucks%' THEN 'paydays'
                                        WHEN ((cat_1 = 'Payment' AND cat_2 = 'Credit Card') OR
                                        lower(name) LIKE '%amex%' OR
                                        (lower(name) LIKE '%american express%' AND lower(name) NOT LIKE '%serve%') OR
                                        lower(name) LIKE '%visa%' OR
                                        lower(name) LIKE '%mastercard%' OR
                                        (lower(name) LIKE '%discover%' AND 
                                        lower(name) NOT LIKE '%tour%' AND 
                                        lower(name) NOT LIKE '%bank%') OR
                                        lower(name) LIKE '%capital one%') AND amount < 0 THEN 'Credit Card'
                                        WHEN (cat_1 = 'Payment' AND cat_2 = 'Loan') OR
                                        lower(name) = '%loan%' THEN 'Loan'
                                        WHEN cat_2 = 'Overdraft' OR cat_2 = 'Insufficient Funds' THEN 'Overdraft'
                                        WHEN cat_2 = 'Late Payment' OR cat_2 = 'Interest Charged' THEN 'Dues'
                                        ELSE NULL END AS adj_cat
                                        FROM trans ORDER BY account_id, 
                                        date desc"))

transaction_class <- transaction_class[!is.na(transaction_class$adj_cat),]

#split the data 3 into 4 parts 

split <- sample(nrow(transaction_class),floor(0.75*nrow(transaction_class)))

# create training and test data

train <- transaction_class[split,]
test <- transaction_class[-split,]

# create document term matrix

#dtMatrix <- create_matrix(transactions1["name"])

dtMatrix <- create_matrix(train["name"], language = "english", removePunctuation = TRUE, stripWhitespace = TRUE, toLower = TRUE, removeNumbers = TRUE, stemWords = TRUE, removeStopwords=TRUE, removeSparseTerms = .95)

X <- as.matrix(dtMatrix)
Matrix(X,sparse=TRUE)
Y = train$adj_cat


# Cross-validation to find optimal lambda

CV = cv.glmnet(x = X,y = Y,family = "multinomial",type.measure = "class", alpha = 1,nfolds = 10)

plot(CV)

# To know the attributes

attributes(CV)

# calculate the minimum lambda

min_lambda = CV$lambda.min

# Construct lasso model

model = glmnet(x= X,y = Y,family = "multinomial",type.logistic = "Newton", alpha = 1,lambda = 0.00002)

attributes(model)

model$lambda
coeff <- model$beta

sign_coeff <- coeff[coeff!=0]

# Building model on test data

#dtMatrix <- create_matrix(transactions1["name"])

dtMatrix_pred <- create_matrix(test["name"], language = "english", removePunctuation = TRUE, stripWhitespace = TRUE, toLower = TRUE, removeNumbers = TRUE, stemWords = TRUE, removeStopwords=TRUE, removeSparseTerms = .95)

X_pred <- as.matrix(dtMatrix_pred)

Matrix(X_pred,sparse=TRUE)

pred <- predict(lasso.model,X_pred, type="class", s = 4.029784e-05)
  
