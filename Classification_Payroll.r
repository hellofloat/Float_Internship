
#######Configure Connection

library(RODBC)
library(RTextTools)
library(plyr)

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
                                        WHEN lower(name) LIKE '%atm %' OR
                                        lower(name) LIKE '%online banking%' OR 
                                        lower(name) LIKE '%online transfer%' OR 
                                        lower(name) LIKE '%deposit%' OR 
                                        lower(name) LIKE '%square inc%' OR 
                                        lower(name) LIKE '%paypal%' OR 
                                        lower(name) LIKE '%venmo%' OR 
                                        lower(name) LIKE '%teller deposit%' OR
                                        lower(name) LIKE '%online deposit%' OR
                                        lower(name) LIKE '%online + transfer%' OR
                                        lower(name) LIKE '%remote online deposit%' THEN 'Transfer'
                                        ELSE NULL END AS adj_cat
                                        FROM trans ORDER BY account_id, 
                                        date desc"))

# selecting only which has no "nas" in "adj_cat" category column
transaction_class_1 <- transaction_class[!is.na(transaction_class$adj_cat),]

# inorder to have balance data across all the adj categories we take only some percentage transactions under "Transfer" Category
a <- transaction_class_1[transaction_class_1$adj_cat == 'Transfer',]

# for now we are only taking 60000 transactions of "Transfer" category
a1 <- a1[sample(nrow(a),60000),]
b <- transaction_class_1[transaction_class_1$adj_cat != 'Transfer',]
transaction_class_1 <- rbind(b,a1)

#split the data 3 into 4 parts (training and test)
split <- sample(nrow(transaction_class_1),floor(0.80*nrow(transaction_class_1)))

# create training and test data
train <- transaction_class_1[split,]
test <- transaction_class_1[-split,]

# count(train, c("adj_cat"))
# create document term matrix
dtMatrix_train <- create_matrix(train["name"], removeSparseTerms = .99)

#language = "english", removePunctuation = TRUE, stripWhitespace = TRUE,  stemWords = TRUE, removeStopwords=TRUE, toLower = TRUE, removeNumbers = TRUE, 
#write.csv(as.matrix(dtMatrix_train),'train.csv')
dtMatrix_train <- as.matrix(dtMatrix_train)

# removing rows with all zeros
train <- train[!(rowSums(dtMatrix_train) == 0),]
dtMatrix_train_Final <- dtMatrix_train[!(rowSums(dtMatrix_train) == 0),]

# Configure the training data
container <- create_container(dtMatrix_train_Final, train$adj_cat, trainSize = 1:length(train$adj_cat),testSize = NULL, virgin=FALSE)

# train a SVM Model

#model <- train_models(container, algorithms = c("SVM","MAXENT","GLMNET","BOOSTING","BAGGING","RF","TREE"))
model <- train_model(container, algorithm = c("SVM"))

## predicting for remaining variables
transaction_class_pred <- transaction_class[is.na(transaction_class$adj_cat),]
dtMatrix_pred <- create_matrix(transaction_class_pred['name'],originalMatrix=dtMatrix_train)
Container_pred <- create_container(dtMatrix_pred, as.numeric(transaction_class_pred$adj_cat), trainSize = NULL, testSize= 1:length(transaction_class_pred$adj_cat), virgin=FALSE)
results <- classify_model(Container_pred, model)

#results <- classify_models(Container_pred, model)
a <- cbind(transaction_class_pred$name,transaction_class_pred$amount, results)
