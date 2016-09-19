
library(RODBC)
library(tm)
library(RTextTools)

c <- odbcConnect("PostgreSQL30")

transactions1 <<- sqlQuery(c,paste0("SELECT * FROM trans where cat_1 <> '' ORDER BY account_id, date desc"))

a <- transactions1$name[transactions1$cat_2 == "Payroll"]

write.csv(a,'payrolltrans.csv')
# create document term matrix

#dtMatrix <- create_matrix(transactions1["name"])

dtMatrix <- create_matrix(transactions1["name"], language = "english", removePunctuation = TRUE, stripWhitespace = TRUE, toLower = TRUE, removeNumbers = TRUE, stemWords = TRUE, removeStopwords=TRUE, removeSparseTerms = .7)

# To display detailed information on a corpus or a term-document matrix (we use inspect function)

inspect(dtMatrix)

# to find freq of observed terms more than 100 times

findFreqTerms(dtMatrix,100)


# Configure the training data

container <- create_container(dtMatrix, transactions1$cat_1, trainSize = 1:length(transactions1$cat_1),testSize = NULL, virgin=FALSE)

# train a SVM Model

model <- train_model(container, "SVM", kernel="linear", cost=1)

transactions_for <<- sqlQuery(c,paste0("SELECT * FROM trans where user_id = '6f660973-9245-4c24-af48-b9839bd262f6' AND cat_1 = '' ORDER BY account_id, date desc"))

# create a prediction document term matrix

# trace("create_matrix", edit=T)

predMatrix <- create_matrix(transactions_for["name"],originalMatrix=dtMatrix)

# create the corresponding container
predSize = length(transactions_for$name);
predictionContainer <- create_container(predMatrix, transactions_for$cat_1, testSize=1:predSize, virgin=FALSE)

# predict
results <- classify_model(predictionContainer, model)
results

results[results$SVM_PROB >= 0.5,]