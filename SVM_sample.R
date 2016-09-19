# library e1071 has functions for latent class analysis, short time Fourier transform, fuzzy clustering, 
# support vector machines, shortest path computation, bagged clustering, naive Bayes classifier etc...

library(e1071)
data(iris)
head(iris)

plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species)

col <- c("Petal.Length", "Petal.Width","Species")
# gives 100 samples between 1 to 150

s <- sample(150,100)

iris_train <- iris[s,col]
iris_test <- iris[-s,col]

svmfit <- svm(Species~.,data = iris_train, kernel = "linear", cost = 100, scale = FALSE)

plot(svmfit,iris_train[,col])

tuned <-  tune(svm, Species~., data = iris_train, kernel = "linear", ranges = list(cost= c(0.01,0.1,1,10,100)))
summary(tuned)

p <- predict(svmfit, iris_test[,col],type = "class")

plot(p)
table(p,iris_test[,3])
mean(p==iris_test[,3])