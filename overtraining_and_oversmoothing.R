#Overtraining and Oversmoothing

#Note that we have higher accuracy in the train set compared to the test set:
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, 
                reference = mnist_27$train$y)$overall["Accuracy"]
#> Accuracy 
#>    0.882

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>    0.815

#Overtraining are wrise when k=1, you are your closest neighbor. So you have "perfect" accuracy
#in your training data.
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, 
                reference=mnist_27$train$y)$overall["Accuracy"]

#But accuracy is worse on test set.  The accuracy is worse than logistic regression.
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]


#Let's try a larger k, k =401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>     0.79

#Looks almost the same as logistic regression.  The k is so large that it does not permit enough flexibility.
#We almost included half the data to compute each single estimated conditional probability.
#This is called oversmoothing.

#What can we do? WE can try again with different values of K.  Let's try odd numbers between 3 and 251.
ks <- seq(3, 251, 2)

#Now we use the map_df function to repeat the above for each one. For comparative purposes, we will compute 
#the accuracy by using both the training set (incorrect) and the test set (correct):
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  list(train = train_error, test = test_error)
})


#We can see that for k=41, it looks quite similar to the true conditional probability:
p1 <- plot_cond_prob() + ggtitle("True conditional probability")

knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 41)
p2 <- plot_cond_prob(predict(knn_fit, newdata = mnist_27$true_p)[,2]) +
  ggtitle("kNN-41 estimate")
grid.arrange(p1, p2, nrow=1)

max(accuracy$test)


