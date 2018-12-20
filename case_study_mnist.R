col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)


#Use kNN on the MNIST data
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
ggplot(train_knn)


#Above takes several minutes to run, we can run a smaller subset of data.
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index,] 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#Now we can fit it to the entire dataset
fit_knn<- knn3(x[ ,col_index], y,  k = 5)


#The accuracy is almost 0.95.
y_hat_knn <- predict(fit_knn, 
                     x_test[, col_index], 
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
#> Accuracy 
#>    0.948


#From the specificity and sensitivity, we also see that 
#8s are the hardest to detect and the most commonly incorrectly predicted digit is 7.
cm$byClass[,1:2]
#>          Sensitivity Specificity
#> Class: 0       1.000       0.996
#> Class: 1       0.991       0.990
#> Class: 2       0.883       0.998
#> Class: 3       0.955       0.996
#> Class: 4       0.928       0.996
#> Class: 5       0.969       0.991
#> Class: 6       0.990       0.999
#> Class: 7       0.958       0.988
#> Class: 8       0.876       1.000
#> Class: 9       0.933       0.990



#Now let's try random forest using Rborist (it is faster than the random forest package)

library(Rborist)
#> Loading required package: Rcpp
#> Rborist 0.1-8
#> Type RboristNews() to see new features/changes/bug fixes.
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1) , predFixed = c(10, 15, 35))

train_rf <-  train(x[ , col_index], 
                   y, 
                   method = "Rborist", 
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune
#>   predFixed minNode
#> 2        15       1


#Now we are ready to optimize our final tree. Now we're going to set the number of trees
#to a larger number.
fit_rf <- Rborist(x[, col_index], y, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

#use confusion matrix to see the accuracy
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)

#This is better than kNN.
cm$overall["Accuracy"]
#> Accuracy 
#>    0.953

