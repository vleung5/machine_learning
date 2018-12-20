
#we will compare Knn to logistic regression, which will be the standard we need to beat.

#We will compute the glm predictions
library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
#0.76 accuracy

#Now let's use knn
#Here is one way:
knn_fit <- knn3(y ~ ., data = mnist_27$train)

#Second way to use knn
#The second way to call this function is that the first argument being the matrix predictors and 
#the second, a vector of outcomes.
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

#Now, let's specify the number of neighbors to include, k = 5
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)


#In this case, since our dataset is balanced and we care just as much about sensitivity as we do about 
#specificity, we will use accuracy to quantify performance.
#The predict function for knn produces a probability for each class. So we keep the probability of being 
#a 7 as the estimate  p^ (x1,x2)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]