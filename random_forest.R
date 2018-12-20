#Random Forest
#Improve prediction perfoormance and reduce instability by averaging multiple decision trees,
#a forest of trees constructed with randomness

library(randomForest)

#Let's do a random forest on the poll data
fit <- randomForest(margin~., data = polls_2008) 

#We can see the algorithm improves as we add more trees:
plot(fit)

#Here is the final result:
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

#Let's do a random forest on the 2 or 7 example:
train_rf <- randomForest(y ~ ., data=mnist_27$train)

confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]


#And here's what the conditional probabilities look like.
plot_cond_prob(predict(train_rf, mnist_27$true_p, type = "prob")[,2])


#The random forest above is a little bit too wiggly. We want something smoother.
#Let's use the caret package and use Rborist, it is a little faster.
fit <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
#> Accuracy 
#>    0.795
