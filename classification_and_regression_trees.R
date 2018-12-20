#Classification and Regression Trees (CART)

#We will use a new datsaase that inclues the breakdown of the composition of olive into eight fatty acids.

data("olive")
head(olive)

#We will try to predict the region using the fatty acid composition values as predictors.
table(olive$region)

#We remove the area column because we don't use it as a predictor.
olive <- select(olive, -area)

#Let's see how we do with knn
library(caret)
fit <- train(region ~ .,
             method = "knn",
             tuneGrid = data.frame(k=seq(1,15,2)), data = olive)

ggplot(fit)
#We get an accuracy of 0.97.

#Exploring the data, if we look at the distribution of each predictor stratified by region,
#We see that eicosenoic is only present in Southern Italy and that linolenic separates Northern Italy 
#from Sardinia. This implies that we should be able to build an algorithm that predicts perfectly! 
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free")


#We can see this clearly by plotting the values for these two predictors:
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p


#We can, by eye, construct a prediction rule that partitions the predictor space like this:
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.535, xend = 0.065, yend = 10.535, color = "black", lty = 2)
  
#We can draw a decision tree.


################################################################################################

#Regression Tree
#When the outcome is continuous, we call this method regression trees. We will use a continuous case, 
#the 2008 poll data introduced earlier, to describe the basic idea of how we build these algorithms.

library(rpart)
data("polls_2008")
qplot(day, margin, data = polls_2008)

#partition data recursively
fit <- rpart(margin ~ ., data = polls_2008)

#We can visually see where the splits were made with this:
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

#The final estimate looks like this:
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#1. Complexity parameter: To avoid spliting down until every point is its own partition (RSS goes all the way
#down to 0 since the average of one value is that same value), the algorithm sets a minimum for how much
#the RSS must improve for another partiion to be added.  This is the complexity paramter (cp).

#2. minsplit: The algorithm sets a minimum number of observations to be partitioned. The default is 20.

#3. minbucket: The algorithm also sets a minimum on the number of observations in each partition. 
#If the optimal split results in a partition with less observations than this minimum, it is not considered.
#The default is round(minsplit/3)

#As expected, if we set cp = 0 and minsplit=2, then our prediction is our original data:
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#We can also prune a tree to be smaller by snipping of partiions that do not meet a CP criterion.
pruned_fit <- prune(fit, cp=0.01)
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#How do we pick CP? We can use cross-validation.  We write this and pick the best cp.
library(caret)
train_rpart <- train(margin ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)


#To see the resulting tree, we access the finalModel and plot it:
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)


#And because we only have one predictor, we can actually plot ^f(x):
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


############################################################################################
#An example of how a classification trees perform on the two or seven sample.
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0,0.1,len=25)),
                     data = mnist_27$train)
plot(train_rpart)

#We use this tree and see what our accuracy is:
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#Note that this is better than logistic regression but not as good as the kernel methods.