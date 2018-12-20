#we can apply regression to categorical data as well.
library(dslabs)
library(caret)
library(dplyr)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)


#What is the condition of probably of being female if you're 66 inches tall?
train_set %>%
  filter(round(height)==66) %>%
  summarize(mean(sex=="Female"))

#We'll repeat the same exercise but for 60 inches, 61 inches, et cetera

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

#Assume that the conditional probability of Y being equals 1 given X is a line -- intercept plus slope 
#times height.  If we convert the factors to 0s anad 1s, we can estimate beta 0 sand beta 1 with
#least squares using this:
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y ~ height, data = .)

#To form a prediction, we define a decision rule.
#We predict female if the conditional probability is bigger than 50%.
#Now we can use the confusion matrix function to see how we did.
#We see that we got an accuracy of 78.5%.
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)

#Logistic regression is an extension of linear regression that assures us the estimate of the conditional 
#probability is, in fact, between 0 and 1.
#We can fit the logistic regression model with the function GLM.
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y~height, data = ., family = binomial)

#Just like with linear regression, we can obtain predictions using the predict function.
p_hat_logic <- predict(glm_fit, newdata = test_set, type = "response")

#Because we have an estimate of the conditional probability, we can obtain predictions using code like this.
#Accuracy has increased to 80%.
y_hat_logic <- ifelse(p_hat_logic > 0.5, "Female","Male") %>% factor
confusionMatrix(y_hat_logic, test_set$sex)
