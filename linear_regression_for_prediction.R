library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Suppose you're tasked with building a machine learning algorithm that predicts the son's height y using
#the father's height x.

#Let's start by generating some testing and train sets using this code
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#In this case, if we're ignoring the father's heights and guessing the son's height, we would guess the
#average height of son's.  So our prediction would be the average:
avg <- mean(train_set$son)
avg

#The R squared loss is about 6.60.
mean((avg - test_set$son)^2)

#Least squares used as a method for estimating the slope and intercept. We can use this to get that 
#fitted model.
fit <- lm(son~father, data=train_set)
fit$coef

#This gives us an estimate of the conditional expectation using this formula
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

#Now we see that this provide an improvement over our guessing approach which gave us a loss of 6.6.
#NOw we get a loss of 4.78, a little bit lower.

