#A simple example of predicting sex -- female or male -- using height.


install.packages("caret")

library(caret)
library(dplyr)
library(purrr)
library(dslabs)
data(heights)

#We start by defining the outcome and predictors, in this example, we have only one preditor

y <- heights$sex
x <- heights$height

#We will split the data into training sets and test sets.
#The argument times in functions is used to define how many random samples of indexes to return.
#The argument p is used to define what proportion of the index represented.
#The argument list is used to decide you want indexes to be returned as a list or not.
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#Use this index to define the training set and test set.
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

#Now let's build two competing algorithms and compare them for overall accuracy.

#(1)
#This one is the simplest possible machine learning algorithm, guessing the outcome.  We are completely
#ignoring the predictor and simply guessing the sex.
y_hat <- sample(c("Male","Female"), length(test_index), replace=TRUE)

#In machine learning, it is useful to use factors to represent the catalorical outcomes.  Our functions 
#developed for machine learning, require or recommend that categorical outcomes be coded as factors.
y_hat <- sample(c("Male","Female"), length(test_index), replace=TRUE)  %>% 
  factor(levels = levels(test_set$sex))

#We can now compute the overall accuracy (the overall proportion that is predicted correctly).
mean(y_hat == test_set$sex)


#Exploratory data suggests we can do better because on average, males are slightly taller than females:
heights %>% group_by(sex) %>%	summarize(mean(height), sd(height))


#Using this information, let's predict male if height is within two standard deviations
#from the average male.
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

#The accuracy goes way up from 50% to 80%, but can we do better?
mean(y == y_hat)


#Remember, it is important that we pick the best value on the training set.  The test set is only for 
#evaluation. We examine the accuracy we obtain with 10 different cutoffs and pick the one yielding the best
#result.

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train_set$height > x, "Male","Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})


data.frame(accuracy = accuracy,
           cutoff = cutoff) %>%
  ggplot(aes(y = accuracy, x = cutoff)) +
  geom_line() +
  geom_point()

max(accuracy)


best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


#Now let's test this cutoff on our test set to make sure accuracy is not overly optimistic.
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#We see that this is a bit lower than the accuracy observed on the training set but it's still better 
#than guessing.


#Test
install.packages("dslabs")
library(dslabs)
library(dplyr)
mnist <- read_mnist()
y <- mnist$train$labels
y[5]
y[6]
sapply(mnist$train, class)
y[5] + y[6]
y[5] > y[6]

##################################
# Confusion Matrix
##################################
#Generally speaking, the overall accuracy can be a deceptive measure.  
#What about female at the cut off? We can tabulate each combination
#of prediction and actual value.
table(predicted = y_hat, actual = test_set$sex)

#If we compute the accuracy separately for each sex, we get male with very high accuracy
# and low accuracy for females.
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

#Why is overall accuracy still so high?  This is because of prevalence.  There are more males in the data
#sets than females.  The heights were collected from 3 data science courses, two of which had more
#males enrolled
prev <- mean(y == "Male")
prev

#This shows us high overall accuracy is possible despite relatively low sensitivity.
#This is because of low prevalence, 23%.  Because prevalence is low, failing to call actual females
# females, low sensitivity, does not lower the accuracy as much as it would have increased if
#incorrectly called males females.
install.packages("e1071")
library(e1071)
confusionMatrix(data = y_hat, reference = test_set$sex)

#Let's rebuild our prediction algorithm but this time maximizing the F score instead of the overall accuracy.
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train_set$height > x , "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(F_1 = F_1,
           cutoff = cutoff) %>%
  ggplot(aes(y = F_1, x = cutoff)) +
  geom_line() +
  geom_point()

#It is maximized at 61% when we use a cutoff of 66 inches.
max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

#Now we can see that it balances the specificity and sensitivity our our confusion matrix:
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)


#Guessing male with higher probability would give us higher accuracy due to the bias in the sample.
#You can see this here, which predicts male.
p <- 0.9
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

#ROC curve for guessing sex approach:
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

#ROC curve for the height-based approach:
cutoffs <- c(50, seq(60,75), 80)
height_cutoff <- map_df(cutoffs, function(x) {
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

#Plotting both curves together we are able to compare sensitivity for different values of specificity:
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

#We can see that we obtain higher sensitivity with the height-based approach for all values of specificity,
#which imply it is a better method.


#When making ROC curves it is often nice to add the cutoff used to the points. 
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text(nudge_y = 0.01)


#But there is one weakness.
#Neither of the measures plotted depend on prevalence, in which case, we may instead make a precision
#recall plot.
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


#From this plot we immediately see that the precision of guessing is not high. This is because the 
#prevalence is low.  If we change positives to mean Male instead of Female, the ROC curve remains 
#the same, but the precision recall plot changes:
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
