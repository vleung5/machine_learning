#Naive Bayes
#Methods that model the joint distribution of y and the predictors x are referred as "generative models".
#The most general generative model: Naive Bayes

#We can get the data and generate training and test set using this code.
library(caret)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#In this example, the naive Bayes approach is particularly appropriate.
#Because we know that the normal distribution is a very good approximation of the conditional distributions
#of height given sex for both classes, females and males.
#This implies that we can approximate the conditional distributions by simply estimating averages and standard
#deviations from the data like this:
params <- train_set %>% 
  group_by(sex) %>% 
  summarize(avg = mean(height), sd = sd(height))
params


#The prevalence (pi) is equal to the probability of y = 1,  pi = Pr(Y=1).  This is the proportion of females.
pi <- train_set %>% 
  summarize(pi=mean(sex=="Female")) %>% 
  .$pi
pi


#Now we can use our estimates of average and standard deviations to get the actual rule.
#We get the conditional distributions, f0 and f1, and then we use Bayes theorem to compute the naive Bayes estimate
#of the conditional probability.
x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))


#Since we only have 23% women, as discussed previously, our sample has a much lower prevalence than the general population.
#This will affect our accuracy due to the low sensitivity, as you can see here:
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

#This is because the algorithm gives more weight to specificity to account for the low prevalence.
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))


#We can change pi hat to 0.5 to force our estimate of pi to be different
p_hat_bayes_unbiased <- f1*0.5 / (f1*0.5 + f0*(1-0.5)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> 0.5, "Female", "Male")


#Now we have a better balance
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

#This plot shows us that the new rule also gives us a very intuitive cutoff between 66 and 67,
#which is about the middle of the female and male average heights.
qplot(x, p_hat_bayes_unbiased, geom = "line") + 
  geom_hline(yintercept = 0.5, lty = 2) + 
  geom_vline(xintercept = 67, lty = 2)
