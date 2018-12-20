#Case Study: more than three classes

#Let's show a more complex example with three classes instead of two.
#Now we will have one, twos, and sevens.


if(!exists("mnist")) mnist <- read_mnist()

set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

## get the quandrants
#temporary object to help figure out the quandrants
row_column <- expand.grid(row=1:28, col=1:28) 
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

#binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

#cbind proportion of pixels in upper right quandrant and
##proportion of pixes in lower rigth quandrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x), 
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])


#Once we're done, we obtain training set and a test set.
#Here we're showing the data for the training set.

train_set %>% 
  ggplot(aes(x_1, x_2, color=y)) + 
  geom_point()


#As an example we will fit a qda model
train_qda <- train(y ~ .,
                   method = "qda",
                   data = train_set)

#Now we will get a matrix of 3 columns, a probability for ones, a probability of twos and a probability for sevens.
#We predict the one with highest probability. So for the first observation, we would predict a two.
predict(train_qda, test_set, type = "prob") %>% head()

#If we use the predict function, with the default setting of just giving you the outcome, we get twos, ones, and sevens.
predict(train_qda, test_set)

#The confusion matrix is a 3x3 table because we can make two kinds of mistakes with the ones,
#two kinds of mistakes with the two, and two kinds of mistakes with the sevens.
confusionMatrix(predict(train_qda, test_set), test_set$y)


#The accuracy's still at one number because it just basically computes how often we make the correct prediction.
confusionMatrix(predict(train_qda, test_set), test_set$y)$overal["Accuracy"]

#We can visualize what parts of the regions are called ones, twos, and seven by simply plotting
#the estimated conditional probability.
GS <- 150
new_x <- expand.grid(x_1 = seq(min(train_set$x_1), max(train_set$x_1), len=GS),
                     x_2 = seq(min(train_set$x_2), max(train_set$x_2), len=GS))
new_x %>% mutate(y_hat = predict(train_qda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))



#Let's see how it looks like for lda.
train_lda <- train(y ~ .,
                   method = "lda",
                   data = train_set)

confusionMatrix(predict(train_lda, test_set), test_set$y)$overal["Accuracy"]


#The accuracy is much worse, and it is because our boundary regions have three lines.
GS <- 150
new_x <- expand.grid(x_1 = seq(min(train_set$x_1), max(train_set$x_1), len=GS),
                     x_2 = seq(min(train_set$x_2), max(train_set$x_2), len=GS))
new_x %>% mutate(y_hat = predict(train_lda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))


#Let's see how it looks like for knn.

train_knn <- train(y ~ .,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)

confusionMatrix(predict(train_knn, test_set), test_set$y)$overal["Accuracy"]

#Look how higher the accuracy is.

#We can see that the estimated conditional probability is much more flexiable.
new_x %>% mutate(y_hat = predict(train_knn, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))


#Note that the reason that qda and, in particularly, lda are not working well is due to lack of fit.
#We can see that by plotting the data and noting that at least the ones are definitely not bivariate normally distributed.
train_set %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") 


#So in summary, generating models can be very powerful but only 
#when we're able to successfully approximate the joint distribution of predictor's condition on each class.