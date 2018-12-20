data("mnist_27")

#We can explore this data by plotting the two predictors and use colors to denote the labels.
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

#Let's start with logistic regression:
fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")

#And now we can build a decision rule based on the estimate of the conditional probability.
#Whenever it is bigger than 0.5, we predict a seven.
#Whenever it's not, we predict a two.
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

#We see that we achieve an accuracy of 79%. But can we do better?
#We can access and plot the true conditional probability with this set of data (not typical in practice)
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()


#We will improve this plot by choosing better colors.
#And we'll also draw a curve that separates the pairs, x1, x2,
#for which the conditional probably is bigger than 0.5 and lower than 0.5.
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")


#note that with logistic regression  p^(x,y) has to be a plane and, as a result, the boundary defined 
#by the decision rule is given by  p^(x,y) = 0.5
#which implies the boundary can’t be anything other than a straight line:
p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 


#Now to see where the mistakes were made, we can again plot the test data with x1 and x2 plotted
#against each other and color used to show the label.
p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 
