
#Unfortunately, the Rborist implementation of Random Forest does not yet 
#support importance calculations. So we demonstrate with a quick fit using the randomForest package.
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)

#The following function computes the importance of each feature
imp <- importance(rf)
imp

#We can see which features are most being used by plotting an image.  We can see where the important features are.
image(matrix(imp, 28, 28))

#We can compare what we got with k-nearest neighbors to what we got with random forest.
p_max <- predict(fit_knn, x_test[col_index])
p_max <- apply(pmax, 1, max)
ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]