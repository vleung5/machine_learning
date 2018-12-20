
#distance

set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

#Let's look at the first 3 labels and save them to these variables
y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#We expect the distances between the same number, like this, to be smaller than between different numbers.
sqrt(sum((x_1-x_2)^2))
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_2-x_3)^2))

#or use cross-product from matrix algebra
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

#or use the dist function which computes the distance between each row and produces and object of class dist.
d <- dist(x)
class(d)

#to access the objects of class dist using row and column indices:
as.matrix(d)[1:3,1:3]


#We can also quickly see an image of these distances using the image function:
image(as.matrix(d))

#We can order the distances by labels to see that the 2s are closer to each other and 7s are
#closer to each other.
image(as.matrix(d)[order(y), order(y)])


#To compute the distince between all pairs of the 784 predictors, we can transpose the matrix first
#and then use the dist function
d <- dist(t(x))
dim(as.matrix(d))

#An interesting thing to note here is that, if we pick a predictor (a pixel)
#we can see which pixels are close, meaning that they are either ink together or they don't have ink together.
#Let's just look at the 492nd pixel and let's look at the distances between each pixel and the 492nd pixel.
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))
