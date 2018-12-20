library(dslabs)
library(base)
library(tidyverse)
library(caret)
data("movielens")

#This model leaves out the fact that  groups of movies have similar rating patterns and groups of users
#have similar rating patterns as well. We will discover these patterns by studying the residuals obtained
#after fitting our model.To study these residuals, we will convert the data into a matrix
#so that each user gets a row and each movie gets a column. So yui is the entry in row u and column i.
#User u, movie i.

#For illustration purposes, we will only consider a small subset of movies with many ratings and users
#that have rated many movies. We will use this code to generate our training data.
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

#We add row names and column names:
rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

#and convert them to residuals by removing the column and row effects:
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))

d
#If the model above explains all the signals, and the ??
#are just noise, then the residuals for different movies should be independent from each other. But they are not. 
#Here is an example:
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

#Same with The Godfather and Goodfellas
m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

#Here is one for You've Got Mail and Sleepless in Seattle
m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)


#We can see a pattern
x <- y[, c(m_1, m_2, m_3, m_4, m_5)]
colnames(x)[1:2] <- c("Godfather", "Godfather 2")
cor(x, use="pairwise.complete") %>% knitr::kable()


#Here is an illustration, using a simulation, of how we can use some structure to predict the  
#ru,i. Suppose our residuals r look like this:
q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(q) <- c("Godfather", "Godfather 2", m_3, m_4, m_5)
p <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(p) <- 1:nrow(p)

set.seed(1)
r <- jitter(p %*% t(q))
round(r, 1)

#There seems to be pattern here. In fact, we can see very strong correlation patterns:
cor(r) 

#We can create vectors Q and P, that can explain much of the structure we see. The Q would look like this:
t(q) 

#and it narrows down movies to two groups: gangster and romance. We can also reduce the users to three groups:
p

#But there could be other factors, for example:
set.seed(1)
m_6 <- "Scent of a Woman"
q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(q) <- c("Godfather", "Godfather 2", m_3, m_4, m_5, m_6)
p <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(p) <- 1:nrow(p)

r <- jitter(p %*% t(q), factor=1)
round(r, 1)

#Now we see another factor, a factor that divides users into those that love,
#those that hate, and those that don't care for Al Pacino.
cor(r)

#Now to explain the structure, we need two factors.
#The first one divides gangster movies from romantic comedies.
#The second factor divide Al Pacino movies and non Al Pacino movies.
t(q)

#And we also have two sets of coefficients to describe the users.
p

#Notice that the overall structure of the correlation obtained from the simulated data is not that far off the 
#real correlation:
six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
x <- y[,six_movies]
colnames(x)[1:2] <- c("Godfather", "Godfather 2")
cor(x, use="pairwise.complete")
