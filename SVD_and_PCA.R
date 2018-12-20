
library(dslabs)
library(base)
library(tidyverse)
library(caret)
data("movielens")

#Let's see an example with the movie data. To compute the decomposition, we will make the residuals with NAs equal to 0:
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

#The vectors q are called the principal components and they are stored in this matrix:
dim(pca$rotation)

#While the p vectors which are the user's effects are stored in this matrix.
dim(pca$x)

#We can see the variability of each of the vectors:
plot(pca$sdev)

#and see that just the first few already explain a large percent.
#So for example, with just 50 principal components we're already explaining about half the variability out of a total
#of over 300 principal components.
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

#To see that the principal components are actually capturing something important
#about the data, we can make a plot of for example, the first two
#principal components, but now label the points with the movie that each one of those points is related to.
library(ggrepel)

pcs <- data.frame(pca$rotation, name = colnames(y))

highlight <- filter(pcs, PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1)

pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = highlight, size = 2)
#Just by looking at the top 10 in each direction, we see a meaningful pattern.
#The first PC shows the difference between critically acclaimed movies on one side:
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)


#You can see Pulp Fiction, Seven, Fargo, Taxi Driver, and Hollywood blockbusters on the other.
pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

#While the second PC seems to go from artsy, independent films:
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

#to nerd favorites:
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)