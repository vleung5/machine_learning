#We will use a small subset of data from movielens to build a recommendation systems

library(dslabs)
data("movielens")

head(movielens)

#We can see the number of unique users that provided ratings and for how many unique movies they 
#provided them for:
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#The first thing we notice is that some movies get rated more than others.
#Here's the distribution.
movielens %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

#Our second observation is that some users are more active than others at rating movies:
movielens %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

#So let's create a test set to assess the accuracy of the models we implement.
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]


#To make sure we don't include users and movies in the test set that do not appear in the training set, 
#we remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Let's write a function that computes the RMSE for vectors of ratings and their corresponding predictors:
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Let's starb building the simplest possible recommendation system.
#We're going to predict the same rating for all movies, regardless of the user and the movie.
#We can use a model-based approach.

#We compute the average rating of all movies across all users on the training data
mu_hat <- mean(train_set$rating)
mu_hat

#And then we compute the residual mean squared error on the test set data.
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
#1.05 is pretty big.

#Keep in mind that if you plug in any other number, you get a higher RMSE. For example:
predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)
#> [1] 1.49

#As we go along, we will be comparing different approaches. Let's start by creating a results table with 
#this naive approach:
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#We can again use least squared to estimate the average ranking for a movie in the following way:
fit <- lm(rating ~ as.factor(userId), data = movielens)

#We know that the average ranking for a movie is just the average of rating - mu. We can compute it 
#like this:
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#We can see that these estimates vary substantially:
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#predicted ratings(^Yu,i) =  overall average(^mu) + average rating for a movie (^bi)
#We can use this code and see that our RMSE did drop a little bit.
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

#Can we improve? What about the users?
#Let's compute the average rating for user u, for those that have rated over 100 movies.
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#There are cranky users that rate everything bad and there are others that love every movie they watch.
#We include the term, bu, whic is the user-specific effect.
#So now if a cranky user-- this is a negative bu-- rates a great movie, which will have a positive b i, the effects
#counter each other, and we may be able to correctly predict 
#that this user gave a great movie a three rather than a five, which will happen.
#and that should improve our predictions.

#We shouldn't run lm function because that will crash our computer.
#Instead, we will compute our approximation by computing the overall mean, u-hat, the movie effects, b-hat i,
#and then estimating the user effects, b u-hat, by taking the average of the residuals obtained
#after removing the overall mean and the movie effect from the ratings yui.
#The code looks like this.
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


#We can now construct predictors and see how much the RMSE improves:
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()
#Our residual error dropped down to about 0.88.