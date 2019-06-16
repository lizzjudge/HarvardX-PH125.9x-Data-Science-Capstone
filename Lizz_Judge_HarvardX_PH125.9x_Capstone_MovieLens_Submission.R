# Lizz Judge 6/16/2019
# Movie Lens Project for HardvardX PH125.9X
# Please see repository at 
# https://github.com/lizzjudge/HarvardX-PH125.9x-Data-Science-Capstone.git

library(tidyverse)

if(!exists("train_set")) train_set <- readRDS("edx.rds")
if(!exists("test_set")) test_set <- readRDS("validation.rds")

# naive prediction: average of all 9 million ratings:
mu <- mean(train_set$rating)

# root mean square error loss function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# loss function value for naive prediction
naive_rmse <- RMSE(train_set$rating, mu)
rmse_results <- tibble(method = "Naive average", RMSE = naive_rmse)

# clean up the test set so that it contains all the same movies and users
# that the train set does
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "genres")

# examine test, genre, and user effects separately

# effect 1: just movie effect
movie_avgs1 <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_m1 = mean(rating - mu))

# predict rating for each entry: add mu and b_m
predicted_ratings_m <- mu + test_set %>%        
  left_join(movie_avgs1, by='movieId') %>% 
  .$b_m1
model_1_rmse <- RMSE(predicted_ratings_m, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="1. Movie Effect Model",
                                     RMSE = model_1_rmse ))
# effect 2: just user effect
user_avgs1 <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u1 = mean(rating - mu))

# predict rating for each entry: add mu and b_u
predicted_ratings_u <- mu + test_set %>%
  left_join(user_avgs1, by='userId') %>% 
  .$b_u1
model_2_rmse <- RMSE(predicted_ratings_u, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="2. User Effect Model",
                                 RMSE = model_2_rmse ))

# model 3: genre effect 
genre_avgs1 <- train_set %>%
  group_by(genres) %>% 
  summarize(b_g1 = mean(rating - mu))

# predict rating for each entry: add mu and b_g
predicted_ratings_g <- mu + test_set %>%
  left_join(genre_avgs1, by='genres') %>% 
  .$b_g1
model_3_rmse <- RMSE(predicted_ratings_g, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="3. Genre Effect Model",
                                 RMSE = model_3_rmse ))

# effect 4: build user effect on top of movie effect
user_avgs2 <- test_set %>% 
  left_join(movie_avgs1, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u2 = mean(rating - mu - b_m1))

predicted_ratings_mu <- test_set %>% 
  left_join(movie_avgs1, by='movieId') %>%
  left_join(user_avgs2, by='userId') %>%
  mutate(pred = mu + b_m1 + b_u2) %>%
  .$pred

model_4_rmse <- RMSE(predicted_ratings_mu, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="4. Movie + User Effects Model",  
                                     RMSE = model_4_rmse ))

# effect 5: build movie effect on top of user effect
movie_avgs2 <- test_set %>% 
  left_join(user_avgs1, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_m2 = mean(rating - mu - b_u1))

predicted_ratings_um <- test_set %>% 
  left_join(user_avgs1, by='userId') %>%
  left_join(movie_avgs2, by='movieId') %>%
  mutate(pred = mu + b_m2 + b_u1) %>%
  .$pred

model_5_rmse <- RMSE(predicted_ratings_um, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="5. User + Movie Effects Model",  
                                 RMSE = model_5_rmse ))

# effect 6: layer genre on top of movie without user effects
genre_avgs3 <- test_set %>% 
  left_join(movie_avgs1, by='movieId') %>%
  group_by(genres) %>%
  summarize(b_g3 = mean(rating - mu - b_m1))

predicted_ratings_mg <- test_set %>% 
  left_join(movie_avgs1, by='movieId') %>%
  left_join(genre_avgs3, by='genres') %>%
  mutate(pred = mu + b_m1 + b_g3) %>%
  .$pred

model_6_rmse <- RMSE(predicted_ratings_mg, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="6. Movie + Genre Effects Model",  
                                 RMSE = model_6_rmse ))

# effect 7: layer genre effect on top of user effect
genre_avgs4 <- test_set %>% 
  left_join(user_avgs1, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g4 = mean(rating - mu - b_u1))

predicted_ratings_ug <- test_set %>% 
  left_join(user_avgs1, by='userId') %>%
  left_join(genre_avgs4, by='genres') %>%
  mutate(pred = mu + b_u1 + b_g4) %>%
  .$pred

model_7_rmse <- RMSE(predicted_ratings_ug, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="7. User + Genre Effects Model",  
                                 RMSE = model_7_rmse ))

# effect 8: build genre effect on top of user effect on top of movie effect
genre_avgs2 <- test_set %>% 
  left_join(movie_avgs1, by='movieId') %>%
  left_join(user_avgs2, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g2 = mean(rating - mu - b_m1 - b_u2))

predicted_ratings_mug <- test_set %>% 
  left_join(movie_avgs1, by='movieId') %>%
  left_join(user_avgs2, by='userId') %>%
  left_join(genre_avgs2, by='genres') %>%
  mutate(pred = mu + b_m1 + b_u2 + b_g2) %>%
  .$pred

model_8_rmse <- RMSE(predicted_ratings_mug, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="8. Movie + User + Genre Effects Model",  
                                 RMSE = model_8_rmse ))

# effect 9: regularization effect on movie averages
# find ideal regularization factor
lambdas <- seq(0, 10, 0.25)

# calculate the sum and count of ratings to build to the regularized average
just_the_sum_9 <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_m = n())
# now calculate rmse as a function of regularization factor, lambda
rmse_lambda_9 <- sapply(lambdas, function(lam){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum_9, by='movieId') %>% 
    mutate(b_m = s/(n_m+lam)) %>%
    mutate(pred = mu + b_m) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmse_lambda_9)  
lambda_m <- lambdas[which.min(rmse_lambda_9)]
# ideal lambda for movie effect is 2.5

# use lambda to correct for (regularize) movie effect term b_m1
movie_avgs1_r <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_mr = sum(rating - mu)/(n()+lambda_m), n_m = n()) 

# plot original and regularized results
tibble(original = movie_avgs1$b_m1, 
       regularized = movie_avgs1_r$b_mr, 
       n = movie_avgs1_r$n_m) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

model_9_rmse <- rmse_lambda[which.min(rmse_lambda_9)]

rmse_results <- bind_rows(rmse_results,
                          tibble(method="9. Movie Effects Regularization Model",  
                                 RMSE = model_9_rmse ))

# effect 10: is there a different lambda for just the user effect?
just_the_sum_10 <- train_set %>% 
  group_by(userId) %>% 
  summarize(s = sum(rating - mu), n_u = n())
# now calculate rmse as a function of regularization factor, lambda
rmse_lambda_10 <- sapply(lambdas, function(lam){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum_10, by='userId') %>% 
    mutate(b_ur = s/(n_u+lam)) %>%
    mutate(pred = mu + b_ur) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmse_lambda_10)  
lambda_u <- lambdas[which.min(rmse_lambda_10)]
# ideal lambda for movie effect is 5.5

model_10_rmse <- rmse_lambda[which.min(rmse_lambda_10)]

rmse_results <- bind_rows(rmse_results,
                          tibble(method="10. User Effects Regularization Model",  
                                 RMSE = model_10_rmse ))

# use lambda to correct for (regularize) user effect term b_u1
user_avgs1_r <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_ur = sum(rating - mu)/(n()+lambda_u), n_u = n()) 

# plot original and regularized results
tibble(original = user_avgs1$b_u1, 
           regularized = user_avgs1_r$b_ur, 
           n = user_avgs1_r$n_u) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# effect 11: what about genre?
just_the_sum_11 <- train_set %>% 
  group_by(genres) %>% 
  summarize(s = sum(rating - mu), n_g = n())
# now calculate rmse as a function of regularization factor, lambda
rmse_lambda_11 <- sapply(lambdas, function(lam){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum_11, by='genres') %>% 
    mutate(b_gr = s/(n_g+lam)) %>%
    mutate(pred = mu + b_gr) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmse_lambda_11)  
lambda_g <- lambdas[which.min(rmse_lambda_11)]
# ideal lambda for genre effect is 0.75

model_11_rmse <- rmse_lambda[which.min(rmse_lambda_11)]

rmse_results <- bind_rows(rmse_results,
                          tibble(method="11. Genre Effects Regularization Model",  
                                 RMSE = model_11_rmse ))

# use lambda to correct for (regularize) genre effect term b_g
genre_avgs1_r <- train_set %>% 
  group_by(genres) %>% 
  summarize(b_gr = sum(rating - mu)/(n()+lambda_g), n_g = n()) 

# plot original and regularized results
tibble(original = genre_avgs1$b_g, 
       regularized = genre_avgs1_r$b_gr, 
       n = genreComb_avgs1_r$n_g) %>%
  ggplot(aes(original, regularized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# effect 12: Pose the question: Is there a single lambda that minimizes rmse
# for all effects?
rmse_lambda <- sapply(lambdas, function(lam){
  b_mr <- train_set_reduced %>% 
    group_by(movieId) %>%
    summarize(b_mr = sum(rating - mu)/(n()+lam))
  b_ur <- train_set_reduced %>% 
    left_join(b_mr, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_ur = sum(rating - b_mr - mu)/(n()+lam))
  b_gr <- train_set_reduced %>% 
    left_join(b_mr, by="movieId") %>%
    left_join(b_ur, by="userId") %>%
    group_by(genres) %>%
    summarize(b_gr = sum(rating - b_mr - b_ur - mu)/(n()+lam))
  predicted_ratings <- 
    test_set_reduced %>% 
    left_join(b_mr, by = "movieId") %>%
    left_join(b_ur, by = "userId") %>%
    left_join(b_gr, by = "genres") %>%
    mutate(pred = mu + b_mr + b_ur + b_gr) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set_reduced$rating))
})

qplot(lambdas, rmse_lambda)  

lambda_all <- lambdas[which.min(rmse_lambda)]
lambda_all # 4.25 

# predict ratings with lambda_all
b_mr <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_mr = sum(rating - mu)/(n()+lambda_all))
b_ur <- train_set %>% 
  left_join(b_mr, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_ur = sum(rating - b_mr - mu)/(n()+lambda_all))
b_gr <- train_set %>% 
  left_join(b_mr, by="movieId") %>%
  left_join(b_ur, by="userId") %>%
  group_by(genres) %>%
  summarize(b_gr = sum(rating - b_mr - b_ur - mu)/(n()+lambda_all))
predicted_ratings <- 
  test_set %>% 
  left_join(b_mr, by = "movieId") %>%
  left_join(b_ur, by = "userId") %>%
  left_join(b_gr, by = "genres") %>%
  mutate(pred = mu + b_mr + b_ur + b_gr) %>%
  .$pred

model_12_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="12. All Effects Regularization Model--Single lambda",  
                                 RMSE = model_12_rmse ))

# effect 13: let's try using the individual lambdas found earlier
b_mr <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_mr = sum(rating - mu)/(n()+lambda_m))
b_ur <- train_set %>% 
  left_join(b_mr, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_ur = sum(rating - b_mr - mu)/(n()+lambda_u))
b_gr <- train_set %>% 
  left_join(b_mr, by="movieId") %>%
  left_join(b_ur, by="userId") %>%
  group_by(genres) %>%
  summarize(b_gr = sum(rating - b_mr - b_ur - mu)/(n()+lambda_g))
predicted_ratings <- 
  test_set %>% 
  left_join(b_mr, by = "movieId") %>%
  left_join(b_ur, by = "userId") %>%
  left_join(b_gr, by = "genres") %>%
  mutate(pred = mu + b_mr + b_ur + b_gr) %>%
  .$pred

model_13_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="13. All Effects Regularization Model--Multiple lambdas",  
                                 RMSE = model_13_rmse ))
rmse_results