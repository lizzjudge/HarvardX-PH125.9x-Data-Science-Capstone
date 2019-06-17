# Lizz Judge 6/16/2019
# Choose Your Own Project for HardvardX PH125.9X
# Please see repository at 
# https://github.com/lizzjudge/HarvardX-PH125.9x-Data-Science-Capstone.git


# predict global sales of video games

library(tidyverse)
library(caret)

# read in data file
if(!exists("video_games")) 
  video_games = read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv", header = TRUE)

# clean up data to remove rows with no sales information or sales
video_games <- video_games[complete.cases(video_games), ]


# naive prediction: average of all global sales:
mu <- mean(video_games$Global_Sales)

# root mean square error loss function
RMSE <- function(true_sales, predicted_sales){
  sqrt(mean((true_sales - predicted_sales)^2))
}

# loss function value for naive prediction
naive_rmse <- RMSE(video_games$Global_Sales, mu)
rmse_results <- tibble(method = "Naive average", RMSE = naive_rmse)

# create partitions for 10-fold cross validation
set.seed(755)
test_index <- createDataPartition(y = video_games$Global_Sales, times = 10,
                                  p = 0.1, list = FALSE) # partition is 10/90


# examine effects separately

# effect 1: just Name effect
rmses_Name <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Name") 
  
  Name_avgs <- train_set %>% 
    group_by(Name) %>% 
    summarize(b_n = mean(Global_Sales - mu))
  
  # predict Name for each entry: add mu and b_n
  predicted_sales_n <- mu + test_set %>%        
    left_join(Name_avgs, by='Name') %>% 
    .$b_n
  
  rmses_Name[i] <- RMSE(predicted_sales_n, test_set$Global_Sales)
}

model_1_rmse <- mean(rmses_Name)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="1. Name Effect Model",
                                 RMSE = model_1_rmse ))

# effect 2: just platform effect
rmses_platforms <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Platform") 
  
  platform_avgs <- train_set %>% 
    group_by(Platform) %>% 
    summarize(b_pl = mean(Global_Sales - mu))
  
  # predict rating for each entry: add mu and b_p
  predicted_sales_pl <- mu + test_set %>%        
    left_join(platform_avgs, by='Platform') %>% 
    .$b_pl
  
  rmses_platforms[i] <- RMSE(predicted_sales_pl, test_set$Global_Sales)
}

model_2_rmse <- mean(rmses_platforms)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="2. Platform Effect Model",
                                 RMSE = model_2_rmse ))

# effect 3: just genre effect
rmses_genres <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Genre") 
  
  genre_avgs <- train_set %>% 
    group_by(Genre) %>% 
    summarize(b_g = mean(Global_Sales - mu))
  
  # predict rating for each entry: add mu and b_g
  predicted_sales_g <- mu + test_set %>%        
    left_join(genre_avgs, by='Genre') %>% 
    .$b_g
  
  rmses_genres[i] <- RMSE(predicted_sales_g, test_set$Global_Sales)
}

model_3_rmse <- mean(rmses_genres)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="3. Genre Effect Model",
                                 RMSE = model_3_rmse ))

# effect 4: just publisher effect
rmses_publisher <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Publisher") 
  
  publisher_avgs <- train_set %>% 
    group_by(Publisher) %>% 
    summarize(b_pu = mean(Global_Sales - mu))
  
  # predict rating for each entry: add mu and b_pu
  predicted_sales_pu <- mu + test_set %>%        
    left_join(publisher_avgs, by='Publisher') %>% 
    .$b_pu
  
  rmses_publisher[i] <- RMSE(predicted_sales_pu, test_set$Global_Sales)
}


model_4_rmse <- mean(rmses_publisher)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="4. Publisher Effect Model",
                                 RMSE = model_4_rmse ))

# effect 5: just developer effect
rmses_developer <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Developer") 
  
  developer_avgs <- train_set %>% 
    group_by(Developer) %>% 
    summarize(b_d = mean(Global_Sales - mu))
  
  # predict rating for each entry: add mu and b_d
  predicted_sales_d <- mu + test_set %>%        
    left_join(developer_avgs, by='Developer') %>% 
    .$b_d
  
  rmses_developer[i] <- RMSE(predicted_sales_d, test_set$Global_Sales)
}


model_5_rmse <- mean(rmses_developer)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="5. Developer Effect Model",
                                 RMSE = model_5_rmse ))


# effect 6: just Rating effect
rmses_Rating <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Rating") 
  
  Rating_avgs <- train_set %>% 
    group_by(Rating) %>% 
    summarize(b_r = mean(Global_Sales - mu))
  
  # predict Rating for each entry: add mu and b_r
  predicted_sales_r <- mu + test_set %>%        
    left_join(Rating_avgs, by='Rating') %>% 
    .$b_r
  
  rmses_Rating[i] <- RMSE(predicted_sales_r, test_set$Global_Sales)
}

model_6_rmse <- mean(rmses_Rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="6. Rating Effects Model",  
                                 RMSE = model_6_rmse ))


# effect 7: stack effects
rmses <- matrix(NA,nrow=1,ncol=10)
for( i in seq(1:10)){
  train_set <- video_games[-test_index[,i],] # 90% of data
  test_set  <- video_games[test_index[,i],]
  test_set <- test_set %>% 
    semi_join(train_set, by = "Name") %>%
    semi_join(train_set, by = "Platform") %>%
    semi_join(train_set, by = "Genre") %>%
    semi_join(train_set, by = "Publisher") %>% 
    semi_join(train_set, by = "Developer")%>% 
    semi_join(train_set, by = "Rating")
  
  Name_avgs <- train_set %>% 
    group_by(Name) %>% 
    summarize(b_n = mean(Global_Sales - mu))

  Platform_avgs <- train_set %>% 
    left_join(Name_avgs, by='Name') %>%
    group_by(Platform) %>% 
    summarize(b_pl = mean(Global_Sales - mu - b_n))

  Genre_avgs <- train_set %>%
    left_join(Name_avgs, by='Name') %>%
    left_join(Platform_avgs, by='Platform') %>%
    group_by(Genre) %>%
    summarize(b_g = mean(Global_Sales - mu - b_n - b_pl))

  Publisher_avgs <- train_set %>%
    left_join(Name_avgs, by='Name') %>%
    left_join(Platform_avgs, by='Platform') %>%
    left_join(Genre_avgs, by='Genre') %>%
    group_by(Publisher) %>%
    summarize(b_pu = mean(Global_Sales - mu - b_n - b_pl - b_g))

  Developer_avgs <- train_set %>%
    left_join(Name_avgs, by='Name') %>%
    left_join(Platform_avgs, by='Platform') %>%
    left_join(Genre_avgs, by='Genre') %>%
    left_join(Publisher_avgs, by='Publisher') %>%
    group_by(Developer) %>%
    summarize(b_d = mean(Global_Sales - mu - b_n - b_pl - b_g - b_pu))
  
  Rating_avgs <- train_set %>%
    left_join(Name_avgs, by='Name') %>%
    left_join(Platform_avgs, by='Platform') %>%
    left_join(Genre_avgs, by='Genre') %>%
    left_join(Publisher_avgs, by='Publisher') %>%
    left_join(Developer_avgs, by='Developer') %>%
    group_by(Rating) %>%
    summarize(b_r = mean(Global_Sales - mu - b_n - b_pl - b_g - b_pu - b_d))

  # predict rating for each entry: add mu and bs
  predicted_sales <- test_set %>%  
    left_join(Name_avgs, by='Name') %>%      
    left_join(Platform_avgs, by='Platform') %>%
    left_join(Genre_avgs, by='Genre') %>%
    left_join(Publisher_avgs, by='Publisher') %>%
    left_join(Developer_avgs, by="Developer") %>%
    left_join(Rating_avgs, by="Rating") %>%
    mutate(pred = mu + b_n + b_pl + b_g + b_pu + b_d + b_r) %>%
    .$pred
  
  rmses[i] <- RMSE(predicted_sales, test_set$Global_Sales)
}

model_7_rmse <- mean(rmses)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="7. Combined Effects Model",  
                                 RMSE = model_7_rmse ))

##

# effect 9: regularization effect on movie averages
# find ideal regularization factor
lambdas <- seq(0, 10, 0.25)

# calculate the sum and count of sales to build to the regularized average
just_the_sum_9 <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_m = n())
# now calculate rmse as a function of regularization factor, lambda
rmse_lambda_9 <- sapply(lambdas, function(lam){
  predicted_sales <- test_set %>% 
    left_join(just_the_sum_9, by='movieId') %>% 
    mutate(b_m = s/(n_m+lam)) %>%
    mutate(pred = mu + b_m) %>%
    .$pred
  return(RMSE(predicted_sales, test_set$rating))
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
  predicted_sales <- test_set %>% 
    left_join(just_the_sum_10, by='userId') %>% 
    mutate(b_ur = s/(n_u+lam)) %>%
    mutate(pred = mu + b_ur) %>%
    .$pred
  return(RMSE(predicted_sales, test_set$rating))
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
  predicted_sales <- test_set %>% 
    left_join(just_the_sum_11, by='genres') %>% 
    mutate(b_gr = s/(n_g+lam)) %>%
    mutate(pred = mu + b_gr) %>%
    .$pred
  return(RMSE(predicted_sales, test_set$rating))
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
  predicted_sales <- 
    test_set_reduced %>% 
    left_join(b_mr, by = "movieId") %>%
    left_join(b_ur, by = "userId") %>%
    left_join(b_gr, by = "genres") %>%
    mutate(pred = mu + b_mr + b_ur + b_gr) %>%
    .$pred
  return(RMSE(predicted_sales, test_set_reduced$rating))
})

qplot(lambdas, rmse_lambda)  

lambda_all <- lambdas[which.min(rmse_lambda)]
lambda_all # 4.25 

# predict sales with lambda_all
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
predicted_sales <- 
  test_set %>% 
  left_join(b_mr, by = "movieId") %>%
  left_join(b_ur, by = "userId") %>%
  left_join(b_gr, by = "genres") %>%
  mutate(pred = mu + b_mr + b_ur + b_gr) %>%
  .$pred

model_12_rmse <- RMSE(predicted_sales, test_set$rating)

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
predicted_sales <- 
  test_set %>% 
  left_join(b_mr, by = "movieId") %>%
  left_join(b_ur, by = "userId") %>%
  left_join(b_gr, by = "genres") %>%
  mutate(pred = mu + b_mr + b_ur + b_gr) %>%
  .$pred

model_13_rmse <- RMSE(predicted_sales, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="13. All Effects Regularization Model--Multiple lambdas",  
                                 RMSE = model_13_rmse ))
rmse_results