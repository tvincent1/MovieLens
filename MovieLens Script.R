#MovieLens Project for edx Data Science 125x.9
#Tasha Vincent
#January 2019

#****************** Prepare the data per instructions provided ************************

# Create edx set, validation set

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#*** added this section to identify single-rating movies so we keep them out of validation set
reviews <- movielens %>%
  group_by(movieId) %>%
  summarize(count = n()) 

singles <- reviews %>%
  filter(count == 1) 

# Validation (or test_set) set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set, and that no films with just 1 rating are included

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>%
#** added further check to remove single-rating movies
  anti_join(singles, by = "movieId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


##********* Explore the data ****************
#Explore the data’s contents and dimensions to generate hypothetical effects on predicted ratings. 

library(tidyverse)
head(edx)
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#create a visualization of the count of ratings per movie to see if there is a pattern.

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")


#  Look at the number of ratings per user.

edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")


# *********** Evaluating Results ************
#create a function to evaluate whether the predictions generated are close to the actual ratings for a given movie and user.

RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2))
}


# ***************** Models **********************

# Basic model predicts that any movie will get the average rating.

mu <- mean(edx$rating)
mu


# check results using RMSE function

avg_only <- RMSE(validation$rating, mu)
avg_only


# **** Store results in a data frame *******

rmse_results <- data_frame(Model = "Simple average", RMSE = avg_only)


# ****** Movie quality effect on predicted rating ******************

# see whether the movie itself is a factor, visualize the number of ratings within each band.

movie_avgs <- edx %>% 
group_by(movieId) %>% 
summarize(m_avg = mean(rating))
movie_avgs %>% qplot(m_avg, geom ="histogram", bins = 10, data = ., color = I("black"))

# create a term for movie quality, m_q as the difference in average rating for a given movie from the average rating of all movies, mu.

mu <- mean(edx$rating) 
mq_avgs <- edx %>% 
group_by(movieId) %>% 
summarize(m_q = mean(rating - mu))

# visualize the distribution of the movie quality term 
mq_avgs %>% qplot(m_q, geom ="histogram", bins = 10, data = ., color = I("black"))


# add m_q term to model and evaluate using test set
predictions <- mu + validation %>% 
left_join(mq_avgs, by='movieId') %>%
.$m_q

movie_rmse <- RMSE(predictions, validation$rating)


#add to results data frame
rmse_results <- bind_rows(rmse_results,
data_frame(Model="Movie Effect Model",  
RMSE = movie_rmse ))
rmse_results %>% knitr::kable()



# ********** Users' bias term  ***********
 #Visualize how much a given user varies from the average rating for all users.

edx %>% 
group_by(userId) %>% 
summarize(u_b = mean(rating)) %>% 
#  filter(n()>=5) %>%
ggplot(aes(u_b)) + 
geom_histogram(bins = 30, color = "black")


 ## user bias per movie (u_b) term is the difference between the user's average rating and the overall average and the movie-specific average.

user_avgs <- edx %>% 
  left_join(q_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(u_b = mean(rating - mu - m_q))

 # add the user term to the model and evaluate   
predictions = mu + m_q + u_b

predicted_ratings <- validation %>% 
  left_join(mq_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + m_q + u_b) %>%
  .$pred

users_rmse <- RMSE(predicted_ratings, validation$rating)


# add results to data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User Effects Model",  
                                     RMSE = users_rmse ))
rmse_results %>% knitr::kable()


#  ********** Genre Term ********************
# create a list of ratings per genre for each movie.


library(tidyr)
edx_genre <- separate_rows(edx, genres, sep = "\\|") 

#Confirm that data is as expected
edx_genre %>%
  group_by(genres) %>%
  summarize(n())

#plot mean ratings per genre to test the hypothesis that there is a genre effect.
library(dplyr)
edx_genre %>%  
  group_by(genres) %>% 
  summarize(genre_avg = mean(rating)) %>%
  ggplot(aes(genre_avg, genres)) + 
  geom_point() +
  geom_text(aes(label=genres))


# define genre bias as the difference between the overall mean and the genre-specific mean.
genre_avgs <- edx_genre %>% 
  left_join(mq_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(g_b = mean(rating - mu - m_q - u_b)) 


# *******evaluate results with new term
#first, create single-genre ratings in validation set
validation_genre <- separate_rows(validation, genres, sep="\\|") 

#then evaluate the model

pg_ratings <- validation_genre %>% 
  left_join(mq_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + m_q + u_b + g_b) %>%
  .$pred


genre_rmse <- RMSE(pg_ratings, validation_genre$rating)

# add results to data frame
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie + User + Genre Effects Model",  
                                     RMSE = genre_rmse ))
rmse_results %>% knitr::kable()