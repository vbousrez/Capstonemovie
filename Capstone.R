# Code supplied by edX to generate the tables
library(tidyverse)
library(caret)
library(dslabs)

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 600)
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)
file.exists(dl)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)

colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

ratings
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))
movies

movielens <- left_join(ratings, movies, by = "movieId")
movielens

dim(movielens) 
#[1] 10000054        6
#10000054 cells number non null and 6 columns       

length(unique(movielens$userId))
length(unique(movielens$movieId))

# Convert string character into string of integer for faster processing
movielens$genres = as.numeric(as.factor(edx$genres))
head(movielens)

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
edx
temp <- movielens[test_index,]
temp

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
final_holdout_test

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
removed
edx <- rbind(edx, removed)
edx

rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx

#edx2 <- edx%>%
#  mutate(
#    Title = str_extract(title, ".*?(?= \\(\\d{4}\\))"),
#    Date = str_extract(title, "\\(\\d{4}\\)")
#  )

#edx2$Date

# Remove parentheses from the 'Date' column
#edx2$Date <- gsub("\\(|\\)", "", edx2$Date)

#names(edx2)
#edx2

#edx2$rating
#muedx2 <- mean(edx$rating)
#muedx2

#rmsemuedx2 <- mean(final_holdout_test$rating, muedx2)
#rmsemuedx2

#prediction <- rep(2.5, nrow(test_set))
#RMSE(final_holdout_test$rating, prediction)

#First Model
edx$rating

muedx <- mean(edx$rating)
muedx
?rmse

rmsemuedx <- RMSE(final_holdout_test$rating, muedx)
rmsemuedx

#prediction <- rep(2.5, nrow(test_set))
#RMSE(final_holdout_test$rating, prediction)

#modeling movie effect
  #Compute least square to estimate using Yu,i-u_hat
names(edx)
movie_avgs <- edx%>% 
  group_by(movieId)%>%
  summarize(b_i= mean(rating - muedx))
movie_avgs

  #Assessing improvement in prediction
predicted_ratings <- muedx + 
  final_holdout_test %>%
  left_join(movie_avgs, by= 'movieId') %>% 
  pull(b_i)

predicted_ratings
model_1_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
model_1_rmse

#Modeling user effect
names(edx)

  #plotting the average rating for those above 100 ratings
edx%>%
  group_by(userId)%>%
  summarize(b_u= mean(rating))%>%
  filter(n()>=100)%>%
  ggplot(aes(b_u))+
  geom_histogram(bins=30, color = "black")

movie_avgs$b_i
mu

  #Compute approximation of least square using average Yu,i_hat-u_hat-bi_hat
user_avgs <- edx%>%
  left_join(movie_avgs, by= 'movieId') %>% 
  group_by(userId) %>%
  summarize(b_u= mean(rating- muedx - b_i))
user_avgs

    #construct predictors
predicted_ratings<-final_holdout_test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred= muedx + b_i+ b_u) %>%
  pull(pred)
  #measures improvement
model_2_rmse <- RMSE(predicted_ratings,final_holdout_test$rating)
model_2_rmse

#Assessing opportunity for using time as predictor
  #adding timestamp
edx3 <- mutate(edx, date= as_datetime(timestamp))
edx3

edx3 %>%mutate(date= round_date(date, unit = "week")) %>%
  group_by(date)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(date, rating))+
  geom_point()+
  geom_smooth()
  #correlation is there but not so strong

#genre effect
names(edx)
  #Assessing opportunity for inclusion of genre
edx %>% group_by(genres) %>%
  summarize(n=n(), avg= mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >=1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x= genres, y=avg, ymin= avg - 2*se, ymax=avg+2*se))+
  geom_point()+
  geom_point()+
  geom_errorbar()#+
  #theme(axis.text.x = element_text(angle =90, hjust=1))
  #Strong evidence of genre effect observed

  #to check from other lessons on genre
# https://github.com/AlessandroCorradini/Harvard-Data-Science-Professional/blob/master/09%20-%20PH125.9x%20-%20Capstone/MovieLens%20Recommender%20System%20Project/MovieLens%20Project.R line   345

  #Compute rating per genre
names(edx)
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - muedx - b_i - b_u))

muedx
movie_avgs
user_avgs
genre_avgs

  #Build prediction
predicted_ratings <- final_holdout_test %>%
  left_join(movie_avgs, by= 'movieId') %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = muedx + b_i + b_u + b_g) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings,final_holdout_test$rating)
model_3_rmse

model_1_rmse
model_2_rmse
model_3_rmse

# Model 4 Setting of lambda via cross validation
# Compute the predicted ratings on validation dataset using different values of lambda

#<- sapply(lambdas, function(lambda)

movie_avgs_lambda_ = NULL
user_avgs_lamda_   = NULL
genre_avgs_lamda_  = NULL

do_one_rmse_lambda <- function(lambda, edx_train, edx_test)  {
  # Calculate the average by movie
  movie_avgs_lambda <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - muedx) / (n() + lambda))
  #movie_avgs_lambda_ <<- movie_avgs_lambda
  
  # Calculate the average by user
  user_avgs_lamda <- edx_train %>%
    left_join(movie_avgs_lambda, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - muedx) / (n() + lambda))
  #user_avgs_lamda_ <<- user_avgs_lamda
  
  # Calculate the average by genre
  genre_avgs_lamda <- edx_train %>%
    left_join(movie_avgs_lambda, by='movieId') %>%
    left_join(user_avgs_lamda, by='userId') %>%
    group_by(genres) %>%
    summarize(b_u_g = sum(rating - b_i - muedx - b_u) / (n() + lambda))
  #genre_avgs_lamda_ <<- genre_avgs_lamda
  
  # filter rows where effects are in train but not in test
  fg = edx_test$genres %in% unique(edx_train$genres)
  fu = edx_test$userId %in% unique(edx_train$userId)
  fm = edx_test$movieId %in% unique(edx_train$movieId)
  edx_test = edx_test[fg & fu & fm,]
  
  # Compute the predicted ratings on test dataset
  predicted_ratings <- edx_test %>%
    left_join(movie_avgs_lambda, by='movieId') %>%
    left_join(user_avgs_lamda, by='userId') %>%
    left_join(genre_avgs_lamda, by='genres') %>%
    mutate(pred = muedx + b_i + b_u + b_u_g) %>%
    pull(pred)
  
  # Predict the RMSE on the validation set
  rmse = RMSE(edx_test$rating, predicted_ratings)
  
  rmse
}

# better:
# with cross-validation for setting best lambda
# then after finding lambda from only sample edx
# ... apply to the final validation sample
# ... (final_holdout_test at the end only)
lambdas <- seq(0, 8, 0.25)
lambdas
rmses   <- 0 * lambdas
rmses

# with cross validation
#first find the k folds from edx

idx = as.data.frame(1:nrow(edx))
colnames(idx) = NULL

K = 10

set.seed(543210)
folds <- sample( cut(seq(1,nrow(edx)),breaks=K,labels=FALSE) )

table(folds)
sum(table(folds))
head(folds)
tail(folds)

# movie_avgs_lambda_ = NULL
# user_avgs_lamda_   = NULL
# genre_avgs_lamda_  = NULL

#then apply compute rmse for each fold removed, and average
for (l in 1:length(lambdas)) {
  print(l)
  lambda = lambdas[l]
  remse_l = 0
  
  for (k in 1:K) {
    # print(k)
    cat(".")
    edx_train_k = edx[folds!=k,]
    edx_test_k = edx[folds==k,]
    # print(dim(edx_train_k))
    # print(dim(edx_test_k))
    remse_lk = do_one_rmse_lambda (lambda=lambda,
                                   edx_train=edx_train_k,
                                   edx_test=edx_test_k)
    
    remse_l = remse_l + remse_lk / K
    
    
    rm(edx_train_k)
    rm(edx_test_k)
  }
  
  rmses[l] = remse_l
  print(paste( lambda,  remse_l, sep= " "))
}

rmses_withcv = rmses


# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]
min_lambda

min_lambda

library(ggplot2)
datap <- data.frame(lambdas=lambdas,
                    rmses_crossval=rmses_withcv)
ggplot(datap, aes(lambdas, rmses_crossval)) +
  ggtitle("RMSE WITH CV")+
  geom_line()+
  geom_vline(xintercept = min_lambda)


ggplot(datap, aes(lambdas)) +
  geom_line(aes(y = rmses_withcv, colour = "cv")) +
  labs(color="method") + ylab("Rmse") + xlab("Lambdas")

rmse_regularized_movie_user_genre_model =
  remse_lk = do_one_rmse_lambda (lambda=min_lambda,
                                 edx_train=edx,
                                 edx_test=final_holdout_test)
rmse_regularized_movie_user_genre_model


# # Implementation of regularization
# lambdas <- seq(0, 15, 0.1)
# 
# # Compute the predicted ratings on validation dataset using different values of lambda
# 
# rmses <- sapply(lambdas, function(lambda) {
#   
#   # Calculate the average by movie
#   
#   movie_avgs_lambda <- edx %>%
#     group_by(movieId) %>%
#     summarize(b_i = sum(rating - muedx) / (n() + lambda))
#   
#   # Calculate the average by user
#   
#   user_avgs_lamda <- edx %>%
#     left_join(movie_avgs_lambda, by='movieId') %>%
#     group_by(userId) %>%
#     summarize(b_u = sum(rating - b_i - muedx) / (n() + lambda))
#   
#   # Calculate the average by genre
#   
#     genre_avgs_lamda <- edx %>%
#     left_join(movie_avgs_lambda, by='movieId') %>%
#     left_join(user_avgs_lamda, by='userId') %>%
#     group_by(genres) %>%
#     summarize(b_u_g = sum(rating - b_i - muedx - b_u) / (n() + lambda))
#   
#   # Compute the predicted ratings on validation dataset
#   predicted_ratings <- final_holdout_test %>%
#     left_join(movie_avgs_lambda, by='movieId') %>%
#     left_join(user_avgs_lamda, by='userId') %>%
#     left_join(genre_avgs_lamda, by='genres') %>%
#     mutate(pred = muedx + b_i + b_u + b_u_g) %>%
#     pull(pred)
#   
#   # Predict the RMSE on the validation set
#   
#   RMSE(final_holdout_test$rating, predicted_ratings)
#   })

# Get the lambda value that minimize the RMSE

#min_lambda <- lambdas[which.min(rmses)]
#min_lambda
# Predict the RMSE on the validation set

#rmse_regularized_movie_user_genre_model <- min(rmses)
#rmse_regularized_movie_user_genre_model

# Adding the results to the results dataset

#results <- results %>% add_row(model="Regularized Movie+User+Genre Based Model", RMSE=rmse_regularized_movie_user_genre_model)




