
library(recommenderlab)

source('https://raw.githubusercontent.com/ChicagoBoothML/MachineLearning_Fall2015/master/Programming%20Scripts/MovieLens%20Movie%20Recommendation/R/ParseData.R')

data <- parse_movielens_1m_data()
movies <- data$movies
users <- data$users
ratings <- data$ratings[ , .(user_id, movie_id, rating)]
ratings[ , `:=`(user_id = factor(user_id),
                movie_id = factor(movie_id))]
nb_ratings_per_user <-
  dcast(ratings, user_id ~ ., fun.aggregate=length, value.var='rating')

nb_ratings_per_movie <-
  dcast(ratings, movie_id ~ ., fun.aggregate=length, value.var='rating')
ratings <- as(ratings, 'realRatingMatrix')

ratings

train_proportion <- .5
nb_of_given_ratings_per_test_user <- 10

evaluation_scheme <- evaluationScheme(
  ratings, 
  method='split',
  train=train_proportion,
  k=1,
  given=nb_of_given_ratings_per_test_user)

evaluation_scheme

ratings_train <- getData(evaluation_scheme, 'train')

ratings_train

ratings_test_known <- getData(evaluation_scheme, 'known')

ratings_test_known

ratings_test_unknown <- getData(evaluation_scheme, 'unknown')

ratings_test_unknown
recommenderRegistry$get_entry('POPULAR', dataType='realRatingMatrix')
#popular
popular_rec <- Recommender(
  data=ratings_train,
  method='POPULAR')

popular_rec

popular_rec_pred <- predict(
  popular_rec,
  ratings_test_known,
  type='ratings')

popular_rec_pred_acc <- calcPredictionAccuracy(
  popular_rec_pred,
  ratings_test_unknown)

popular_rec_pred_acc

#item-based
item_based_cofi_rec <- Recommender(
  data=ratings_train,
  method='IBCF',           
  parameter=list(
    normalize='center',    
    
    method='Pearson',      
    k=100                  
  ))

item_based_cofi_rec

item_based_cofi_rec_pred <- predict(
  item_based_cofi_rec,
  ratings_test_known,
  type='ratings')

item_based_cofi_rec_pred_acc <- calcPredictionAccuracy(
  item_based_cofi_rec_pred,
  ratings_test_unknown)

item_based_cofi_rec_pred_acc

#user-based

user_based_cofi_rec <- Recommender(
  data=ratings_train,
  method='UBCF',           
  parameter=list(
    normalize='center',    
    method='Pearson',      
    nn=30                 
  ))

user_based_cofi_rec

user_based_cofi_rec_pred <- predict(
  user_based_cofi_rec,
  ratings_test_known,
  type='ratings')

user_based_cofi_rec_pred_acc <- calcPredictionAccuracy(
  user_based_cofi_rec_pred,
  ratings_test_unknown)

user_based_cofi_rec_pred_acc
