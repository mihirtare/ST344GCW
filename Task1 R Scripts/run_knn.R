run_knn <- function(artistname, album){
  entry <- filter(albumdat, (Artist == artistname)&(AlbumName == album))
  year <- entry$ReleaseYear
  training_set <- trainsetgen(albumdat, year)
  
  #To control the nuances of our model training function, we set the resampling      method to repeated cross-validation with 10 resampling iterations and 3          complete sets of folds to compute for our repeated cross-validation
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  
  pred1 <- train(AlbumPopularityRating ~ AlbumDanceability + 
                   AlbumEnergy + AlbumLiveness + 
                   AlbumValence, data = training_set, 
                 method = "knn", trControl=trctrl, 
                 preProcess = c("center", "scale"), tuneLength = 10)
  
  pred2 <- train(AlbumPopularityRating ~  AlbumAcousticness +
                   AlbumInstrumentalness, data = training_set, 
                 method = "knn", trControl=trctrl, 
                 preProcess = c("center", "scale"), tuneLength = 10)
  
  pred3 <- train(AlbumPopularityRating ~ AlbumSpeechiness + 
                   AlbumDuration, data = training_set, 
                 method = "knn", trControl=trctrl, 
                 preProcess = c("center", "scale"), tuneLength = 10)
  
  poppred1 <-  predict(pred1, newdata = entry)
  poppred2 <-  predict(pred2, newdata = entry)
  poppred3 <-  predict(pred3, newdata = entry)
  
  poppred <- mean(poppred1, poppred2, poppred3)
  entry <- {entry %>% group_by(Artist, AlbumName) %>%
      mutate(PopularityPrediction = poppred)}
  
  return(entry$PopularityPrediction)
}