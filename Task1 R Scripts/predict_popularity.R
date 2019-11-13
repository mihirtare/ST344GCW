predict_popularity <- function(filename){
  test_data <- fetch_test(filename)
  
  test_data <- test_data %>% group_by(Artist, AlbumName) %>%
    mutate(PredictedPopularity = run_knn(Artist, AlbumName)) %>%
    mutate(AbsolutePercentageError = 
             (abs(PredictedPopularity - AlbumPopularityRating)/
                AlbumPopularityRating)*100)
  
  print("The Mean Absolute % Error is: ")
  View(test_data)
  return(sum(test_data$AbsolutePercentageError)/length(test_data))
}