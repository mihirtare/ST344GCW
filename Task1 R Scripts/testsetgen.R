#Testing data set generator.
testsetgen <- function(){
  intrain <- createDataPartition(y = albumdat$AlbumPopularityRating, 
                                 p= 0.05, list = FALSE)
  return(albumdat[intrain,])
}