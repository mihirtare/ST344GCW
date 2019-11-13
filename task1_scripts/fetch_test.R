fetch_test <- function(filename){
  # e.g. filename = test_albums_2.txt
  source <- paste("test_albums",filename, sep = "/")
  TData <- read.table(source)
  
  test_data <- matrix(ncol = 2)
  
  #Word selects all phrases between the commas
  for (i in (1:dim(TData))) {
    test_data <-
      rbind(test_data, c(word(TData[i, 1], 1:2, sep = fixed(': '))))
  }
  
  test_data <- test_data[-1, ]
  test_data <- as_tibble(test_data)
  colnames(test_data) <- c("Artist", "AlbumName")
  
  test_data <- filter(albumdat, (Artist %in% as.list(test_data$Artist)) &
                        (AlbumName %in% as.list(test_data$AlbumName)))
  
  return(test_data)
}