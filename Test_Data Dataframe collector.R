#Requires .txt file input format where artist is named next to song name with 
#":" being used to separate them.

library(stringr)

TData <- read.table("test_albums/test_albums_2.txt")

Test_Data <- matrix(ncol = 2)
for (i in (1:dim(TData))) {
  Test_Data <-
    rbind(Test_Data, c(word(TData[i, 1], 1:2, sep = fixed(':'))))
} #Word selects all phrases between the commas

Test_Data <- Test_Data[-1, ]
Test_Data <- as.data.frame(Test_Data)
colnames(Test_Data) <- c("Artist", "TrackName")
View(Test_Data)
