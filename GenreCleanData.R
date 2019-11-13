##Task 1 Data Clean
library(readxl)
library(dplyr)
library(lubridate)
library(rio)
library(tidyr)
library(stringr)


#IMPORT THE DATA
Spotify_Data <- import("edited_spotify.xlsx", setclass = "tibble")




#CREATING TASK1 DATA
Task1_Data <- na.omit({
  Spotify_Data %>%
    group_by(Artist, AlbumName, AlbumReleaseDate, ArtistPopularity)
})
Task1_Data$AlbumReleaseDate = as.Date(parse_date_time(Task1_Data$AlbumReleaseDate, orders = c("y", "ym", "ymd")))





#CREATING TASK2 DATA


##Removing NA's and grouping
Data2 <- na.omit({
  Spotify_Data %>%
    group_by(Artist, AlbumName, AlbumReleaseDate, ArtistPopularity)
})

##Sorting Date into one format. Here this is the year!
Data2$AlbumReleaseDate = year(as.Date(parse_date_time(
  Task1_Data$AlbumReleaseDate, orders = c("y", "ym", "ymd")
)))

##Creating a matrix from the Genres
Genres <- matrix()
Genres <- matrix(ncol = 17)

##Using a for statement to find each individual Artist Genre
for (i in (1:dim(Data2))) {
  Genres <-
    rbind(Genres, c(word(Data2$ArtistGenres[i], 1:17, sep = fixed(','))))
} #Word selects all phrases between the commas

##Removing 1st line from matrix formation and adding TrackID column
Genres <- Genres[-1, ]
Genres <- cbind(Data2$TrackID, Genres)

##Making matrix into a dataframe, and naming column names
Task2_Data <- as.data.frame(Genres)
colnames(Task2_Data) <-
  c(
    "TrackID",
    "Genre1",
    "Genre2",
    "Genre3",
    "Genre4",
    "Genre5",
    "Genre6",
    "Genre7",
    "Genre8",
    "Genre9",
    "Genre10",
    "Genre11",
    "Genre12",
    "Genre13",
    "Genre14",
    "Genre15",
    "Genre16",
    "Genre17"
  )

##Merging Genre Data with Main Data to make out Dataframe
Task2_Data <- merge(Data2, Task2_Data, by = ("TrackID"))
View(Task2_Data)
