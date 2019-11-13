# Data Import
albumdatgen <- function(){
  maindat <- read_excel("edited_spotify.xlsx")
  
  #NA Removal
  maindat <- na.omit({ 
    maindat %>%
      group_by(Artist, AlbumName, AlbumReleaseDate, ArtistPopularity)
  })
  
  #Date Parsing
  maindat$AlbumReleaseDate = as.Date(parse_date_time(
    maindat$AlbumReleaseDate, orders = c("y", "ym", "ymd")))
  
  #Grouping data by albums because there is no data to calculate an individual       track's popularity.
  albumdat = {maindat %>% 
      group_by(Artist,ArtistPopularity,AlbumName,
               AlbumReleaseDate,AlbumPopularity,ArtistNumFollowers,
               AlbumWeeksOnChart,AlbumWeeksNumberOne,
               AlbumBestChartPosition) %>% 
      summarise(AlbumDuration = mean(TrackDuration),
                AlbumDanceability = mean(TrackDanceability),
                AlbumEnergy = mean(TrackEnergy),
                AlbumLoudness = mean(TrackLoudness),
                AlbumSpeechiness = mean(TrackSpeechiness),
                AlbumAcousticness = mean(TrackAcousticness),
                AlbumInstrumentalness = mean(TrackInstrumentalness),
                AlbumLiveness = mean(TrackLiveness),
                AlbumValence = mean(TrackValence),
                AlbumTempo = mean(TrackTempo)
      )}
  
  #Calculating the AlbumPopularityRating and ReleaseYear for each album.
  albumdat <- {albumdat %>% 
      group_by(Artist, AlbumName, AlbumReleaseDate) %>%
      mutate(AlbumPopularityRating = 
               ((ArtistPopularity + AlbumPopularity)/2) +
               floor((ArtistNumFollowers/10^6)) +
               (AlbumWeeksOnChart/100) +
               (AlbumWeeksNumberOne/10) +
               (1 - (AlbumBestChartPosition/100)),
             ReleaseYear = year(AlbumReleaseDate))
  }
  return(albumdat)
}
