#Training data set generator.
trainsetgen <- function(data, year){
  if(year <= 1964){
    return(filter(data, (ReleaseYear <= 1964)))
  }
  else if(year > 1964 & year <= 1975){
    return(filter(data, (ReleaseYear > 1964), (ReleaseYear <= 1975)))
  }
  else if(year > 1975 & year <= 1992){
    return(filter(data, (ReleaseYear > 1975), (ReleaseYear <= 1992)))
  }
  else if(year > 1992 & year <= 2002){
    return(filter(data, (ReleaseYear > 1992), (ReleaseYear <= 2002)))
  }
  else if(year > 2002){
    return(filter(data, (ReleaseYear > 2002)))
  }
}