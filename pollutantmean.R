pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
  
  ## "pollutant" parameter will be either "sulfate" or "nitrate".
  
  # Setup variables
  filenames <- new("character")
  filename <- new("character")
  data <- data.frame()
  dataSubset <- data.frame()
  
  # Loop through the id's to create a vector of the filenames.
  for(i in id) {
    if(i < 10) {
      filename <- paste("00", i, ".csv", sep = "")
    } 
    else {
      if(i < 100) {
        filename <- paste("0", i, ".csv", sep = "")
      } 
      else {
        filename <- paste(i, ".csv", sep = "")
      }
    }
    filenames <- c(filenames, filename)
  }
  
  # Loop through reading the files into a data.frame.
  for(i in filenames) {
    data <- rbind(data, read.csv(file.path(directory, i)))
  }
  
  # Subset the data.frame for the required column, excluding NA values.
  if(pollutant == "sulfate") {
    dataSubset <- subset.data.frame(data, !is.na(sulfate), select = pollutant)
  }
  else {
    dataSubset <- subset.data.frame(data, !is.na(nitrate), select = pollutant)
  }
  
  # Return the mean of the subset's only column.
  return(mean(dataSubset[[1]]))
  
}
