corr <- function(directory = "specdata", threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  
  # Setup variables
  fileIDs <- new("integer")
  filenames <- new("character")
  filename <- new("character")
  data <- data.frame()
  result <- new("numeric")
  

  # Get an integer vector of the monitors to process.
  fileIDs <- subset.data.frame(complete(directory), nobs > threshold, select = "id")[["id"]]
  
  # Loop through the id's to create a vector of the filenames.
  for(i in fileIDs) {
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
  
  # Process each file.
  for(i in filenames) {
    data <- read.csv(file.path(directory, i))
    result <- c(result, cor(data[["sulfate"]], data[["nitrate"]], use = "complete.obs"))
  }
  
  return(result)
  
}