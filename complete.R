complete <- function(directory = "specdata", id = 1:332) {

  # Setup variables
  filenames <- new("character")
  filename <- new("character")
  data <- data.frame()
  results <- data.frame()
  
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
  
  # Loop through and process each file, building up a new results data.frame.
  for(i in filenames) {
    data <- read.csv(file.path(directory, i))
    data <- data.frame(data[1, "ID"], sum(complete.cases(data)))
    names(data) <- c("id", "nobs")
    results <- rbind.data.frame(results, data)
  }
  
  # Return the completed results data.frame.
  return(results)
  
}