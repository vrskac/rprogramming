best <- function(state, outcome) {
  
  d <- read.csv("outcome-of-care-measures.csv", header = TRUE)
  
  # Get a vector of unique states in the data and check the parameter.
  states <- unique(as.vector(d$State))
  if(state %in% states == FALSE) {
    stop("invalid state")
  }
  
  # Create a vector of the outcomes and check the parameter.
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes == FALSE) {
    stop("invalid outcome")
  }
  
  column <- new("integer")
  
  # Set a column variable for the outcome to be studied.
  if(outcome == "heart attack") {
    column <- 11
  }
  else {
    if(outcome == "heart failure") {
      column <- 17
    }
    else {
      # "pneumonia"
      column <- 23
    }
  }
  
  # Subset the data based on the state and outcome to study.
  dsub <- subset.data.frame(d, d$State == state, select = c(2, column))
  
  # Get the column name for the outcome column.
  colName <- names(dsub)[2]
  
  # Remove string "Not Available" from the observations.
  dsub <- subset.data.frame(dsub, dsub[colName] != "Not Available")
  
  # Get the minimum value in the outcome column.
  v <- as.vector(dsub[,2])
  v <- na.omit(v)
  pos <- which.min(v)
  outcomeMin <- v[pos]
  
  # Subset the data based on the outcome minimum and return only the names.
  dsub <- subset.data.frame(dsub, dsub[colName] == outcomeMin)
  
  # Get the names into a vector.
  names <- as.vector(dsub[,1])
  
  # Sort and return the first value.
  return(sort(names)[1])
  
}
