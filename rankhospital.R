rankhospital <- function(state, outcome, num = "best") {
  
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

  # Set the column names.
  colnames(dsub) <- c("hospital.name", "rate")  
  
  # Remove observations with NA and "Not Available" from rate column.
  dsub <- subset.data.frame(dsub, dsub$rate != "Not Available")
  dsub <- dsub[complete.cases(dsub),]
  
  # Now the sample is clean, we can set the column data types.
  dsub$hospital.name <- as.character(as.vector(dsub$hospital.name))
  dsub$rate <- as.numeric(as.vector(dsub$rate))
  
  # Order by rate then name.
  dsub <- dsub[order(dsub$rate, dsub$hospital.name),]

  # Add a new column with the rank.
  dsub$rank <- c(1:nrow(dsub))
  
  # Process num to identify the rank to find.
  targetRank <- new("integer")
  if(num == "best") {
    targetRank <- 1
  }
  else {
    if(num == "worst") {
      targetRank <- nrow(dsub)
    }
    else {
      if(num %in% dsub$rank) {
        targetRank <- num
      }
      else {
        return(NA)
      }
    }
  }

  # Return the result.
  return(dsub[dsub$rank == targetRank, "hospital.name"])
  
}