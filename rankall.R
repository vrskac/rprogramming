rankall <- function(outcome, num = "best") {
  
  # Create a vector of the outcomes and check the parameter.
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes == FALSE) {
    stop("invalid outcome")
  }
  
  # Setup variable to identify the outcome column.
  column.name <- new("character")
  if(outcome == "heart attack") {
    column.number <- 11
  }
  else {
    if(outcome == "heart failure") {
      column.number <- 17
    }
    else {
      # "pneumonia"
      column.number <- 23
    }
  }

  # Get the data.
  d <- read.csv("outcome-of-care-measures.csv", header = TRUE)
  
  # Get a vector of unique states in the data.
  states <- unique(as.character(as.vector(d$State)))
  
  # Column name setup.
  colnames(d)[2] <- "name"
  colnames(d)[7] <- "state"
  colnames(d)[column.number] <- "rate"

  # Subset for the required columns only.
  d <- d[, c("name", "state", "rate")]
  
  # Remove observations with NA and "Not Available" from rate column.
  d <- subset.data.frame(d, d$rate != "Not Available")
  d <- d[complete.cases(d),]
  
  # Column types setup.
  d$name <- as.character(as.vector(d$name))
  d$state <- as.character(as.vector(d$state))
  d$rate <- as.numeric(as.vector(d$rate))
  
  # Create a DF for the results.
  resultsDF <- data.frame()
  
  for(statecode in states) {

    # Subset the data for the state.
    d2 <- subset.data.frame(d, d$state == statecode)

    # Order by rate then name.
    d2 <- d2[order(d2$rate, d2$name),]
    
    # Add a new column with the rank.
    d2$rank <- c(1:nrow(d2))
    
    # Process num to identify the rank to find.
    targetRank <- new("integer")
    if(num == "best") {
      targetRank <- 1
    }
    else {
      if(num == "worst") {
        targetRank <- nrow(d2)
      }
      else {
        if(num %in% d2$rank) {
          targetRank <- num
        }
        else {
          targetRank <- NA
        }
      }
    }

    # Deal with no rows.
    if(nrow(d2) == 0) {
      d2 <- d[1,]
      
      # Set name to NA.
      d2[1, "name"] <- NA
      
      # Set the state.
      d2[1, "state"] <- statecode
      
      resultsDF <- rbind(resultsDF, d2[, c("name", "state")])
      next
    }
    
    # Deal with invalid target rank.
    if(is.na(targetRank)) {
      # Reduce to one row.
      d2 <- d2[1,]
      
      # Set name to NA.
      d2[1, "name"] <- NA
      
      # Bind to the results DF.
      resultsDF <- rbind(resultsDF, d2[, c("name", "state")])
      next
    }
    
    # Else bind the result row.
    resultsDF <- rbind(resultsDF, d2[d2$rank == targetRank, c("name", "state")])

  }
  
  # Fix column name.
  colnames(resultsDF)[1] <- "hospital"
  
  # Set row names and return.
  rownames(resultsDF) <- resultsDF$state
  return(resultsDF[order(resultsDF$state),])

}
