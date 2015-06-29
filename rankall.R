rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
    ## Check that outcome is valid
    validOutcomes = c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% validOutcomes) {
      stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    if(outcome == "heart attack") {
      columnNumber = 11
    }
    else if (outcome == "heart failure") {
      columnNumber = 17
    }
    else if (outcome == "pneumonia") {
      columnNumber = 23
    }
    
    data[, columnNumber] <- suppressWarnings(as.numeric(data[, columnNumber]))
    
    ## Take out NA values
    data <- data[complete.cases(data[[columnNumber]]), ]
    
    ##For each state, find the hospital of the specified rank
    validStates = sort(unique(data[, 7]))
    newLine <- character()
  
    for(i in validStates) {
      dataByState <- data[data$State==i, ]
      
      ## Sort data based on 30-day mortality rate, break ties using alphabetical order
      sortedData <- dataByState[order(dataByState[[columnNumber]], dataByState$Hospital.Name, 
                                      na.last = NA), ]
      
      ## Rank data using num
      temp = num
      if (temp == "best") {
        temp = 1
      }
      
      if (temp == "worst") {
        temp = nrow(sortedData)
      }
      
      newLine[i] <- sortedData[temp, "Hospital.Name"]
    }
    byState <- data.frame(hospital = newLine, state = validStates, row.names = validStates)
    byState
    
}