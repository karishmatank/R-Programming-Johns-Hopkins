rankhospital <- function(state, outcome, num){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validStates = unique(data[, 7])
  if (!state %in% validStates){
    stop("invalid state")
  }
  
  validOutcomes = c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% validOutcomes) {
    stop("invalid outcome")
  }
      
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
  
  ## Take out NA values, sort by state
  data <- data[complete.cases(data[[columnNumber]]), ]
  dataByState <- data[data$State==state, ]
  
  ## Sort data based on 30-day mortality rate, break ties using alphabetical order
  sortedData <- dataByState[order(dataByState[[columnNumber]], dataByState$Hospital.Name, 
                                  na.last = NA), ]
  
  ## Rank data using num
  if (num == "best") {
      num = 1
  }
  
  else if (num == "worst") {
      num = nrow(sortedData)
  }
  
  else if (is.numeric(num) && num > nrow(sortedData)) {
       return(NA)
  }
  
  sortedData[num, "Hospital.Name"]
}