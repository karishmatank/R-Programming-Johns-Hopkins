best <- function(state, outcome) {
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
    
    ## Return hospital name in state with lowest 30 day death rate    
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
    
    ## Look for hospital with lowest 30-day mortality rate
    minimum <- which.min(dataByState[[columnNumber]])
    dataByState[minimum, "Hospital.Name"]
}