corr <- function(directory, threshold = 0) {
    ## create list of files
    files_list <- list.files(directory, full.names = TRUE)
    
    ## create an empty numeric vector
    complete <- c()
    
    ## look for complete cases, bind correlation of good data together
    for (i in 1:332) {
        data <- read.csv(files_list[i])
        good <- complete.cases(data)
        data_subset <- data[good, ]
        if (nrow(data_subset) > threshold) {
          correlation <- cor(data_subset$sulfate, data_subset$nitrate)
          complete <- c(complete, correlation)
        }
    }
    
    complete
}