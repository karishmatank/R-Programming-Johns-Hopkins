pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## create a list of files
    files_list <- list.files(directory, full.names = TRUE)
    
    ## create an empty data frame
    data <- data.frame()
    
    ##bind files together
    for (i in id) {
      data <- rbind(data, read.csv(files_list[i]))
    }
    
    ##identify the mean of the pollutant and strip out NAs
    mean <- mean(data[, pollutant], na.rm = TRUE)
    mean
}