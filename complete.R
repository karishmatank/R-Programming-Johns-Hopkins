complete <- function(directory, id = 1:332) {
    ## create a list of files
    files_list <- list.files(directory, full.names = TRUE)
    
    ## create an empty data frame
    complete <- data.frame()
    
    ##look for complete cases, bind rows
    for(i in id) {
        data <- read.csv(files_list[i])
        good <- complete.cases(data)
        data_subset <- data[good, ]
        row_entry <- c(i,nrow(data_subset))
        complete <- rbind(complete, row_entry)
    }
    
    #name the columns
    colnames(complete) <- c("id", "nobs")
    
    complete
}