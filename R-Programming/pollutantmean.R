pollutantmean <- function(directory, pollutant, id = 1:332){
        
        combined <- data.frame()
        
        for (i in id){
                
                fullid <- formatC(i, width = 3, format = "d", flag = "0")
                filename <- paste(directory, "/", fullid, ".csv", sep="")
                
                temp_dataset <- read.csv(filename, header = TRUE)

                combined <- rbind(combined, temp_dataset)
                rm(temp_dataset)
        }
        
        column <- combined[ ,pollutant]
        mean(column, na.rm = TRUE)
        
}