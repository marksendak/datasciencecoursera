complete <- function(directory, id = 1:332) {
        
        combined <- data.frame()
        
        for (i in id){
                
                fullid <- formatC(i, width = 3, format = "d", flag = "0")
                filename <- paste(directory, "/", fullid, ".csv", sep="")
                
                nobs <- sum(complete.cases(read.csv(filename, header = TRUE)))
                
                newrow <- c(i, nobs)
                combined <- rbind(combined, newrow)
                # rbind appends datasets by rows
        }
        
        colnames(combined) <- c("id", "nobs")
        combined
}