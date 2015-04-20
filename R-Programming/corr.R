corr <- function(directory, threshold = 0) {
        
        cor.vector <- vector('numeric')
        
        complete.cases <- complete(directory)
        above.threshold <- subset(complete.cases, nobs > threshold)
        
        for(i in above.threshold[, 1]){
                
                fullid <- formatC(i, width = 3, format = "d", flag = "0")
                filename <- paste(directory, "/", fullid, ".csv", sep="")
                
                temp_dataset <- read.csv(filename, header = TRUE)
                
                cor.matrix <- cor(temp_dataset[c(2,3)], use = "complete.obs")
                cor.vector <- append(cor.vector, cor.matrix[2,1])   
        }
        cor.vector
}