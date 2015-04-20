setwd("~/datasciencecoursera")

### Part 1 ###
pollutantmean <- function(directory, pollutant, id = 1:332){
        
        combined <- data.frame()
        # Create an empty data frame

        for (i in id){
    
                fullid <- formatC(i, width = 3, format = "d", flag = "0")
                # formatC takes the ID and makes sure it's 3 characters long by adding leading 0's
                # ref: http://stat.ethz.ch/R-manual/R-devel/library/base/html/formatc.html
                filename <- paste(directory, "/", fullid, ".csv", sep="")
            
                temp_dataset <- read.csv(filename, header = TRUE)
                combined <- rbind(combined, temp_dataset)
                # rbind appends datasets by rows
                
                rm(temp_dataset)
        }
  
        column <- combined[ ,pollutant]
        mean(column, na.rm = TRUE)

}

pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 1:10)

### Part 2 ###
complete <- function(directory, id = 1:332) {

        combined <- data.frame()
        
        for (i in id){
                
               fullid <- formatC(i, width = 3, format = "d", flag = "0")
                # formatC takes the ID and makes sure it's 3 characters long by adding leading 0's
                # ref: http://stat.ethz.ch/R-manual/R-devel/library/base/html/formatc.html
               filename <- paste(directory, "/", fullid, ".csv", sep="")

               nobs <- sum(complete.cases(read.csv(filename, header = TRUE)))
                
                newrow <- c(i, nobs)
                combined <- rbind(combined, newrow)
                # rbind appends datasets by rows
                }
        
        colnames(combined) <- c("id", "nobs")
        combined
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

### Part 3 ###
corr <- function(directory, threshold = 0) {

        #Create a numeric vector
        cor.vector <- vector('numeric')
        
        #Find the set of monitors that have a number of complete cases above the threshold
        complete.cases <- complete(directory)
        above.threshold <- subset(complete.cases, nobs > threshold)
        
        for(i in above.threshold[, 1]){
                
                fullid <- formatC(i, width = 3, format = "d", flag = "0")
                # formatC takes the ID and makes sure it's 3 characters long by adding leading 0's
                # ref: http://stat.ethz.ch/R-manual/R-devel/library/base/html/formatc.html
                filename <- paste(directory, "/", fullid, ".csv", sep="")
                
                temp_dataset <- read.csv(filename, header = TRUE)
                
                cor.matrix <- cor(temp_dataset[c(2,3)], use = "complete.obs")
                cor.vector <- append(cor.vector, cor.matrix[2,1])   
        }
        cor.vector
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)