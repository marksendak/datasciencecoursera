WD_1999 <- "/Users/sommpd10/Desktop/Data/Rd_501_88101_1999"

setwd("/Users/sommpd10/Desktop/Data")


Data_1999 <- read.table("Rd_501_88101_1999/RD_501_88101_1999-0.txt", sep = "|", comment.char = "#", header = FALSE, na.strings = "")
Data_2012 <- read.table("RD_501_88101_2012/RD_501_88101_2012-0.txt", sep = "|", comment.char = "#", header = FALSE, na.strings = "")

cnames_1999 <- readLines("Rd_501_88101_1999/RD_501_88101_1999-0.txt", 1)
cnames_1999 <- strsplit(cnames_1999, "|", fixed = TRUE)
        ## strsplit returns a list, but only want the first element of the list
cnames_2012 <- readLines("RD_501_88101_2012/RD_501_88101_2012-0.txt", 1)
cnames_2012 <- strsplit(cnames_2012, "|", fixed = TRUE)


names(Data_1999) <- cnames_1999[[1]]
        ## some column names have spaces, which aren't valid
names(Data_1999) <- make.names(cnames_1999[[1]])
        ## make.names() function makes string valid column names
        ## replaced spaces with periods (.)
names(Data_2012) <- cnames_2012[[1]]
        ## some column names have spaces, which aren't valid
names(Data_2012) <- make.names(cnames_2012[[1]])
        ## make.names() function makes string valid column names
        ## replaced spaces with periods (.)

x.0 <- Data_1999$Sample.Value
class(x.0)
str(x.0)
summary(x.0)
        ## see that 13k values are missing

mean(is.na(x.0))
        ## shows that 11% of data is missing

object.size(Data_2012)
        ## calculates the amount of memory required to store the object

x.1 <- Data_2012$Sample.Value
