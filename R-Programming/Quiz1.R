setwd("~/datasciencecoursera")
data <- read.csv ("/Users/sommpd10/datasciencecoursera/hw1_data.csv")

colnames(data)
# ref: http://stat.ethz.ch/R-manual/R-patched/library/base/html/colnames.html

head(data,2)
# ref: http://rfunction.com/archives/699

nrow(data)
# ref: http://stat.ethz.ch/R-manual/R-patched/library/base/html/nrow.html

tail(data,2)
# ref: http://rfunction.com/archives/699

data[47,1]
# Element in the 47th row and 1st column
# ref: http://www.r-tutor.com/r-introduction/matrix

data[47,]
# Prints the entire 47th row
# ref: http://www.r-tutor.com/r-introduction/matrix

#16
data$Ozone
# Pulls out the "Ozone" column

sum(is.na(data$Ozone))
# Adds all the NAs in the column "Ozone"

#17
mean(data$Ozone, na.rm = TRUE)
# Returns the mean of column "Ozone", removing all NA values
# ref: http://www.ats.ucla.edu/stat/r/faq/missing.htm

colMeans(data, na.rm = TRUE)
# Returns the means of each column, removing all NA values
# ref: http://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html

#18
subset1 <- subset(data, Ozone > 31 & Temp > 90)
# Creates a new data set, subset1, that filters by certain values of Ozone and Temp
# ref: http://www.statmethods.net/management/subset.html

mean(subset1$Solar.R)
# Returns the mean of column "Solar.R" in data frame subset1

#19
subset2 <- subset(data, Month == 6)
# Creates a new data set, subset2, that selects out the observations where Month == 6
# ref: http://www.statmethods.net/management/subset.html

subset2a <- data[data$Month == 6, ]
# ref: http://www.ats.ucla.edu/stat/r/faq/subset_R.htm

identical(subset2, subset2a)
# The two subsets are identical

mean(subset2$Temp)

#20
subset3 <- subset(data, Month == 5)
# Creates a subset of the data frame that extracts all observations where Month == 5
# ref: http://www.statmethods.net/management/subset.html

apply(subset3, 2, max, na.rm = TRUE)
# The apply() function applies a function over every row or column of a matrix or data frame. Above, the apply() function returns a vector with the maximum for each column and conveniently uses the column names as names for this vector as well.
# The second argument, 2, specifies the dimension or index over which the function has to be applied. The number 1 means row-wise, and the number 2 means column-wise. Here we apply the funciton over the columns.
# The na.rm = TRUE drops all values of NA when calculating the max in each column
# ref: http://www.dummies.com/how-to/content/how-to-apply-functions-on-rows-and-columns-in-r.html