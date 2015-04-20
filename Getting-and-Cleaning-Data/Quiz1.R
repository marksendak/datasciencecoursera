################## Question 1 ##################
getwd()

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile = "./Getting-and-Cleaning-Data/Quiz1/AmericanCommunitySurvey2006.csv", method = "curl")

fileURL_2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(fileURL_2, destfile = "./Getting-and-Cleaning-Data/Quiz1/DataDictionary.pdf", method = "curl")

Q1data <- read.csv("Getting-and-Cleaning-Data/Quiz1/AmericanCommunitySurvey2006.csv", header=TRUE)

names(Q1data)

table(Q1data$VAL)
# ref: http://www.r-bloggers.com/r-function-of-the-day-table/

################## Question 2 ##################
table(Q1data$FES)

sum(Q1data$FES==NA)
# This always gives back NA
sum(is.na(Q1data$FES))
# This is the proper syntax to calculate the number of observations with NA
sum(Q1data$FES==1, na.rm=TRUE)
class(Q1data$FES)

################## Question 3 ##################
fileURL_3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileURL_3, destfile = "./Getting-and-Cleaning-Data/Quiz1/NaturalGasAcquisitionProgram.xlsx", method = "curl")

library(xlsx)
dat <- read.xlsx("Getting-and-Cleaning-Data/Quiz1/NaturalGasAcquisitionProgram.xlsx", sheetIndex = 1, rowIndex=18:23, colIndex=7:15, header=TRUE)

sum(dat$Zip*dat$Ext,na.rm=T)

################## Question 4 ##################
# ref: http://giventhedata.blogspot.com/2012/06/r-and-web-for-beginners-part-ii-xml-in.html

install.packages("XML")
library(XML)

# Save the URL of the xml file in a variable
xml.url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

# Use the xmlTreePares-function to parse xml file directly from the web
xmlfile <- xmlTreeParse(xml.url)

# the xml file is now saved as an object you can easily work with in R:
class(xmlfile)

# Use the xmlRoot-function to access the top node
rootnode <- xmlRoot(xmlfile)

r <- rootnode[[1]]

# have a look at the XML-code of the first subnodes:
r[[1]]
        # First element

r[1:2]
        # First two elements

# To extract the XML-values from the document, use xmlSApply:
restaurants <- xmlSApply(r, function(x) xmlSApply(x, xmlValue))

# Finally, get the data in a data-frame and have a look at the first rows and columns
restaurants_df <- data.frame(t(restaurants),row.names=NULL)

class(restaurants_df$zipcode)
#zipcode is a list
sum(restaurants_df$zipcode=="21231")
# 127 restaurants!

################## Question 5 ##################
install.packages("data.table")
library(data.table)

fileURL_4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL_4, destfile = "./Getting-and-Cleaning-Data/Quiz1/IdahoHousing.csv", method = "curl")

DT <- fread("Getting-and-Cleaning-Data/Quiz1/IdahoHousing.csv")

# Compare ways to calculate the average of variable pwgtp15 by sex
DT[,mean(pwgtp15),by=SEX]
