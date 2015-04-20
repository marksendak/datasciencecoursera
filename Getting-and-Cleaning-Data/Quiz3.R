#### This code is for Quiz 3 of the Coursera course "Getting and Cleaning Data"
#### Date: June 19, 2014
#### Author: Mark Dakkak

##################### Install packages ##################### 
install.packages("RCurl")
library(RCurl)
        ## This is for question 1, to import https files without saving them on the computer

install.packages("jpeg")
library(jpeg)

install.packages("plyr")
library(plyr)

install.packages("Hmisc")
library(Hmisc)

##################### Question 1 ##################### 
fileURL <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
data <- read.csv(text = fileURL)
        ## ref: http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r

names(data)

# Variable "ACR" is "Lot Size", level 3: house on ten or more acres
# Variable "AGS" is "Sales of Agricultural Products", level 6: $10,000 +

## Create logical vector
agricultureLogical <- data[which(data$ACR == 3 & data$AGS == 6), ]

head(agricultureLogical)

##################### Question 2 ##################### 

myurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url=myurl, destfile="/Users/sommpd10/Desktop/Image.jpeg", mode="wb", method = "curl")
        ## mode = "wb" is for binary files, such as a jpeg
        ## ref: https://class.coursera.org/getdata-004/forum/thread?thread_id=7

image <- readJPEG("/Users/sommpd10/Desktop/Image.jpeg", native = TRUE)


quantile(image, probs = c(0.3, 0.8))
        ## 30%: -15259150, 80%: -10575416 

##################### Question 3 ##################### 
GDP_URL <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv")
GDP <- read.csv(text = GDP_URL, skip=3, header=TRUE, stringsAsFactors=FALSE)
        ## Don't want R to read in the Ranking variable as a factor
        ## ref: https://class.coursera.org/getdata-004/forum/thread?thread_id=10

ED_URL <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")
ED <- read.csv(text = ED_URL)

## Fix column names in GDP
GDP <- GDP[, c(1,2,4,5)]

GDP <- GDP[2:191, ]

names(GDP)[names(GDP)=="X"] <- "CountryCode"

## Merge files
Total <- merge(GDP, ED, by = "CountryCode")
        ## 189 files match

## Change class of ranking
Total$Ranking <- as.numeric(Total$Ranking)

## Sort by GDP
Rank_order <- order(-Total$Ranking)
sorted_Total <- Total[Rank_order, ]
        ## 13th line: St. Kitts and Nevis

##################### Question 4 ##################### 
tapply(sorted_Total$Ranking, sorted_Total$Income.Group, mean)
        ## Mean ranking across the levels of Income Group

##################### Question 5 ##################### 
sorted_Total$GDP_Groups = cut2(sorted_Total$Ranking, g=5)
        ## East cutting that breaks countries up into 5 groups based on GDP rankings
        ## Doesn't require you to provide the cut points

class(sorted_Total$GDP_Groups)
levels(sorted_Total$GDP_Groups)

table(sorted_Total$GDP_Groups, sorted_Total$Income.Group)
        ## Creates a 2x2 table