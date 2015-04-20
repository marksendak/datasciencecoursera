#### This code is for Quiz 4 of the Coursera course "Getting and Cleaning Data"
#### Date: June 26, 2014
#### Author: Mark Dakkak

##################### Load libraries ##################### 
library(RCurl)
        ## use to get https URLs to work

install.packages("stringr")
library(stringr)

install.packages("quantmod")
library(quantmod)

library(xts)

##################### Question 1 ##################### 
URL <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
Q1_data <- read.csv(text = URL)

strsplit(names(Q1_data), "wgtp")

##################### Question 2 ##################### 
URL2 <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv")
Q2_data <- read.csv(text = URL2, skip=3, header=TRUE, stringsAsFactors=FALSE)

Q2_data <- Q2_data[2:191, ]

Q2_data$US.dollars. <- gsub(",", "", Q2_data$US.dollars.)
        ## substitutes commas with nothing
        ## have to use gsub, because using sub only removes the FIRST comma

Q2_data$US.dollars. <- str_trim(Q2_data$US.dollars.)
        ## removes the trailing spaces

Q2_data$US.dollars. <- as.numeric(Q2_data$US.dollars.)

summary(Q2_data)

##################### Question 3 ##################### 

## Fix names
Q2_data <- Q2_data[, c(1,2,4,5)]
names(Q2_data)[names(Q2_data) == "Economy"] <- "countryNames"
names(Q2_data)[names(Q2_data) == "X"] <- "CountryCode"
names(Q2_data)[names(Q2_data) == "US.dollars."] <- "GDP"

grep("^United", Q2_data$countryNames)

##################### Question 4 ##################### 
ED_URL <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv")
ED <- read.csv(text = ED_URL)

Q4_data <- merge(Q2_data, ED, by = "CountryCode")

grep("[Ff]iscal(.*)[Jj][Uu][Nn][Ee]", Q4_data$Special.Notes, value = TRUE)

##################### Question 5 #####################
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 

## Work with row.indices, which denote time
Subset_data <- amzn['20120101/20121231']
        ## 250 values from 2012
        ## ref: http://cran.r-project.org/web/packages/xts/xts.pdf

Mondays <- Subset_data[.indexwday(Subset_data) == 0]
        ## the index for Monday is 0
        ## ref: http://stackoverflow.com/questions/16805231/r-search-for-a-particular-time-from-index