#### This code is for Quiz 2 of the Coursera course "Getting and Cleaning Data"
#### Date: June 12, 2014
#### Author: Mark Dakkak

##################### Install packages ##################### 

install.packages("httr")
library(httr)

install.packages("httpuv")
library(httpuv)

install.packages("jsonlite")
library(jsonlite)

options(gsubfn.engine = "R") #add this line before loading sqldf 
install.packages("sqldf")
library(sqldf)

##################### Question 1 ##################### 
# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications
#    Insert your values below - if secret is omitted, it will look it up in
#    the GITHUB_CONSUMER_SECRET environmental variable.
#
#    Use http://localhost:1410 as the callback url
myapp <- oauth_app("github",key="02a6ed8c836a3e828392",secret = "004522022f5357f81bc5e638c25d661ab8431292")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))

json1 = content(req)
# Uses a from.json function from JSON io package
# json is a structured R object

json2 = jsonlite::fromJSON(toJSON(json1))
## Take a json object and use the jsonlite function to convert to a data frame

names(json2)
json2[, 45]
json2[, 2]

class(json2)

json2_subset <- json2[, c(2, 45)]
# The repo "datasharing" was created at 2013-11-07T13:25:07Z

##################### Question 2 ##################### 


fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile = "/Users/sommpd10/Desktop/Quiz2.csv", method = "curl")
data <- read.csv("/Users/sommpd10/Desktop/Quiz2.csv")

names(data)

subset <- sqldf("select * from data where AGEP < 50 and pwgtp1")
subset <- sqldf("select pwgtp1 from data where AGEP < 50")
# Output is correct!

##################### Question 3 ##################### 
# unique () returns a vector, data frame or array like x but with duplicate elements/rows removed

Ages1 <- unique(data$AGEP)
Ages2 <- sqldf("select distinct AGEP from data")
# Output is correct!

Ages1 <- as.data.frame(Ages1)

##################### Question 4 ##################### 
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
## Reads in the lines from the page connected to
close(con)
htmlCode
## prints one long string of letters of HTML code

class(htmlCode)

nchar(htmlCode[10])
# Number of characters in 10th line of code - 45

nchar(htmlCode[20])
# Number of characters in 20th line of code - 31

nchar(htmlCode[30])
# Number of characters in 30th line of code - 7

nchar(htmlCode[100])
# Number of characters in 10th line of code - 25

##################### Question 5 ##################### 
data <- read.fwf("/Users/sommpd10/Desktop/getdata-wksst8110.for", skip=4, widths=c(12, 7,4, 9,4, 9,4, 9,4))

data2 <- read.fwf("/Users/sommpd10/Desktop/getdata-wksst8110.for", skip=4, widths=c(10, 9,4, 9,4, 9,4, 9,4))

colSums(data[, c(2:4)])
colSums(data2[, c(2:4)])

tail(data, n=50)
