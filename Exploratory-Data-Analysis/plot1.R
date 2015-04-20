#### This code is for Plot 1 of project 1 for the Coursera course "Exploratory Data Analysis"
#### Date: June 8, 2014
#### Author: Mark Dakkak

######### Set working directory #########

setwd("/Users/sommpd10/datasciencecoursera/Exploratory-Data-Analysis")

######### Load data.table library #########

library(data.table)

######### Read in zip file #########

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
file <- "household_power_consumption"
download.file(url, file, method = "curl")

unzip(file, exdir = "/Users/sommpd10/datasciencecoursera/Exploratory-Data-Analysis")
# Creates the "household_power_consumption.txt" file!

data <- fread("household_power_consumption.txt")

######### Clean data #########

class(data$Date)
class(data$Time)
# Data and time variables are characters

# Change the format of Date variable
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
class(data$Date)

# Subset the data for the two dates of interest
data_subset <- data[data$Date=="2007-02-01" | data$Date=="2007-02-02"]

# Convert data subset to a data frame
class(data_subset)
data_subset <- data.frame(data_subset)

# Convert columns to numeric
for(i in c(3:9)) {data_subset[,i] <- as.numeric(as.character(data_subset[,i]))}

# Create Date_Time variable
data_subset$Date_Time <- paste(data_subset$Date, data_subset$Time)

# Convert Date_Time variable to proper format
data_subset$Date_Time <- strptime(data_subset$Date_Time, format="%Y-%m-%d %H:%M:%S")
class(data_subset$Date_Time)

######### Plot 1 #########

png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")

par(mar = c(6, 6, 5, 4))

hist(data_subset$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
dev.off()
