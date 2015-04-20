#### This code is for Plot 3 of project 1 for the Coursera course "Exploratory Data Analysis"
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

######### Plot 3 #########

png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")

par(mar = c(7, 6, 5, 4))

plot(data_subset$Date_Time, data_subset$Sub_metering_1, xaxt=NULL, xlab = "", ylab = "Energy sub metering", type="n")
## Sets up the plot, but does not populate with any data

lines(data_subset$Date_Time, data_subset$Sub_metering_1, col = "black", type = "S")
## Plots lines for sub_metering_1
lines(data_subset$Date_Time, data_subset$Sub_metering_2, col = "red", type = "S")
## Plots lines for sub_metering_2
lines(data_subset$Date_Time, data_subset$Sub_metering_3, col = "blue", type = "S")
## Plots lines for sub_metering_3

legend("topright", lty = c(1, 1), lwd = c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
# Adds a legend with lines
# lwd = c(1, 1, 1) assigns the lines widths of 1
# lty = c(1, 1) assigns the line type within the legend

dev.off()