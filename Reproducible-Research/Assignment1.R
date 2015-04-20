#### This code is for Peer Assessment Assignment 1 of the Coursera course "Reproducible Research"
#### Date: July 18, 2014
#### Author: Mark Dakkak

######### Set working directory #########
Rep_Research <- "/Users/sommpd10/datasciencecoursera/Reproducible-Research"
setwd(Rep_Research)

######### Read zip file and save to wd #########
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file <- "activity"
download.file(url, file, method = "curl")
unzip(file, exdir = Rep_Research)

######### Read in data #########
data <- read.csv("activity.csv", header=TRUE)

## clean classes
data$date <- as.Date(data$date, format = "%Y-%m-%d")
class(data$interval)

######### Q1) What is mean total number of steps taken per day?
## Make table with daily sums
Daily_Sums <- aggregate(data$steps, by = list(data$date), FUN = "sum")
length(data)
colnames(Daily_Sums) <- c("Date", "Total_Steps")

## Graph daily sums
library(ggplot2)
ggplot(Daily_Sums, aes(x = Total_Steps)) + geom_histogram(binwidth = 1000, fill="white", color="black") + labs(title="Distribution of Total Steps per Day", x = "Total Steps per Day")

## Calculate mean and median
mean(Daily_Sums$Total_Steps, na.rm=TRUE)
median(Daily_Sums$Total_Steps, na.rm=TRUE)

######### Q2) What is the average daily activity pattern?
## Make table with average number of steps for every intervals across days
data_noNAs <- data[!is.na(data$steps), ]
Interval_Means <- aggregate(data_noNAs$steps, by = list(data_noNAs$interval), FUN = "mean")
colnames(Interval_Means) <- c("Time_Interval", "Mean_Steps")

## Graph daily trend
library(ggplot2)
g <- ggplot(Interval_Means, aes(x = Time_Interval, y = Mean_Steps))
g + geom_line() + labs(title="Average Number of Steps \n During 5 Minute Intervals Throughout the Day", x = "Time Interval", y = "Average Number of Steps")

## Find max number of steps throughout the day
Max_Interval <- Interval_Means[which.max(Interval_Means[, 2]), 1]
        ## Alternatively: Max_Interval <- Interval_Means[Interval_Means$Mean_Steps==max(Interval_Means$Mean_Steps), 1]
Max_Value <- Interval_Means[which.max(Interval_Means[, 2]), 2]

######### Q3) Imputing missing values
## Calculate number of rows with missing values
sum(complete.cases(data))

## Impute missing values with means
data_NAs_Imputed <- merge(data, Interval_Means, by.x = "interval", by.y = "Time_Interval", all.x = TRUE)
data_NAs_Imputed$steps[is.na(data_NAs_Imputed$steps)] <- data_NAs_Imputed$Mean_Steps[is.na(data_NAs_Imputed$steps)]

## Make table with daily sums
Daily_Sums_Imputed <- aggregate(data_NAs_Imputed$steps, by = list(data_NAs_Imputed$date), FUN = "sum")
colnames(Daily_Sums_Imputed) <- c("Date", "Total_Steps")

## Graph daily sums
library(ggplot2)
ggplot(Daily_Sums_Imputed, aes(x = Total_Steps)) + geom_histogram(binwidth = 1000, fill="white", color="black") + labs(title="Distribution of Total Steps per Day \n Using Date Set with Imputed Values", x = "Total Steps per Day")

## Calculate mean and median
mean(Daily_Sums_Imputed$Total_Steps, na.rm=TRUE)
median(Daily_Sums_Imputed$Total_Steps, na.rm=TRUE)

######### Q4) Are there differences in activity patterns between weekdays and weekends?
## Create weekday factor variable
data_NAs_Imputed$Weekday <- ifelse(weekdays(data_NAs_Imputed$date) != "Saturday" & weekdays(data_NAs_Imputed$date) != "Sunday", "Weekday", "Weekend")
data_NAs_Imputed$Weekday <- as.factor(data_NAs_Imputed$Weekday)

## Create table that shows activity trends for weekdays and weekends
Interval_Means_Imputed <- aggregate(data_NAs_Imputed$steps, by = list(data_NAs_Imputed$Weekday, data_NAs_Imputed$interval), FUN="mean")
names(Interval_Means_Imputed) <- c("Weekday", "Interval", "Mean_Steps")

## Graph daily trend comparing weekday to weekend
library(ggplot2)
g <- ggplot(Interval_Means_Imputed, aes(x = Interval, y = Mean_Steps))
g + geom_line() + facet_grid(Weekday ~ .) + labs(title="Average Number of Steps \n During 5 Minute Intervals Throughout the Day", x = "Time Interval", y = "Average Number of Steps")

######### AFTER CREATE THE .Rmd FILE, RUN THIS TO GET THE MD FILE AND FIGURES!
library(knitr)
file <- "/Users/sommpd10/datasciencecoursera/Reproducible-Research/PA1_template.Rmd"
knit(file)
