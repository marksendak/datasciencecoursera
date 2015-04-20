#### This code is for plot 4 of course project 2 for the Coursera course "Exploratory Data Analysis"
#### Date: June 21, 2014
#### Author: Mark Dakkak

######### Load libraries #########
library(reshape2)
library(ggplot2)

######### Set working directory #########
setwd("/Users/sommpd10/Desktop/exdata-data-NEI_data")

######### Read in files #########
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

######### Clean data #########
## Find coal combustion-related activities
Coal_Subset <- SCC[grep("Coal", SCC$EI.Sector), ]
Coal_SCCs <- Coal_Subset[, 1]

## Make the coal SCCs a vector from a factor
Coal_SCCs <- as.vector(Coal_SCCs)

## Subset NEI by the Coal SCCs 
Coal_NEI <- NEI[NEI$SCC %in% Coal_SCCs, ]

######### Create data table for graph #########
Q4_df <- aggregate(Coal_NEI$Emissions ~ Coal_NEI$year, FUN = sum)

## Convert names
names(Q4_df)[1] <- "Year"
names(Q4_df)[2] <- "Emissions"

######### Make PNG Plot #########
png(filename = "/Users/sommpd10/datasciencecoursera/Exploratory-Data-Analysis/Project 2/plot4.png", width = 480, height = 480, units = "px", bg = "white")

barplot(Q4_df$Emissions, main= expression('Total PM' [2.5] * ' Emissions Per Year from Coal'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()