#### This code is for plot 3 of course project 2 for the Coursera course "Exploratory Data Analysis"
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
## pull out Baltimore data
Baltimore <- subset(NEI, NEI$fips == "24510")

## Melt & cast
Balt_Melt <- melt(Baltimore, id=c("year", "type"), measure.vars=c("Emissions"))

Balt_cast <- dcast(Balt_Melt, year + type ~ variable, sum)

## convert type to factor
Balt_cast$type <- as.factor(Balt_cast$type)

######### Make PNG Plot #########
png(filename = "/Users/sommpd10/datasciencecoursera/Exploratory-Data-Analysis/Project 2/plot3.png", width = 480, height = 480, units = "px", bg = "white")

g <- ggplot(Balt_cast, aes(x = year, y = Emissions, color = type))
g + geom_line() + labs(title = expression('Total PM' [2.5] * ' Emissions Per Year in Baltimore by Type of Emission'))

dev.off()
