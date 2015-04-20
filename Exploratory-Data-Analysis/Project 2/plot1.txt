#### This code is for plot 1 of course project 2 for the Coursera course "Exploratory Data Analysis"
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

######### Change working directory #########
setwd("/Users/sommpd10/datasciencecoursera/Exploratory-Data-Analysis/Project 2")

######### Build table #########
Q1_df <- aggregate(NEI$Emissions ~ NEI$year, FUN = sum)

## Convert names
names(Q1_df)[1] <- "Year"
names(Q1_df)[2] <- "Emissions"

######### Make PNG Plot #########
png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")

barplot(Q1_df$Emissions, main=expression('Total PM' [2.5] * ' Emissions Per Year'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()