#### This code is for plot 6 of course project 2 for the Coursera course "Exploratory Data Analysis"
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
## pull out Baltimore & LA data
Baltimore <- subset(NEI, NEI$fips == "24510")
LA <- subset(NEI, NEI$fips == "06037")

## Find motor vehicle-related activities
MV_Subset <- SCC[grep("Locomotives|Vehicles", SCC$EI.Sector), ]
MV_SCCs <- MV_Subset[, 1]

## Make the MV SCCs a vector from a factor
MV_SCCs <- as.vector(MV_SCCs)

### Subset Baltimore & LA by the MV SCCs 
MV_Baltimore <- Baltimore[Baltimore$SCC %in% MV_SCCs, ]
MV_LA <- LA[LA$SCC %in% MV_SCCs, ]

######### Create data table for graph #########
Q5_df <- aggregate(MV_Baltimore$Emissions ~ MV_Baltimore$year, FUN = sum)
Q6_df <- aggregate(MV_LA$Emissions ~ MV_LA$year, FUN = sum)

## Convert names
names(Q5_df)[1] <- "Year"
names(Q5_df)[2] <- "Emissions"
names(Q6_df)[1] <- "Year"
names(Q6_df)[2] <- "Emissions"

## Add variable for location
Q5_df$Location <- "Baltimore"
Q6_df$Location <- "Los Angeles"

## Append Baltimore and Los Angeles Data
Q6_df <- rbind(Q6_df, Q5_df)

######### Make PNG Plot #########
png(filename = "/Users/sommpd10/datasciencecoursera/Exploratory-Data-Analysis/Project 2/plot6.png", width = 480, height = 480, units = "px", bg = "white")

g <- ggplot(Q6_df, aes(x = Year, y = Emissions, color = Location))
g + geom_line() + labs(title = expression('Total PM' [2.5] * ' Emissions from Motor Vehicles Per Year by Location'))

dev.off()