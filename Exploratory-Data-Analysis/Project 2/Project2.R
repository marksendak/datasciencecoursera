#### This code is for course project 2 of the Coursera course "Exploratory Data Analysis"
#### Date: June 21, 2014
#### Author: Mark Dakkak

######### Load library #########

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

######### Question 1 #########
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008

####### Using tapply
# Convert year to a factor
NEI$year <- as.factor(NEI$year)
class(NEI$year)
levels(NEI$year)


tapply(NEI$Emissions, NEI$year, sum)
        ## Output is not in a data frame

####### Using aggregate
        ## ref: http://stat.ethz.ch/R-manual/R-patched/library/stats/html/aggregate.html
Q1_df <- aggregate(NEI$Emissions ~ NEI$year, FUN = sum)

## Convert names
names(Q1_df)[1] <- "Year"
names(Q1_df)[2] <- "Emissions"

####### Make plot
## On screen
quartz()

barplot(Q1_df$Emissions, main=expression('Total PM' [2.5] * ' Emissions Per Year'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()

## PNG
png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")

barplot(Q1_df$Emissions, main=expression('Total PM' [2.5] * ' Emissions Per Year'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()

######### Question 2 #########
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

## pull out Baltimore data
Baltimore <- subset(NEI, NEI$fips == "24510")

####### Using aggregate
        ## ref: http://stat.ethz.ch/R-manual/R-patched/library/stats/html/aggregate.html
Q2_df <- aggregate(Baltimore$Emissions ~ Baltimore$year, FUN = sum)

## Convert names
names(Q2_df)[1] <- "Year"
names(Q2_df)[2] <- "Emissions"

####### Make plot
## On screen
quartz()

barplot(Q2_df$Emissions, main=expression('Total PM' [2.5] * ' Emissions Per Year in Baltimore'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()

## PNG
png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")

barplot(Q2_df$Emissions, main=expression('Total PM' [2.5] * ' Emissions Per Year in Baltimore'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()

######### Question 3 #########
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

####### Using melt & cast
Balt_Melt <- melt(Baltimore, id=c("year", "type"), measure.vars=c("Emissions"))

Balt_cast <- dcast(Balt_Melt, year + type ~ variable, sum)

## convert type to factor
Balt_cast$type <- as.factor(Balt_cast$type)

####### Make plot
## On screen
quartz()

g <- ggplot(Balt_cast, aes(x = year, y = Emissions, color = type))
g + geom_line() + labs(title = expression('Total PM' [2.5] * ' Emissions Per Year in Baltimore by Type of Emission'))

dev.off()

## PNG
png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")

g <- ggplot(Balt_cast, aes(x = year, y = Emissions, color = type))
g + geom_line() + labs(title = expression('Total PM' [2.5] * ' Emissions Per Year in Baltimore by Type of Emission'))

dev.off()

######### Question 4 #########
## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

####### Find coal combustion-related activities
Coal_Subset <- SCC[grep("Coal", SCC$EI.Sector), ]
Coal_SCCs <- Coal_Subset[, 1]

## Make the coal SCCs a vector from a factor
Coal_SCCs <- as.vector(Coal_SCCs)

####### Subset NEI by the Coal SCCs 
Coal_NEI <- NEI[NEI$SCC %in% Coal_SCCs, ]

####### Using aggregate
## ref: http://stat.ethz.ch/R-manual/R-patched/library/stats/html/aggregate.html
Q4_df <- aggregate(Coal_NEI$Emissions ~ Coal_NEI$year, FUN = sum)

## Convert names
names(Q4_df)[1] <- "Year"
names(Q4_df)[2] <- "Emissions"

####### Make plot
## On screen
quartz()

barplot(Q4_df$Emissions, main= expression('Total PM' [2.5] * ' Emissions Per Year from Coal'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()

## PNG
png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")

barplot(Q4_df$Emissions, main= expression('Total PM' [2.5] * ' Emissions Per Year from Coal'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions'))

dev.off()

######### Question 5 #########
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

####### Find motor vehicle-related activities
MV_Subset <- SCC[grep("Locomotives|Vehicles", SCC$EI.Sector), ]
MV_SCCs <- MV_Subset[, 1]

## Make the MV SCCs a vector from a factor
MV_SCCs <- as.vector(MV_SCCs)

####### Subset Baltimore by the MV SCCs 
MV_Baltimore <- Baltimore[Baltimore$SCC %in% MV_SCCs, ]

####### Using aggregate
## ref: http://stat.ethz.ch/R-manual/R-patched/library/stats/html/aggregate.html
Q5_df <- aggregate(MV_Baltimore$Emissions ~ MV_Baltimore$year, FUN = sum)

## Convert names
names(Q5_df)[1] <- "Year"
names(Q5_df)[2] <- "Emissions"

####### Make plot
## On screen
quartz()

barplot(Q5_df$Emissions, main= expression('Total PM' [2.5] * ' Emissions from Motor Vehicles in Baltimore Per Year'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions from Motor Vehicles in Baltimore'))

dev.off()

## PNG
png(filename = "plot5.png", width = 480, height = 480, units = "px", bg = "white")

barplot(Q5_df$Emissions, main= expression('Total PM' [2.5] * ' Emissions from Motor Vehicles in Baltimore Per Year'), names.arg=c("1999", "2002", "2005", "2008"), xlab = "Year", ylab = expression('Total PM' [2.5] * ' Emissions from Motor Vehicles in Baltimore'))

dev.off()

######### Question 6 #########
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

## Subset NEI for Los Angeles
LA <- subset(NEI, NEI$fips == "06037")

####### Subset LA by the MV SCCs 
MV_LA <- LA[LA$SCC %in% MV_SCCs, ]

####### Using aggregate
## ref: http://stat.ethz.ch/R-manual/R-patched/library/stats/html/aggregate.html
Q6_df <- aggregate(MV_LA$Emissions ~ MV_LA$year, FUN = sum)

## Convert names
names(Q6_df)[1] <- "Year"
names(Q6_df)[2] <- "Emissions"

## Add variable for location
Q6_df$Location <- "Los Angeles"
Q5_df$Location <- "Baltimore"

## Append Baltimore and Los Angeles Data
Q6_df <- rbind(Q6_df, Q5_df)

####### Make plot
## On screen
quartz()

g <- ggplot(Q6_df, aes(x = Year, y = Emissions, color = Location))
g + geom_line() + labs(title = expression('Total PM' [2.5] * ' Emissions from Motor Vehicles Per Year by Location'))

dev.off()

## PNG
png(filename = "plot6.png", width = 480, height = 480, units = "px", bg = "white")

g <- ggplot(Q6_df, aes(x = Year, y = Emissions, color = Location))
g + geom_line() + labs(title = expression('Total PM' [2.5] * ' Emissions from Motor Vehicles Per Year by Location'))

dev.off()
