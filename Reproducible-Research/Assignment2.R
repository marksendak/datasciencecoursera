#### This code is for Peer Assessment Assignment 2 of the Coursera course "Reproducible Research"
#### Date: July 24, 2014
#### Author: Mark Dakkak

######### Set working directory #########
Rep_Research <- "/Users/sommpd10/datasciencecoursera/Reproducible-Research"
setwd(Rep_Research)

######### Load packages #########
library(data.table)
library(stringr)
library(plyr)
library(ggplot2)

######### Read zip file and save to wd #########
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file <- "repdata-data-StormData.csv.bz2"
download.file(url, file, method = "curl")

## Fix settings
## ref: https://class.coursera.org/repdata-004/forum/thread?thread_id=141
Sys.setlocale('LC_ALL', 'C')

## Read in data
to_read = c("NULL", "character", rep("NULL", 4), "character", "character", rep("NULL", 14), "numeric", "numeric", "numeric", "character", "numeric", "character", rep("NULL", 9))
## Only read in useful variables: "BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"
data <- read.csv("repdata-data-StormData.csv", header=TRUE, colClasses=to_read)

## Convert to data table
data <- data.table(data)

######### Process data #########
## Fix date variable
data$BGN_DATE <- as.Date(word(as.character(data$BGN_DATE), 1), format="%m/%d/%Y")
## Create year variable
data[,YEAR:=as.numeric(format(BGN_DATE, "%Y"))]

## Fix property damage and crop damage
data[PROPDMGEXP == "H" | PROPDMGEXP == "h",PROPDMG:= (PROPDMG * 100)]
data[PROPDMGEXP == "K" | PROPDMGEXP == "k",PROPDMG:= (PROPDMG * 1000)]
data[PROPDMGEXP == "M" | PROPDMGEXP == "m",PROPDMG:= (PROPDMG * 1000000)]
data[PROPDMGEXP == "B" | PROPDMGEXP == "b",PROPDMG:= (PROPDMG * 1000000000)]

data[CROPDMGEXP == "H" | CROPDMGEXP == "h",CROPDMG:= (CROPDMG * 100)]
data[CROPDMGEXP == "K" | CROPDMGEXP == "k",CROPDMG:= (CROPDMG * 1000)]
data[CROPDMGEXP == "M" | CROPDMGEXP == "m",CROPDMG:= (CROPDMG * 1000000)]
data[CROPDMGEXP == "B" | CROPDMGEXP == "b",CROPDMG:= (CROPDMG * 1000000000)]

## Fix factor levels
# Combine "EXCESSIVE HEAT" and "HEAT"
data[EVTYPE == "HEAT", EVTYPE:="EXCESSIVE HEAT"]
# Combine "WILD/FOREST FIRE" and "WILDFIRE"
data[EVTYPE == "WILD/FOREST FIRE", EVTYPE:="WILDFIRE"]
# Combine "TSTM WIND" and "THUNDERSTORM WIND" and "THUNDERSTORM WINDS"
data[EVTYPE == "TSTM WIND" | EVTYPE == "THUNDERSTORM WINDS", EVTYPE:="THUNDERSTORM WIND"]
# Combine "FLOOD", "RIVER FLOOD", and "FLASH FLOOD"
data[EVTYPE == "FLASH FLOOD" | EVTYPE == "RIVER FLOOD", EVTYPE:="FLOOD"]
# Combine "HURRICANE OPAL", "HURRICANE/TYPHOON", "HURRICANE", and "TROPICAL STORM"
data[EVTYPE == "HURRICANE OPAL" | EVTYPE == "HURRICANE/TYPHOON" | EVTYPE == "HURRICANE", EVTYPE:="TROPICAL STORM"]


######### Q1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

## Explore event types with greatest health impact
Event_Health_Summary <- data[(FATALITIES != 0 | INJURIES != 0), lapply(.SD, sum), by="EVTYPE", .SDcols=c("FATALITIES", "INJURIES")]
Event_Health_Summary[,HEALTH_DAMAGES:=(FATALITIES + INJURIES)]
Event_Health_Summary <- arrange(Event_Health_Summary, desc(HEALTH_DAMAGES))

## Create list of top 10 dangerous event types
Dangerous_Events <- Event_Health_Summary[1:10,EVTYPE]

## Calculate number of deaths and injuries for each year for dangerous events
Health_Summary <- data[EVTYPE %in% Dangerous_Events, lapply(.SD, sum), by="EVTYPE,YEAR", .SDcols=c("FATALITIES", "INJURIES")]
Health_Summary[,HEALTH_DAMAGES:=(FATALITIES + INJURIES)]

## Create line graph for fatalities
g1 <- ggplot(Health_Summary, aes(x = YEAR, y = FATALITIES, color = EVTYPE)) + geom_line() + labs(title="Number of Fatalities per Year \n Due to Different Types of Weather Events", x = "Year", y = "Total Fatalities")


## Create line graph for total injuries
g2 <- ggplot(Health_Summary, aes(x = YEAR, y = INJURIES, color = EVTYPE)) + geom_line() + labs(title="Number of Injuries per Year \n Due to Different Types of Weather Events", x = "Year", y = "Total Injuries")


## Create line graph for health damages
g3 <- ggplot(Health_Summary, aes(x = YEAR, y = HEALTH_DAMAGES, color = EVTYPE)) + geom_line() + labs(title="Number of Health Damages per Year \n Due to Different Types of Weather Events", x = "Year", y = "Total Health Damages (Injuries + Fatalities)")

## Create multiplot
multiplot(g1, g2, g3, cols=1)

######### Q2) Across the United States, which types of events have the greatest economic consequences?

## Explore event types with greatest economic impact
Event_Econ_Summary <- data[(PROPDMG != 0 | CROPDMG != 0), lapply(.SD, sum), by="EVTYPE", .SDcols=c("PROPDMG", "CROPDMG")]
Event_Econ_Summary[,ECON_DAMAGES:=(PROPDMG + CROPDMG)]
Event_Econ_Summary <- arrange(Event_Econ_Summary, desc(ECON_DAMAGES))

## Create list of top 10 dangerous event types
Costly_Events <- Event_Econ_Summary[1:10,EVTYPE]

## Calculate number of deaths and injuries for each year for dangerous events
Econ_Summary <- data[EVTYPE %in% Costly_Events, lapply(.SD, sum), by="EVTYPE,YEAR", .SDcols=c("PROPDMG", "CROPDMG")]
Econ_Summary[,ECON_DAMAGES:=(PROPDMG + CROPDMG)]

## Create line graph for property damages
g1 <- ggplot(Econ_Summary, aes(x = YEAR, y = PROPDMG, color = EVTYPE)) + geom_line() + labs(title="Property Damage per Year", x = "", y = "")

## Create line graph for crop damages
g2 <- ggplot(Econ_Summary, aes(x = YEAR, y = CROPDMG, color = EVTYPE)) + geom_line() + labs(title="Crop Damage per Year", x = "", y = "")

## Create line graph for Econ damages
g3 <- ggplot(Econ_Summary, aes(x = YEAR, y = ECON_DAMAGES, color = EVTYPE)) + geom_line() + labs(title="Property + Crop Damage per Year", x = "", y = "")
dev.off

multiplot(g1, g2, g3, cols=1)

######### ref: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#################################################################################
#################################################################################
#################################################################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}