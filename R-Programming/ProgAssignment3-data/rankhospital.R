### Part 3: Ranking hospitals by outcome in a state ###
# Author: Mark Dakkak
# Date: June 1, 2014

setwd("~/datasciencecoursera/rprog-data-ProgAssignment3-data")

rankhospital <- function(state, outcome, num = "best") {
        ## Section 1 - Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Section 2 - Check that state and outcome are valid
        state_list <- unique(outcome_data[, 7])
        # unique returns a vector that contains all the unique elements in the 7th column of the outcome data
        # ref: http://stat.ethz.ch/R-manual/R-devel/library/base/html/unique.html
        if(state %in% state_list == FALSE){
                stop("invalid state")
        }
        if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
                stop("invalid outcome")
        }
        
        ## Section 3 - Return hospital name in the given state with the specified rank of 30-day death rate
        # 1) Identify the column number
        col_num <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
        
        # 2) Make a dataset with three columns: Hospital Name, State, and Outcome (outcome of interest). Drop all rows that are not for the given state 
        names(outcome_data)[col_num] <- "Outcome"
        # Renames the third column "Outcome"
        # ref: http://rprogramming.net/rename-columns-in-r/
        
        outcome_subset <- subset(outcome_data, State == state & Outcome != "Not Available", select = c(2, 7, col_num))
        # ref: http://www.ats.ucla.edu/stat/r/faq/subset_R.htm
        
        # 3) Order the data subset by the mortality rate (second column) and then by Hospital Name
        outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
        # To sort by the outcome, first convert the values in that column to numeric values
        
        outcome.order <- with(outcome_subset, order(outcome_subset$Outcome, outcome_subset$Hospital.Name))
        # Sorts the data frame outcome_subset first by the number in the column "Outcome" and then by "Hospital.Name"
        # ref: http://www.dummies.com/how-to/content/how-to-sort-data-frames-in-r.html
        Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]
        
        # 4) Create a column with a variable called "rank"
        Outcome_Subset_Sorted$rank <- rank(Outcome_Subset_Sorted$Outcome, ties.method = "first")
        # creates a new variable called "rank" that takes on the values of the rank. ties.method = "first" makes sure that rows that are tied in rank are given integer ranks in ascending order.
        # ref: http://stat.ethz.ch/R-manual/R-devel/library/base/html/rank.html
        
        # 5) Clean the values of the given num argument
        max <- max(Outcome_Subset_Sorted$rank)
        # finds the maximum value in the rank column
        
        if(num == "best"){num <- 1}
        if(num == "worst"){num <- max}
        
        # 6) Return the name of the hospital in the given state with the given rank
        desired_hospital <- Outcome_Subset_Sorted[num, 1]
        desired_hospital
}