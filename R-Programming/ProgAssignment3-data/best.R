### Part 2: Finding the best hospital in a state ###
# Function takes 2 arguments: 2-character abbreviated name of a state and an outcome name. The function returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings
# Author: Mark Dakkak
# Date: June 1, 2014

best <- function(state, outcome) {
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
        
        ## Section 3 - Return hospital name in that state with lowest 30-day death rate
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
        
        # 4) Return the name of the hospital with the best outcome
        best_hospital <- Outcome_Subset_Sorted[1, 1]
        best_hospital
}