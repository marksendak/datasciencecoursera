### Part 4: Ranking hospitals in all states ###
# Author: Mark Dakkak
# Date: June 1, 2014

rankall <- function(outcome, num = "best") {
        ## Section 1 - Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Section 2 - Check that outcome argument is valid
        if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
                stop("invalid outcome")
        }
        
        ## Section 3 - For each state, find the hospital of the given rank
        # 1) Subset the data
        # a) Identify the column number
        col_num <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
        names(outcome_data)[col_num] <- "Outcome"
        # Renames the column "Outcome"
        # ref: http://rprogramming.net/rename-columns-in-r/
        
        # b) Make a dataset with three columns: Hospital Name, State, and Outcome. Drop all rows that are not for the given state 
        outcome_subset <- subset(outcome_data, Outcome != "Not Available", select = c(2, 7, col_num))
        # ref: http://www.ats.ucla.edu/stat/r/faq/subset_R.htm
        
        # 2) Sort by state, then Outcome rate, then hospital name
        outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
        # First convert the values in that column to numeric values
        
        outcome.order <- with(outcome_subset, order(outcome_subset$State, outcome_subset$Outcome, outcome_subset$Hospital.Name))
        # Sorts the data frame outcome_subset first by the number in the column "Outcome" and then by "Hospital.Name"
        # ref: http://www.dummies.com/how-to/content/how-to-sort-data-frames-in-r.html
        Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]        
        
        # 3) Split the cleaned data by state
        state_outcome_data <- split(Outcome_Subset_Sorted, Outcome_Subset_Sorted$State)
        
        # 4) Find the hospital from each state with the given rank        
        hospitals_by_rank <- sapply(state_outcome_data, function(x){
                x$rank <- rank(x$Outcome, ties.method = "first")
                max <- max(x$rank)
                if(num == "best"){num <- 1}
                if(num == "worst"){num <- max}
                
                x$Hospital.Name[num]
        })
        
        df <- data.frame(hospital = hospitals_by_rank)
        df$state <- row.names(df)
        
        df
}