# This code is for programming assignment 3 in the Coursera course "R Programming
# Author: Mark Dakkak
# Date: June 1, 2014

setwd("~/datasciencecoursera/rprog-data-ProgAssignment3-data")

### Part 1: Plot the 30-day mortality rates for heart attack ###
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

ncol(outcome)
        # Tells you the number of columns (variables) in the dataset, 46
nrow(outcome)
        # Tells you the number of rows (observations) in the dataset, 4706
names(outcome)
        # Tells you the names of the 46 columns (variables)

outcome[, 11] <- as.numeric(outcome[, 11])
        # Pulls out the 11th column of the outcome database (30 day mortality from heart attacks) and assigns it to a new vector
hist(outcome[, 11])

### Part 2: Finding the best hospital in a state ###
        # Function takes 2 arguments: 2-character abbreviated name of a state and an outcome name. The function returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings

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

top.20 <- function(state, outcome) {
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
        
        # 4) Return the name of the 20 hospitals with the best outcome
        top_20 <- Outcome_Subset_Sorted[1:20, 1]
        top_20
}

# Test cases for the function "best"
best("TX" ,"heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

# Where does Duke Stand?
best("NC" ,"heart attack")
best("NC" ,"heart failure")
best("NC" ,"pneumonia")

top.20("NC" ,"heart attack")
top.20("NC" ,"heart failure")
top.20("NC" ,"pneumonia")

### Part 3: Ranking hospitals by outcome in a state ###
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

outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
names(outcome_data)[17] <- "Outcome"
outcome_subset <- subset(outcome_data, State == "TX" & Outcome != "Not Available", select = c(2, 7, 17))
outcome_subset[, 3] <- as.numeric(outcome_subset[, 3])
outcome.order <- with(outcome_subset, order(outcome_subset$Outcome, outcome_subset$Hospital.Name))
Outcome_Subset_Sorted <- outcome_subset[outcome.order, ]
Outcome_Subset_Sorted$rank <- rank(Outcome_Subset_Sorted$Outcome, ties.method = "first")
max <- max(Outcome_Subset_Sorted$rank)
Outcome_Subset_Sorted[max, 1]

# Test cases for rankhospital()
rankhospital("TX", "heart failure", 4)
rankhospital("TX", "heart failure", 294)
rankhospital("TX", "heart failure", "worst")
rankhospital("TX", "heart failure", 300)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

### Part 4: Ranking hospitals in all states ###
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

# Test cases
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
