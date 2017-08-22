#### Assignment: Part 2 of Week 4
#### Course: Advanced R Programming
#### Speciaization: Mastering Software Deveopment in R
#### Date Created: July 21, 2017
#### Author: Mark Sendak

############################ Build classes ############################

## longitudinal data class
make_LD <- function(x){
    ## Set the class name
    structure(x, class = c("LongitudinalData","data.frame"))
}

## subject class
make_Subject <- function(x,subject_id){
    ## Set the class name
    structure(list(Data = x, Subject_ID = subject_id), class = c("Subject","data.frame"))
}

## visit class
make_Visit <- function(x,subject_id,visit_num){
    ## Set the class name
    structure(list(Data = x, Subject_ID = subject_id, Visit_Num = visit_num), class = c("Visit","data.frame"))
}

## room class
make_Room <- function(x,subject_id,visit_num,room_name){
    ## Set the class name
    structure(list(Data = x, Subject_ID = subject_id, Visit_Num = visit_num, Room_Name = room_name), class = c("Room","data.frame"))
}

############################ Build functions ############################

## subject: a generic function for extracting subject-specific information
subject <- function(data, subject_id) UseMethod("subject")
subject.LongitudinalData <- function(data, subject_id){
    out <- filter(data, id == subject_id)
    make_Subject(out, subject_id)
}


## visit: a generic function for extracting visit-specific information
visit <- function(x, visit_num) UseMethod("visit")
visit.Subject <- function(x, visit_num){
    ## Extract data
    data <- x$Data
    
    ## Build filtered data
    out <- filter(data, visit == visit_num)
    out <- data.frame(out)
    
    ## Make visit class object
    make_Visit(out,x$Subject_ID,visit_num)
}

## room: a generic function for extracting room-specific information
room <- function(x, room_name) UseMethod("room")
room.Visit <- function(x, room_name){
    ## Extract data
    data <- x$Data
    
    ## Build filtered data
    out <- filter(data, room == room_name)
    out <- data.frame(out)
    
    ## Make visit class object
    make_Room(out,x$Subject_ID,x$Visit_Num,room_name)
}

############################ Print methods ############################
## For longitudinal data class
print.LongitudinalData <- function(x) {
    cat("Longitudinal dataset with",length(unique(x$id)),"subjects")
    invisible(x)
}

## For subject class
print.Subject <- function(x) {
    if(nrow(x$Data) == 0) return(NULL)
    else {
        cat("Subject ID:",x$Subject_ID)
        invisible(x)
    }
}

## For room class
print.Room <- function(x) {
    cat("ID: ",x$Subject_ID,"\n","Visit: ",x$Visit_Num,"\n","Room: ",x$Room_Name,sep = "")
}

############################ Summary functions ############################

## Subject
summary.Subject <- function(x) {
    ## Pull out data attribute
    data <- x$Data
    
    ## Build new data attribute
    out <- group_by(data,visit,room) %>%
        summarise(value = mean(value)) %>% 
        spread(room,value)
    out <- data.frame(out)
    
    ## Return new summary subject object
    object <- structure(list(Data = out, Subject_ID = x$Subject_ID), class = c("Summary_Subject","data.frame"))
    object
}

## Room
summary.Room <- function(x) {
    ## Pull out data attribute
    data <- x$Data
    
    ## Build new data attribute
    out <- summary(data[["value"]])
    
    ## Return new summary subject object
    object <- structure(list(Data = out, Subject_ID = x$Subject_ID, Visit_Num = x$Visit_Num, Room_Name = x$Room_Name), class = c("Summary_Room","data.frame"))
    object
}

############################ Print for summary functions ############################
    ## NOTE: When I print summary results, I include all meta-data about the object, which is different than the assignment. I do this so that you can understand the results if you only see the output. Otherwise, when you print the pollutant levels for a specific room, all you have is the Subject ID. For that case, I also include the visit number and the room name.

## Summary subject
print.Summary_Subject <- function(x){
    cat("Subject ID:",x$Subject_ID,"\n")
    print(x$Data)
}

## Summary room
print.Summary_Room <- function(x){
    cat("ID: ",x$Subject_ID,"\n","Visit: ",x$Visit_Num,"\n","Room: ",x$Room_Name,"\n",sep = "")
    print(x$Data)
}