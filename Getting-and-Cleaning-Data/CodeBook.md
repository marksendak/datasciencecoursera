## This file explains the variables and transformations in the run_analysis.R program

I labeled the single variable in the y_test and y_train data as "Activity" and the single variable in the subject_test and subject_train data as "Subject." Within the data sets y_test and y_train, I assigned the 6 factor levels the names included in the file "activity_labels.txt". For the data sets X_test and X_train, I labeled the 561 variables with the values included in the file "features.txt".

After merging the three training data sets and test data sets, I subsetted the variables by selecting for variables that ended with "-mean()" and "-sd()". This selected 66 variables from the 561 total.

Following is a list of the 66 variables I selected:

 [1] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
 [3] "tBodyAcc-mean()-Z"           "tBodyAcc-std()-X"           
 [5] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
 [7] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"       
 [9] "tGravityAcc-mean()-Z"        "tGravityAcc-std()-X"        
[11] "tGravityAcc-std()-Y"         "tGravityAcc-std()-Z"        
[13] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
[15] "tBodyAccJerk-mean()-Z"       "tBodyAccJerk-std()-X"       
[17] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
[19] "tBodyGyro-mean()-X"          "tBodyGyro-mean()-Y"         
[21] "tBodyGyro-mean()-Z"          "tBodyGyro-std()-X"          
[23] "tBodyGyro-std()-Y"           "tBodyGyro-std()-Z"          
[25] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
[27] "tBodyGyroJerk-mean()-Z"      "tBodyGyroJerk-std()-X"      
[29] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
[31] "tBodyAccMag-mean()"          "tBodyAccMag-std()"          
[33] "tGravityAccMag-mean()"       "tGravityAccMag-std()"       
[35] "tBodyAccJerkMag-mean()"      "tBodyAccJerkMag-std()"      
[37] "tBodyGyroMag-mean()"         "tBodyGyroMag-std()"         
[39] "tBodyGyroJerkMag-mean()"     "tBodyGyroJerkMag-std()"     
[41] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
[43] "fBodyAcc-mean()-Z"           "fBodyAcc-std()-X"           
[45] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
[47] "fBodyAccJerk-mean()-X"       "fBodyAccJerk-mean()-Y"      
[49] "fBodyAccJerk-mean()-Z"       "fBodyAccJerk-std()-X"       
[51] "fBodyAccJerk-std()-Y"        "fBodyAccJerk-std()-Z"       
[53] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
[55] "fBodyGyro-mean()-Z"          "fBodyGyro-std()-X"          
[57] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
[59] "fBodyAccMag-mean()"          "fBodyAccMag-std()"          
[61] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"  
[63] "fBodyBodyGyroMag-mean()"     "fBodyBodyGyroMag-std()"     
[65] "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()"

To make the tiny data set, for each of the 30 patients, I calculated the average value for all 66 variables across each of the 6 activities. For example, patient 1 had an average value for "tBodyAcc-mean()-X" calculated for each of the 6 activities. Thus, the tidy data set includes a total of 180 observations (6 for each of the 30 patients). The number of columns in the tidy data set is 68, for "Subject", "Activity", plus the 66 measure variables. 