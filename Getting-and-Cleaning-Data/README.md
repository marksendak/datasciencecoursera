## This is an explanation of the run_analysis.R code for the Course Project

We are given the URL of the zip file, which once decompressed contains three training data files and three test data files. The subject_train and subject_test files are vectors of the subject ID and the y_train and y_test files are vectors of activities.

The only library to load for the script is reshape2, which is used to melt and case the data set to create the tidy data output.

Read in the 3 training data files and 3 test data files with read.csv, making sure to indicate that there is no header, and for the "x" files, indicate that the separator is "".

To add the column naes to the X_train and X_test files, we use the labels that are included in the features.txt file. Pull out the 561 values in the features file and assign them to the column names of X_train and X_test.

The names of the factor levels of the activities are included in the activity_labels.txt file. Pull out the 6 values in this file and assign those values to the factor levels. 

To extract the 68 variables that are calculations of mean and standard deviation, use grep to search for "mean()" or "std()". To ensure that the parentheses are interpreted literally and not as special characters, use double backslashes.

To merge the data sets, use cbind to merge the 3 training data sets and cbind to merge the 3 test data sets. Afterwards, use rbind to merge the complete training data and the complete test data.

To create the tidy dataset, melt and then cast the data. Use Subject and Activity as the ID variables and every other remaining mean and standard deviation variable as measures. To cast the dataset correctly, use Subject + Activity, to make sure that averages are calculated for each activity for each subject.

The tidy data set is printed as a text file with a comma separator. This enables easy reading with the read.csv() function. In addition, excel can open the file if you indicate that the file is comma separated. Enjoy!