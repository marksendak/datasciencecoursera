#### Summary: Practical machine learning project
#### Date: April 19, 2015
#### Author: Mark Dakkak

############################ Load packages ############################
library(data.table)
library(caret)

############################ Load data ############################

setwd("/Users/sommpd10/Desktop/Programming Resources/datasciencecoursera/Practical-Machine-Learning/Course Project")

training <- fread("pml-training.csv")
names(training)
table(training$classe)
