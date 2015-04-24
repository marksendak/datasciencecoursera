#### Summary: Practical machine learning project
#### Date: April 19, 2015
#### Author: Mark Dakkak

############################ Load packages ############################
library(data.table)
# Need 1.9.5, instructions here: https://github.com/Rdatatable/data.table/wiki/Installation
library(caret)
library(ggplot2)
library(rattle)
library(rpart)
library(rpart.plot)

############################ Load data ############################

## Put data in memory
training <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

############################ Clean data ############################

## Calculate missingness in training data
missing_training <- as.data.table(t(training[, lapply(.SD, function(x) sum(is.na(x) | x == "", na.rm = TRUE))]), keep.rownames = TRUE)

## Confirm that variables are either entirely missing / entirely present
table(missing_training$V1)

## Drop columns that are entirely missing
missing <- missing_training[V1 != 0, rn]
training <- training[, setdiff(names(training), missing), with = FALSE]

## Drop columns from testint set
testing <- testing[, setdiff(names(testing), missing), with = FALSE]

############################ Split training data ############################

## Convert data tables to data frames
training <- as.data.frame(training)
testing <- as.data.frame(testing)

## Split training set
inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
train <- training[inTrain,]
validate <- training[-inTrain,]

## Dimensions of training and testing sets
dim(train)
# 13,737 rows
dim(test)
# 5,885 rows

############################ Identify covariates with zero covariance ############################
nsv <- nearZeroVar(train, saveMetrics = TRUE)
# Identifies the variables that have nearly zero variability

nsv <- nsv[order(-nsv$freqRatio),]
# Reorder data frame by frequency ratio (the ratio of frequencies for the most common value over the second most common value)

head(nsv)
# Returns a data frame

## Show the number of variables with 0 variance or near-zero variance
sum(nsv$zeroVar)
# None with zero variance
sum(nsv$nzv)
# One variable, which is new_window

## Remove new_window from data sets
train <- train[,setdiff(names(train),"new_window")]
validate <- validate[,setdiff(names(validate), "new_window")]
testing <- testing[,setdiff(names(testing), "new_window")]

############################ Remove V1, user_name, and timestamp variables ############################

train <- train[, setdiff(names(train), c(grep("time|window", names(train), value = TRUE), "V1", "user_name"))]
validate <- validate[, setdiff(names(validate), c(grep("time|window", names(validate), value = TRUE), "V1", "user_name"))]
testing <- testing[, setdiff(names(testing), c(grep("time|window", names(testing), value = TRUE), "V1", "user_name"))]

############################ Simple exploratory data analysis on predictors ############################

## Change class of classe variable
train$classe <- as.factor(train$classe)
validate$classe <- as.factor(validate$classe)

## Feature plot
# Predictors 1-10
featurePlot(x = train[, 7:10], y = train$classe, plot = "pairs")

########################################################
############################ Data modeling ############################
########################################################

############################ Random Forest ############################

## Use 5-fold cross validation
controlRF <- trainControl(method = "cv", 5)

## Run model
modelRF <- train(classe ~ ., data = train, method = "rf", trControl = controlRF, ntree = 250)
# Build 250 trees

## Examine model output
modelRF

############################ Predict on validation test set

predictRF <- predict(modelRF, validate)
confusionMatrix(validate$classe, predictRF)

## Calculate accuracy measures
accuracy <- postResample(predictRF, validate$classe)
accuracy
# Accuracy (the fraction of correct predictions) = 99.30%
# Kappa (measure of concordance) = 0.9911

## Calculate out-of-sample accuracy
oose <- (1-as.numeric(confusionMatrix(validate$classe, predictRF)$overall[1]))
oose
# Estimated out of sample error is 0.69%

############################ Predict 20 test observations
result <- predict(modelRF, testing[, setdiff(names(testing), c("problem_id"))])
result

############################ Decision Tree ############################

## Make model
treeModel <- rpart(classe ~ ., data = train, method = "class")

## Plot fancy tree
quartz()
fancyRpartPlot(treeModel)
dev.off()

## Predict validation test set
predictTree <- predict(treeModel, validate, type = "class")

## Confusion matrix
confusionMatrix(predictTree, validate$classe)
# Much lower accuracy

## Calculate accuracy measures
accuracyTree <- postResample(predictTree, validate$classe)
accuracyTree
# Accuracy (the fraction of correct predictions) = 75.44%
# Kappa (measure of concordance) = 0.6882

## Calculate out-of-sample accuracy
ooseTree <- (1-as.numeric(confusionMatrix(validate$classe, predictTree)$overall[1]))
ooseTree
# Estimated out of sample error is 24.55%

############################ Predict 20 test observations
resultTree <- predict(treeModel, testing[, setdiff(names(testing), c("problem_id"))])
resultTree

