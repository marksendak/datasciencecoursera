## To help along the way: http://rstudio-pubs-static.s3.amazonaws.com/19452_2fd823509cb64054813867d90c02b34c.html
################# Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.5, list = FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

dim(training)
dim(testing)

################# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Add index
training$index <- seq_along(1:nrow(training))

## Turn continuous variables into categorical
library(Hmisc)
library(ggplot2)
summary(training)

# Cement
cutCement <- cut2(training$Cement, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutCement)

# BlastFurnaceSlag
cutBlastFurnaceSlag <- cut2(training$BlastFurnaceSlag, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutBlastFurnaceSlag)

# FlyAsh
cutFlyAsh <- cut2(training$FlyAsh, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutFlyAsh)

# Water
cutWater <- cut2(training$Water, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutWater)

# Superplasticizer
cutSuperplasticizer <- cut2(training$Superplasticizer, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutSuperplasticizer)

# CoarseAggregate
cutCoarseAggregate <- cut2(training$CoarseAggregate, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutCoarseAggregate)

# FineAggregate
cutFineAggregate <- cut2(training$FineAggregate, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutFineAggregate)

# Age
cutAge <- cut2(training$Age, g = 4)
qplot(index, CompressiveStrength, data = training, color = cutAge)

################# Question 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(ggplot2)

ggplot(concrete, aes(x = Superplasticizer)) + geom_density()
ggplot(training, aes(x = Superplasticizer)) + geom_density()

concrete$test <- log10(concrete$Superplasticizer)
ggplot(concrete, aes(x = test)) + geom_density()

################# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Subset to columns starting with "IL"
training_subset <- subset(training, select = c("diagnosis", grep("^IL", names(training), value = TRUE)))

## Perform PCA with caret, testing different numbers of PCs
# 7
preProc <- preProcess(training_subset[,-1], method = "pca", pcaComp = 7)
trainPC <- predict(preProc, training_subset[,-1])
modelFit7 <- train(training_subset$diagnosis ~ ., method = "glm", data = trainPC)
modelFit7

# 8
preProc <- preProcess(training_subset[,-1], method = "pca", pcaComp = 8)
trainPC <- predict(preProc, training_subset[,-1])
modelFit8 <- train(training_subset$diagnosis ~ ., method = "glm", data = trainPC)
modelFit8

## Perform PCA with caret to explain 90% of variance
preProc <- preProcess(training_subset[,-1], method = "pca", thresh = 0.90)
trainPC <- predict(preProc, training_subset[,-1])
    ## See that there are 9 principal components

################# Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Subset to columns starting with "IL"
training_IL <- subset(training, select = c("diagnosis", grep("^IL", names(training), value = TRUE)))

## Using predictors as they are
modFit_IL <- train(diagnosis ~ ., data = training_IL, method = "glm")
pred_IL <- predict(modFit_IL, testing)
confusionMatrix(pred_IL, testing$diagnosis)

## Using PCA of predictors with caret package
modFit_PC <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", data = training_IL, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
pred_PC <- predict(modFit_PC, testing)
confusionMatrix(pred_PC, testing$diagnosis)
