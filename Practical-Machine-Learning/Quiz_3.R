################# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

## 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
inTrain <- segmentationOriginal$Case == "Train"
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[!inTrain,]

set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training)

# Print model
print(modFit$finalModel)

# Print tree
quartz()
plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)
dev.off()

## Classify test cases using the plotted tree
#a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2  --> PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 --> WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 --> PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 --> Can't classify 

################# Question 2
# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out cross validation?

# Small K: more bias, less variance
# In leave one out, K = the size of the sample

################# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

# Fit classification tree
modFit <- train(Area ~ ., method = "rpart", data = olive)

# New data to predict
newdata = as.data.frame(t(colMeans(olive)))

# Predict new value
predict(modFit, newdata = newdata)

# Plot tree
quartz()
plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)
dev.off()

################# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

# fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors

# Subset data
trainSA <- subset(trainSA, select = c("chd", "age", "alcohol", "obesity", "tobacco", "typea", "ldl"))
testSA <- subset(testSA, select = c("chd", "age", "alcohol", "obesity", "tobacco", "typea", "ldl"))

modelFit <- train(chd ~.,data=trainSA, method = "glm", family = "binomial")
modelFit$finalModel

# Calculate the misclassification rate for your model using this function and a prediction on the "response" scale
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

# Predictions for training set
predictionsTrain <- predict(modelFit, newdata=trainSA)
predictionsTrain

missClass(trainSA$chd, predictionsTrain)
# 0.27

# Predictions for test set
predictionsTest <- predict(modelFit, newdata=testSA)
predictionsTest

missClass(testSA$chd, predictionsTest)
# 0.31

################# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833.
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)

# Fit a random forest predictor relating the factor variable y to the remaining variables
modFit <- train(y ~ ., data = vowel.train, method = "rf", prox = TRUE)

# Calculate the variable importance using the varImp function in the caret package.
varImp(modFit)
