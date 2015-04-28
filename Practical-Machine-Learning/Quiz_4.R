################# Question 1
library(ElemStatLearn)
library(caret)
library(data.table)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

# Fit (1) a random forest predictor relating the factor variable y to the remaining variables
modFit1 <- train(y ~ ., data = vowel.train, method = "rf", prox = TRUE)

# (2) a boosted predictor using the "gbm" method
modFit2 <- train(y ~ ., data = vowel.train, method = "gbm")

# accuracies for the two approaches on the test data set
# Random Forest
pred1 <- predict(modFit1, vowel.test)
confusionMatrix(pred1, vowel.test$y)$overall
# RF accuracy is ~60%
# GBM
pred2 <- predict(modFit2, vowel.test)
confusionMatrix(pred2, vowel.test$y)$overall
# GBM accuracy is 53%

# What is the accuracy among the test set samples where the two methods agree?
predAgree <- data.frame(predRF = pred1, predGBM = pred2, y = vowel.test$y)
predAgree$Agree[predAgree$predRF == predAgree$predGBM] <- 1
predAgree$Agree[predAgree$predRF != predAgree$predGBM] <- 0

# Convert to data table
predAgree <- as.data.table(predAgree)
predAgree[, Agree_Correct := 0][Agree == 1 & predGBM == y, Agree_Correct := 1]

sum(predAgree$Agree_Correct)/sum(predAgree$Agree)
# 65% of cases where the classifiers agree correctly classify the observation

################# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

# predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model
modFitRF <- train(diagnosis ~ ., data = training, method = "rf")
modFitGBM <- train(diagnosis ~ ., data = training, method = "gbm")
modFitLDA <- train(diagnosis ~ ., data = training, method = "lda")

predRF <- predict(modFitRF, testing)
predGBM <- predict(modFitGBM, testing)
predLDA <- predict(modFitLDA, testing)

# Combine predictions
predDF <- data.frame(predRF, predGBM, predLDA, diagnosis = testing$diagnosis)

# Stack the predictions together using random forests ("rf")
comboModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
predCombo <- predict(comboModFit, testing)

# What is the resulting accuracy on the test set?
confusionMatrix(predCombo, testing$diagnosis) # Accuracy of 80.5%

# Is it better or worse than each of the individual predictions?
confusionMatrix(predRF, testing$diagnosis) # 76.8%
confusionMatrix(predGBM, testing$diagnosis) # 79.3%
confusionMatrix(predLDA, testing$diagnosis) # 76.8%

################# Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

# fit a lasso model to predict Compressive Strength
modFit <- train(CompressiveStrength ~ ., method = "lasso", data = training)

# Which variable is the last coefficient to be set to zero as the penalty increases?
quartz()
plot.enet(modFit$finalModel, xvar = "penalty", use.color = TRUE)
dev.off()
# Cement is the last variable to drop to zero

################# Question 4
library(data.table)
library(lubridate)
library(forecast)
dat <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training time series
modFit <- bats(tstrain)

# forecast this model for the remaining time points
pred <- forecast(modFit, level = 95, h = nrow(testing))
quartz()
plot(pred)
dev.off()

# For how many of the testing points is the true value within the 95% prediction interval bounds?
library(data.table)
predDT <- as.data.table(pred, keep.rownames )
setnames(predDT, names(predDT), gsub(" ", ".", names(predDT)))

accuracyDF <- as.data.frame(cbind(predDT$Hi.95, testing$visitsTumblr))

sum(accuracyDF$V2 < accuracyDF$V1) / nrow(accuracyDF)
# 96% of real values are within 95% of the forecast prediction

################# Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)

# fit a support vector machine using the e1071 package to predict Compressive Strength using the default settings
modFit <- svm(CompressiveStrength ~ ., data = training)

# Predict on the testing set
pred <- predict(modFit, testing)

# What is the RMSE?
accuracy(pred, testing$CompressiveStrength)
# 6.71