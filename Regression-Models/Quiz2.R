#### This code is for Quiz 2 of the Coursera course "Regression Models"
#### Date: August 11, 2014
#### Author: Mark Dakkak

######### Q1)
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y ~ x)
summary(fit)$coefficients

######### Q2)
beta1 <- cor(y,x) *sd(y)/sd(x)
beta0 <- mean(y) - beta1*mean(x)
e <- y - beta0 - beta1*x
n <- length(y)
sigma <- sqrt(sum(e^2)/(n-2))
sigma

######### Q3)
library(UsingR); data(mtcars)
fit <- lm(mpg ~ wt, mtcars)

summary(fit)$coefficients
plot(mtcars$wt, mtcars$mpg)

mean(mtcars$wt)
newdata <- data.frame(wt = mean(mtcars$wt))
        ## Must enter the new data in a data frame

predict(fit, newdata, interval = ("confidence"))
        ## Gives a 95% confidence interval for the new data element

######### Q5)
newdata <- data.frame(wt = 3)
predict(fit, newdata, interval = ("prediction"))

######### Q6)
y <- mtcars$mpg
x <- mtcars$wt/2
fit <- lm(y ~ x)
summary(fit)$coefficients

sumCoef <- summary(fit)$coefficients
sumCoef[2,1] + c(-1,1)*qt(.975, df=fit$df)*sumCoef[2,2]

######### Q9)
y <- mtcars$mpg
x <- mtcars$wt

plot(x,y)

## Part A - regression with only intercept
Beta0_A <- 16.873
e_A <- y - (Beta0_A)

## Part B - regression with intercept and slope
fit <- lm(y ~ x)
sumCoef <- summary(fit)$coefficients
beta1 <- sumCoef[2,1]
beta0 <- sumCoef[1,1]
e_B <- y - (beta0 + (beta1*x))

## Part C - calculate ratio
sum((y - (beta0 + (beta1*x)))^2)/sum((y - (Beta0_A))^2)

## 
fit_A <- lm(y ~ x)
anova(fit_A)
R2_A <- (847.73/(847.73+278.32))
1-R2_A

fit_B <- lm(y ~ offset(x))
anova(fit_B)
R2_B <- (1473/(847.73+278.32))

summary(fit_B)
