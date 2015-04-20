#### This code is for Quiz 3 of the Coursera course "Regression Models"
#### Date: August 20, 2014
#### Author: Mark Dakkak

######### Q1)
library(datasets)
data(mtcars)

## Convert cylinders to a factor
mtcars$cyl <- as.factor(mtcars$cyl)

## Fit model
fit <- lm(mpg ~ cyl + wt, data = mtcars)
summary(fit)

mycol=rainbow(8)
plot(mtcars$wt, mtcars$mpg, pch=19, col=mycol[mtcars$cyl])
abline(c(fit$coeff[1],fit$coeff[4]),col="red",lwd=3)
abline(c(fit$coeff[1] + fit$coeff[2] ,fit$coeff[4]),col="blue",lwd=3)
abline(c(fit$coeff[1] + fit$coeff[3] ,fit$coeff[4]),col="black",lwd=3)

######### Q2)
fit8 <- lm(mpg ~ cyl + wt, data = mtcars)
fit4 <- lm(mpg ~ cyl, data = mtcars)
summary(fit8)
summary(fit4)

######### Q3)
fit_NoInteract <- lm(mpg ~ cyl + wt, data = mtcars)
fit_Interact <- lm(mpg ~ cyl + wt + cyl*wt, data = mtcars)

summary(fit_NoInteract)
summary(fit_Interact)

anova(fit_Interact, fit_NoInteract)

######### Q4)
fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit)

######### Q5)
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)

plot(x, y)
abline(fit)

hatvalues(fit)

######### Q6)
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)

plot(x, y)
abline(fit)

dfbetas(fit)
