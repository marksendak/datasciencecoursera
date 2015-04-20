#### This code is for the Course Project for the Coursera course "Regression Models"
#### Date: August 21, 2014
#### Author: Mark Dakkak

################## Load data
library(datasets)
data(mtcars)

################## Fix variable types
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

################## Try models
fit1 <- lm(mpg ~ (cyl - 1) + wt + am, data = mtcars)
summary(fit1)

fit2 <- lm(mpg ~ wt, data = mtcars)
summary(fit2)

################## Plot cars by different types of transmission
plot(mtcars$wt, mtcars$mpg, pch = 19)
points(mtcars$wt, mtcars$mpg, pch = 19, col= ((mtcars$am == "0")*1 + 1))
        ## Black is for manual transmission and red is for automatic transmission

abline(h = mean(mtcars$mpg[mtcars$am == "1"]), col = "black", lwd = 3)
abline(h = mean(mtcars$mpg[mtcars$am == "0"]), col = "red", lwd = 3)


################## Do two linear models (with different everything) by transmission type
lm_Manual <- lm(mtcars$mpg[mtcars$am == "1"] ~ mtcars$wt[mtcars$am == "1"])
lm_Automatic <- lm(mtcars$mpg[mtcars$am == "0"] ~ mtcars$wt[mtcars$am == "0"])

### Plot two linear models
plot(mtcars$wt, mtcars$mpg, pch = 19)
points(mtcars$wt, mtcars$mpg, pch = 19, col= ((mtcars$am == "0")*1 + 1))
lines(mtcars$wt[mtcars$am == "1"], lm_Manual$fitted, col = "black", lwd=3)
lines(mtcars$wt[mtcars$am == "0"], lm_Automatic$fitted, col = "red", lwd=3)

################## Do two linear models (with same slope) by transmission type
lm_Parallel <- lm(mpg ~ wt + am, data = mtcars)
summary(lm_Parallel)
summary(lm_Parallel)$coefficients[3,4]

### Plot two linear models
plot(mtcars$wt, mtcars$mpg, pch = 19)
points(mtcars$wt, mtcars$mpg, pch = 19, col= ((mtcars$am == "0")*1 + 1))
abline(c(lm_Parallel$coeff[1], lm_Parallel$coeff[2]), col="red", lwd = 3)
abline(c(lm_Parallel$coeff[1] + lm_Parallel$coeff[3],lm_Parallel$coeff[2]), col="black", lwd = 3)
        ## See that lines are essentially the same. Transmission does not have a significant impact on the relationship between weight and mpg

################## Do two linear models (with different slopes) by transmission type
lm_Different <- lm(mpg ~ wt + am + wt * am, data = mtcars)
summary(lm_Different)

### Plot two linear models
plot(mtcars$wt, mtcars$mpg, pch = 19)
points(mtcars$wt, mtcars$mpg, pch = 19, col= ((mtcars$am == "0")*1 + 1))
abline(c(lm_Different$coeff[1], lm_Different$coeff[2]), col="red", lwd = 3)
abline(c(lm_Different$coeff[1] + lm_Different$coeff[3],lm_Different$coeff[2] + lm_Different$coeff[4]), col="black", lwd = 3)
        ## See that lines are have very different slopes and that the coefficients are significantly different

################## Full Model
lm1 <- lm(mpg ~ am, data = mtcars)
lm2 <- lm(mpg ~ wt + am, data = mtcars)
lm3 <- lm(mpg ~ wt + am + wt*am, data = mtcars)
lm4 <- lm(mpg ~ (cyl-1) + wt + am + wt*am, data = mtcars)
lm5 <- lm(mpg ~ hp + (cyl-1) + wt + am + wt*am, data = mtcars)
lm5b <- lm(mpg ~ drat + (cyl-1) + wt + am + wt*am, data = mtcars)
lm5c <- lm(mpg ~ disp + (cyl-1) + wt + am + wt*am, data = mtcars)


lm6 <- lm(mpg ~ drat + hp + (cyl-1) + wt + am + wt*am, data = mtcars)
lm7 <- lm(mpg ~ disp + drat + hp + (cyl-1) + wt + am + wt*am, data = mtcars)

anova(lm1, lm2, lm3, lm4, lm5, lm6, lm7)
anova(lm1, lm2, lm3, lm4, lm5b)
anova(lm1, lm2, lm3, lm4, lm5c)

################## Residual plot
lm4R_resid <- resid(lm4)

plot(resid(lm(am ~ wt, data = mtcars)), resid(lm(mpg ~ wt, data = mtcars)))
plot(mtcars$am, mtcars$mpg)

hatvalues(lm4)
hist(hatvalues(lm4))

dfbetas(lm4)[,6]
hist(dfbetas(lm4)[,6])