#### This code is for Quiz 1 of the Coursera course "Regression Models"
#### Date: August 11, 2014
#### Author: Mark Dakkak

######### Q1)
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

######### Q2)
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

x %*% y
        ## 2.9227
x %*% x
        ## 3.5373

(x %*% y) / (x %*% x)

######### Q3)
library(datasets)
data(mtcars)

names(mtcars)
plot(mtcars$wt,mtcars$mpg)
abline(coef(lm(mtcars$mpg ~ mtcars$wt))[1],coef(lm(mtcars$mpg ~ mtcars$wt))[2])
lm(mtcars$mpg ~ mtcars$wt)

######### Q6)
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x[1]-mean(x))/sd(x)

######### Q7)
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

plot(x, y, pch=19, xlim=c(0,1), ylim=c(-2,2))
abline(coef(lm(y~x))[1],coef(lm(y~x))[2])
lm(y ~ x)

######### Q9)
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
