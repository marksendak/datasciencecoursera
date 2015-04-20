#### This code is for Quiz 4 of the Coursera course "Regression Models"
#### Date: August 30, 2014
#### Author: Mark Dakkak

######### Q1)
install.packages("MASS")
library(MASS)
data(shuttle)
summary(shuttle)

## Fix variables
shuttle$use <- factor(shuttle$use)
shuttle$wind <- factor(shuttle$wind)

glm1 <- glm(use ~ wind, data = shuttle, family = "binomial")
summary(glm1)

exp(summary(glm1)$coefficients[2,1])
        # 0.9686906

######### Q2)
glm2 <- glm(use ~ wind + magn, data = shuttle, family = "binomial")
summary(glm2)

exp(summary(glm2)$coefficients[2,1])
        # 0.9684981

######### Q3)
q3data <- shuttle
q3data$use <- factor(q3data$use, levels = c("noauto", "auto"))

levels(q3data$use)
levels(shuttle$use)

glm3a <- glm(use ~ wind, data = shuttle, family = "binomial")
glm3b <- glm(use ~ wind, data = q3data, family = "binomial")

summary(glm3a)$coef
summary(glm3b)$coef

######### Q4)
library(datasets)
data(InsectSprays)
head(InsectSprays)

## Clean variables
class(InsectSprays$spray)
levels(InsectSprays$spray)

## Regression
glm4 <- glm(count ~ (spray-1), data = InsectSprays, family = "poisson")
summary(glm4)

exp(summary(glm4)$coef[1,1])/exp(summary(glm4)$coef[2,1])

mean(InsectSprays[InsectSprays$spray == "A",1])

######### Q5)
count <- round(rnorm(20, mean = 50, sd = 3), 0)
x <- rep(rnorm(10, mean = 15, sd = 6), times = 2)
t <- rep(1:10, times = 2)
q5data <- data.frame(cbind(count,x,t))

glm5a <- glm(count ~ x + offset(t), data = q5data, family = "poisson")

t2 <- log(10) + t
glm5b <- glm(count ~ x + offset(t2), family = poisson)

summary(glm5a)$coef
summary(glm5b)$coef

######### Q6)
## reference: http://www.r-bloggers.com/r-for-ecologists-putting-together-a-piecewise-regression/
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

plot(x,y)

## Find break point
breaks <- x[which(x >= -2 & x <= 2)]
        ## Estimate that the break point is between -2 and +2

mse <- numeric(length(breaks))
for(i in 1:length(breaks)){
        piecewise1 <- lm(y ~ x*(x < breaks[i]) + x*(x>=breaks[i]))
        mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse)
        ## Calculate the mean squared errors for the range of possible breakpoints

plot(x[which(x >= -2 & x <= 2)], mse)
        ## Plot the MSE for all potential break points
breaks[which(mse==min(mse))]
        ## Find that the minimum MSE is at x = 0

piecewise2 <- lm(y ~ x*(x < 0) + x*(x > 0))
summary(piecewise2)

knot = 0
lhs <- function(x) ifelse(x < 0,0-x,0) # basis function 1 (lhs = left hockey stick)
rhs <- function(x) ifelse(x > 0,x-0,0) # basis function 2 (rhs = right hockey stick)

glm6 <- lm(y ~ lhs(x) + rhs(x))
summary(glm6)
