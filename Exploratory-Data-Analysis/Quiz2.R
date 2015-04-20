### This code is my work for Quiz 2 of Coursera's "Exploratory Data Analysis
### Date: June 13, 2014
### Author: Mark Dakkak

################## Question 2 ##################
library(nlme)
library(lattice)

data <- BodyWeight
class(BodyWeight$Diet)

xyplot(weight ~ Time | Diet, BodyWeight)

################## Question 2 ##################
# panel.lmline(x,y) is equivalent to panel.abline(lm(y~x))
        # ref: http://stat.ethz.ch/R-manual/R-devel/library/lattice/html/panel.functions.html

################## Question 5 ##################
# trellis.par.set() is a function used to query, display and modify graphical parameters for fine control of Trellis displays
        # ref: http://stat.ethz.ch/R-manual/R-devel/library/lattice/html/trellis.par.get.html

################## Question 7 ##################
library(datasets)
library(ggplot2)

data(airquality)

airquality$Month <- as.factor(airquality$Month)

qplot(Wind, Ozone, data = airquality, color=Month)
qplot(Wind, Ozone, data = airquality, facets=.~Month)

class(airquality$Month)
airquality = transform(airquality, Month = factor(Month))
## Quick way to transform the variable Month in dataset airquality to a factor
class(airquality$Month)

qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

################## Question 9 ##################
library(ggplot2)
movies <- movies
g <- ggplot(movies, aes(votes, rating))
print(g)
## There's no layers to the plot yet :-(

g + geom_point()

################## Question 10 ##################
qplot(votes, rating, data = movies)

# Try to add smoother
qplot(votes, rating, data = movies) + geom_smooth()