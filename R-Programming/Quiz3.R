#1
library(datasets)
data(iris)

?iris
head(iris)

names(iris)
# the names() function gives the names of all the variables in the data set "iris"

# Method 1: create a subset and then calculate on subset
iris.subset <- iris[iris$Species == "virginica", ]
# creates a subset of iris for all observations for the flower "virginica"
mean(iris.subset$Sepal.Length)

# Method 2: use aggregate() function
aggregate(iris$Sepal.Length, list(Species = iris$Species), mean)
        # reference: http://davetang.org/muse/2013/05/22/using-aggregate-and-apply-in-r/

# Method 3: use by function
by(iris[, 1], iris$Species, mean)
        # reference: http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/

#2
apply(iris[, 1:4], 2, mean)
        # reference: http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/

#3
library(datasets)
data(mtcars)

?mtcars
head(mtcars)

tapply(mtcars$mpg, mtcars$cyl, mean)
        # reference: http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/

#4
a <- tapply(mtcars$hp, mtcars$cyl, mean)
# Create a vector of dimensions (3,1) that contains the average horsepower by number of cylinders

b <- (a[3] - a[1])
# Calculate the difference between the horsepower of cars with 8 cylinders and cars with 4 cylinders

#5
debug(ls)
ls
?ls
ls(a)
