# This file is my work for Quiz 4 of the Coursera course "R Programming"
# Date: June 1, 2014

# Question 1
set.seed(1)
rpois(5, 2)

# Description of Poisson Distribution
        # Suppose someone typically gets 4 pieces of mail per day on average. There will be, however, a certain spread: sometimes a little more, sometimes a little fewer, once in a while nothing at all. Given only the average rate, for a certain period of observations (pieces of mail per day, phonecalls per hour, etc.), and assuming that the process, or mix of processes, that produces the event flow is essentially random, the Poisson distribution specifies how likely it is that the count will be 3, or 5, or 10, or any other number, during one period of observation. That is, it predicts the degree of spread around a known average rate of occurrence.

# Question 5
set.seed(10)
x <- rbinom(10, 10, 0.5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e

# Description of binomial distribution
        # The binomial distribution is the discrete probability distribution of the number of successes in a sequence of n independent yes/no experiments, each of which yields success with probability p. 