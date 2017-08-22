#### Assignment: Part 1 of Week 4
#### Course: Advanced R Programming
#### Speciaization: Mastering Software Deveopment in R
#### Date Created: July 21, 2017
#### Author: Mark Sendak

############################ prepare environment ############################
library(dplyr)
library(tidyr)
library(purrr)
library(microbenchmark)
library(ggplot2)

############################ For loop ############################

Factorial_loop <- function(n) {
    ## Check for positive integer
    if(!(n%%1==0)) stop("n is not an integer")
    if(n < 0) stop("n is not a positive integer")
    
    ## Case for n = 0
    if(n == 0) return(1)
    
    ## Initiaize a variabe to capture multipication
    mult <- n
    
    ## for loop for n > 0
    for (i in seq(1:(n-1))){
        mult <- mult * (n - i)
    }
    return(mult)
}

## Testing
Factorial_loop(0)
Factorial_loop(-1)
Factorial_loop(5)
Factorial_loop(10)

############################ Factorial Reduce ############################

Factorial_reduce <- function(n) {
    ## Check for positive integer
    if(!(n%%1==0)) stop("n is not an integer")
    if(n < 0) stop("n is not a positive integer")
    
    ## Case for n = 0
    if(n == 0) return(1)
    
    ## Reduce function
    mult <- reduce(c(n:1), function(x,y){
        x*y
    })
    return(mult)
}

## Testing
Factorial_reduce(0)
Factorial_reduce(-1)
Factorial_reduce(5)
Factorial_reduce(10)

############################ Factorial recursion function ############################
    ## NOTE: I use the function name Factorial_recursion instead of Factorial_func to remind mysef what differentiates this function from others in benchmarking

Factorial_recursion <- function(n) {
    ## Check for positive integer
    if(!(n%%1==0)) stop("n is not an integer")
    if(n < 0) stop("n is not a positive integer")
    
    ## Case for n = 0
    if(n == 0) return(1)
    
    ## Recursion
    if(n == 1){
        1
    } else{
        Factorial_recursion(n - 1) * n
    }
}

## Testing
Factorial_recursion(0)
Factorial_recursion(-1)
Factorial_recursion(1)
Factorial_recursion(5)
Factorial_recursion(10)

############################ Factorial memoization ############################

## Caching the Factorial resuts within the function
Factorial_mem <- function(n) {
    ## Check for positive integer
    if(!(n%%1==0)) stop("n is not an integer")
    if(n < 0) stop("n is not a positive integer")
    
    ## Cases unti n > 2
    if(n == 0){
        return(1)
    } 

    fact_tbl <- integer(n)
    fact_tbl[1:2] <- c(1,2)
    
    if(fact_tbl[n] != 0){
        fact_tbl[n]
    } else{
        fact_tbl[n] <- n * Factorial_mem(n-1)
        fact_tbl[n]
    }
}

## Testing
Factorial_mem(0)
Factorial_mem(-1)
Factorial_mem(1)
Factorial_mem(2)
Factorial_mem(3)
Factorial_mem(5)
Factorial_mem(10)

## Caching the Factorial resuts in the gobal environment
fact_tbl <- integer(300)
fact_tbl[1:2] <- c(1,2)

Factorial_mem2 <- function(n) {
    ## Check for positive integer
    if(!(n%%1==0)) stop("n is not an integer")
    if(n < 0) stop("n is not a positive integer")
    
    ## Cases unti n > 2
    if(n == 0){
        return(1)
    } 
    
    if(fact_tbl[n] != 0){
        fact_tbl[n]
    } else{
        fact_tbl[n] <<- n * Factorial_mem2(n-1)
        fact_tbl[n]
    }
}

## Testing
Factorial_mem2(0)
Factorial_mem2(-1)
Factorial_mem2(1)
Factorial_mem2(2)
Factorial_mem2(3)
Factorial_mem2(5)
Factorial_mem2(10)

############################ Microbenchmark resuts ############################

## First, reinitialize the reference table to accurately measure memoization. Also, make sure go in increasing order when testing to update table each iteration. Otherwise, table is only updated on first instance.
fact_tbl <- integer(300)
fact_tbl[1:2] <- c(1,2)

## Run on 10, 20, 30, 100, 200, 300
microbench_Factorial <- function(n){
    microbenchmark(
        loop = Factorial_loop(n),
        Reduce = Factorial_reduce(n),
        Recursion = Factorial_recursion(n),
        Memoization = Factorial_mem(n),
        Memoization_Global = Factorial_mem2(n)
    )
}

mbm_10 <- microbench_Factorial(10)
mbm_20 <- microbench_Factorial(20)
mbm_30 <- microbench_Factorial(30)
mbm_100 <- microbench_Factorial(100)
mbm_200 <- microbench_Factorial(200)
mbm_300 <- microbench_Factorial(300)

autoplot(mbm_10)
autoplot(mbm_20)
autoplot(mbm_30)
autoplot(mbm_100)
autoplot(mbm_200)