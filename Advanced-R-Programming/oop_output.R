#### Assignment: Part 2 of Week 4
#### Course: Advanced R Programming
#### Speciaization: Mastering Software Deveopment in R
#### Date Created: July 21, 2017
#### Author: Mark Sendak

############################ prepare environment ############################
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)

## Read in data and functions
data <- read_csv("data/MIE.csv")
source("oop_code.R")

############### Run through code
x <- make_LD(data)
print(class(x))

print(x)

out <- subject(x,10)
print(out)

out <- subject(x,14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
