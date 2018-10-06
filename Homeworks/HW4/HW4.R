# HW4

##### Random draws #####

# Preliminaries
rm(list = ls())
library("utils")
library("tidyverse")
library(dplyr)
library(ggplot2)
library(stringr)

u <- runif(1000)

hist(qnorm(u), breaks=50)

##### Auctions #####

# Uniform Valuations - posted price model
number_of_bidders <- 2
N <- number_of_bidders
V <- 10000

set.seed(5)
valuations <- matrix(runif(N*V, min = 0, max = 1), nrow = V)

maximum_valuation <- apply(valuations, 1, max)
optimal_price <- 1/((N+1)^(1/N))
expected_revenue <- (N/(N+1))*(1/((N+1)^(1/N)))

revenue <- optimal_price*(maximum_valuation >= optimal_price)
print(mean(revenue))
print(expected_revenue)
