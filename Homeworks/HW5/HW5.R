#HW5

rm(list = ls())
library(ggplot2)
library(tidyverse)

# Function to calculate n choose x
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}

# Poisson distribution problem
pois = function(lambda) {
  exp(-lambda) + lambda*exp(-lambda)
}

# For loop 
coin <- c("H", "T")
toss <- c() # Create an empty vector

for (i in 1:100) {
  toss[i] = sample(x = coin, size = 1)
}

# Another example
marital <- c("married", "unmarried")
income <- 1:4
states <- c("Washington", "Massachussetts", "California")
results <- matrix(nrow = 100, ncol = 3, data = NA)
colnames(results) <- c("marital", "income", "state")
head(results)

for (i in 1:100) {
  results[i, 1] <- sample(marital, size = 1)
  results[i, 2] <- sample(income, size = 1)
  results[i, 3] <- sample(states, size = 1)
}

# Apply function
apply(X = results, MARGIN = 2, FUN = table)
