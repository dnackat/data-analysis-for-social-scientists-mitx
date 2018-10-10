#HW5

rm(list = ls())
library(ggplot2)
library(tidyverse)

comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}

pois = function(lambda) {
  exp(-lambda) + lambda*exp(-lambda)
}