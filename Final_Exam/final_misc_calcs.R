# Misc calcs for final exam 

# Preliminaries
rm(list = ls())
library(tidyverse)

nCk <- function(n,k) {
  factorial(n)/(factorial(k)*factorial(n-k))
}

# Calc prob Q1, part 5 (world cup ties)
prob <- 0
for (i in 1:4) {
  j <- i-1
  prob <- prob + (nCk(3,j)*((0.7)^j)*((0.3)^(3-j)))^2
}
print(prob)

# Prob. that team 1 will win, part 6
prob_3_2 <- (nCk(3,1)*(0.7)*(0.3^2))*(nCk(3,0)*(0.3^3))
prob_4_2 <- (nCk(3,2)*((0.7)^2)*0.3)*(nCk(3,0)*(0.3^3))
prob_4_3 <- (nCk(3,2)*((0.7)^2)*0.3)*(nCk(3,1)*0.7*(0.3^2))
prob_5_2 <- (nCk(3,3)*((0.7)^3))*(nCk(3,0)*(0.3^3))
prob_5_3 <- (nCk(3,3)*((0.7)^3))*(nCk(3,1)*0.7*(0.3^2))
prob_5_4 <- (nCk(3,3)*((0.7)^3))*(nCk(3,2)*((0.7)^2)*0.3)

prob_1_win <- prob_3_2 + prob_4_2 + prob_4_3 + prob_5_2 + prob_5_3 + prob_5_4
print(prob_1_win)