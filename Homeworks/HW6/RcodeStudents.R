#preliminaries
rm(list = ls())

#Real value of theta
theta <- 5
#sample size
n <- 25

#Generating 1000 samples of size n
simul <- 1000
sample <- matrix(runif(simul*n, max = theta), 
                 nrow = n)

thetahat <- (n+1)/n*apply(sample, 2, max) # max over 2nd column to get nth order stat

ll <- thetahat/(0.95^(1/n)*(n+1)/n) # lower limit
ul <- thetahat/(0.05^(1/n)*(n+1)/n) # upper limit
thetain <- (theta>=ll & theta<=ul) 
mean(thetain) # Prob that theta is within these bounds