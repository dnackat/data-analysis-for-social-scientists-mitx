# Preliminaries
#-------------------------------------------------
#install.packages('perm')
library(perm)
library(modelr)
library(np)
library(tidyverse)
rm(list = ls())
setwd("/Users/floza/Dropbox (MIT)/14.310x 2018/Problem Sets/PSET 7")

# Questions 1 - 4
#-------------------------------------------------

#Q1
perms <- chooseMatrix(8,4)
A <- matrix(c(0.462, 0.731, 0.571, 0.923, 0.333, 0.750, 0.893, 0.692), nrow=8, ncol=1, byrow=TRUE)
treatment_avg <- (1/4)*perms%*%A
control_avg <- (1/4)*(1-perms)%*%A
test_statistic <- abs(treatment_avg-control_avg)
rownumber <- apply(apply(perms, 1, 
                         function(x) (x == c(0, 1, 0, 0, 0, 1, 1, 1))), 
                   2, sum)

#Q2
observed_test <- test_statistic[rownumber == 8]


#Q3-Q4
larger_than_observed <- (test_statistic >= observed_test)
#numbers in which the statistic exceeds the value in the observed date
sum(larger_than_observed)
df <- data.frame(perms,control_avg,treatment_avg,test_statistic)


# Question 5 - 6
#-------------------------------------------------
simul_stat <- as.vector(NULL)
schools <- read.csv('teachers_final.csv')
set.seed(1001)
for(i in 1:1000) {
  print(i)
  schools$rand <- runif(100,min=0,max=1)
  schools$treatment_rand <- as.numeric(rank(schools$rand)<=49)
  schools$control_rand = 1-schools$treatment_rand
  simul_stat <-append(simul_stat,
            sum(schools$treatment_rand*schools$open)/sum(schools$treatment_rand) 
            - sum(schools$control_rand*schools$open)/sum(schools$control_rand))
}

schools$control = 1-schools$treatment
actual_stat <- sum(schools$treatment*schools$open)/sum(schools$treatment) - sum(schools$control*schools$open)/sum(schools$control)
sum(abs(simul_stat) >= actual_stat)/NROW(simul_stat)

#Printing the ATE 
ate <- actual_stat

#Question 7 - 8   
#---------------------------------------------------

control_mean <- sum(schools$control*schools$open)/sum(schools$control)
treatment_mean <- sum(schools$treatment*schools$open)/sum(schools$treatment)

s_c <- (1/(sum(schools$control)-1))*sum(((schools$open-control_mean)*schools$control)^2)
s_t <- (1/(sum(schools$treatment)-1))*sum(((schools$open-treatment_mean)*schools$treatment)^2)

Vneyman <- (s_c/sum(schools$control) + s_t/sum(schools$treatment))
print(sqrt(Vneyman))
print(actual_stat/sqrt(Vneyman))

print(actual_stat-1.96*sqrt(Vneyman))
print(actual_stat+1.96*sqrt(Vneyman))


#Question 14 
#---------------------------------------------------
attach(schools)
bw_a <-npreg(xdat=pctpostwritten, ydat= open, bws=0.04,bandwidth.compute=FALSE)
plot(bw_a)

bw_b <-npreg(xdat=pctpostwritten, ydat= open, bws=0.001,bandwidth.compute=FALSE)
plot(bw_b)

bw_c <-npreg(xdat=pctpostwritten, ydat= open, bws=1,bandwidth.compute=FALSE)
plot(bw_c)

bw_d <-npreg(xdat=pctpostwritten, ydat= open, bws=20,bandwidth.compute=FALSE)
plot(bw_d)


#Question 17 
#---------------------------------------------------


treat <- schools$pctpostwritten[treatment==1]
cont <- schools$pctpostwritten[treatment==0]

ks.test(treat, cont, "greater")


#Question 18 
#---------------------------------------------------
schools$group[schools$treatment==1] <-"T"
schools$group[schools$treatment==0] <-"C"
ggplot(schools, aes(pctpostwritten,colour = group)) + stat_ecdf()



