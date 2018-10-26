# Preliminaries
#-------------------------------------------------
#install.packages('perm')
library(perm)
rm(list = ls())

# Questions 1 - 4
#-------------------------------------------------

#*change information here for students
perms <- chooseMatrix(8,4)
A <- matrix(c(0.462, 0.731, 0.571, 0.923, 0.333, 0.750, 0.893, 0.692), nrow=8, ncol=1, byrow=TRUE)
treatment_avg <- (1/4)*perms%*%A
control_avg <- (1/4)*(1-perms)%*%A
test_statistic <- abs(treatment_avg-control_avg)
rownumber <- apply(apply(perms, 1, 
                         function(x) (x == c(0, 1, 0, 0, 0, 1, 1, 1))), 
                   2, sum)
rownumber <- (rownumber == 8)
observed_test <- test_statistic[rownumber == TRUE]

#*change information here for students
larger_than_observed <- (test_statistic >= observed_test)
#numbers in which the statistic exceeds the value in the observed date
sum(larger_than_observed)
df <- data.frame(perms,control_avg,treatment_avg,test_statistic)

# Question 5 - 6
#-------------------------------------------------
simul_stat <- as.vector(NULL)
schools <- read.csv('teachers_final.csv')
set.seed(1001)
for(i in 1:100) {
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

#Question 7 - 8
#---------------------------------------------------
#Printing the Average Treatment Effect
ate <- actual_stat
ate

# Control and treatment means
control_mean <- sum(schools$control*schools$open)/sum(schools$control)
treatment_mean <- sum(schools$treatment*schools$open)/sum(schools$treatment)

# Sample var
s_c <- (1/(sum(schools$control)-1))*sum(((schools$open-control_mean)*schools$control)^2)
s_t <- (1/(sum(schools$treatment)-1))*sum(((schools$open-treatment_mean)*schools$treatment)^2)

# Neyman std devn
Vneyman <- (s_c/sum(schools$control) + s_t/sum(schools$treatment))
std_err <- sqrt(Vneyman)
print(actual_stat/std_err)

# 95% CI
print(actual_stat-1.96*std_err)
print(actual_stat+1.96*std_err)

# Associated treatment effect (2 sided test)
p = 2*(1-pnorm(actual_stat/std_err))
print(p)

# Required sample size for half the incentive and power of 90% and significance level of 5%
gamma = 0.5
alpha = 0.05
beta = 0.1
tau = ate*0.5 # For half the incentive case
sigma_sq = (49*s_c + 51*s_t)/100

# Least sample size
N = ((qnorm(1-beta) + qnorm(1-(alpha/2)))^2)/((tau^2/sigma_sq)*gamma*(1-gamma))

# Kernel regression
library(ggplot2)
library(np)
library(tidyverse)

attach(schools)
scores <- npreg(xdat=open, ydat=pctpostwritten, bws=0.04,bandwidth.compute=FALSE)
plot(scores)

# FOSD test
data_treatment <- filter(schools, treatment == 1)
data_control <- filter(schools, control == 1)
  
ggplot(data = data_treatment, aes(x = open, y = pctpostwritten)) + 
  stat_ecdf(data = data_treatment, aes(x = open, y = pctpostwritten), color = "darkblue") +
  stat_ecdf(data = data_control, aes(x = open, y = pctpostwritten), color = "red") +
  xlab("Percentage school open time") +
  ylab("Mean scores")
