# Preliminaries
#-------------------------------------------------
install.packages('perm')
library(perm)
rm(list = ls())
setwd("/Users/raz/Dropbox/14.31 edX Building the Course/Problem Sets/PSET 8")

# Questions 1 - 4
#-------------------------------------------------

#*change information here for students
perms <- chooseMatrix(8,4)
A <- matrix(c(85, 99, 100, 76, 26, 45, 97, 72), nrow=8, ncol=1, byrow=TRUE)
treatment_avg <- (1/4)*perms%*%A
control_avg <- (1/4)*(1-perms)%*%A
test_statistic <- abs(treatment_avg-control_avg)
rownumber <- apply(apply(perms, 1, 
                         function(x) (x == c(1, 1, 1, 1, 0, 0, 0, 0))), 
                   2, sum)
rownumber <- (rownumber == 8)
observed_test <- test_statistic[rownumber == TRUE]

#*change information here for students
larger_than_observed <- (test_statistic >= observed_test)
sum(larger_than_observed)
df <- data.frame(perms,control_avg,treatment_avg,test_statistic)
