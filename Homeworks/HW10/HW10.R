# HW10: Replicating the results from Joshua Angrist and William Evans' study:
# "Children and Their Parents' Labor Supply: Evidence from Exogenous Variation in Family Size."

# Preliminaries
rm(list = ls())
library(tidyverse)

# Load the dataset
data_hw10 <- as.data.frame(read.csv("census80.csv"))

# Data Summary
summary(data_hw10)

# Instrumental variable for whether there was multiple pregnancy during the 2nd pregnancy
data_hw10$mult2nd <- as.numeric(data_hw10$ageq2nd == data_hw10$ageq3rd)

# Proportion of such households
sum(data_hw10$mult2nd, na.rm = TRUE)/nrow(data_hw10)

# Instrumental variable for whether the first and second child have the same sex
data_hw10$sex1st2nd <- as.numeric(data_hw10$sex1st == data_hw10$sex2nd)

# Proportion of such households
sum(data_hw10$sex1st2nd, na.rm = TRUE)/nrow(data_hw10)

# Run an OLS model for labor supply: 
ols1 <- lm(formula = workedm ~ I(numberkids - 1) + blackm + hispm + othracem, data = data_hw10)
summary(ols1)

ols2 <- lm(formula = weeksm ~ I(numberkids - 1) + blackm + hispm + othracem, data = data_hw10)
summary(ols2)

# 

