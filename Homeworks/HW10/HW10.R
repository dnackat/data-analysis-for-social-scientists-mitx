# HW10: Replicating the results from Joshua Angrist and William Evans' study:
# "Children and Their Parents' Labor Supply: Evidence from Exogenous Variation in Family Size."

# Preliminaries
rm(list = ls())
library(tidyverse)
library(AER)

# Load the dataset
data_hw10 <- as.data.frame(read.csv("census80.csv"))

# Data Summary
summary(data_hw10)

# Instrumental variable for whether there was multiple pregnancy during the 2nd pregnancy
data_hw10$temp[data_hw10$ageq2nd == data_hw10$ageq3rd] <- 1
data_hw10$multiple <- 0
data_hw10$multiple[data_hw10$temp == 1] <- 1

# Proportion of such households
summary(data_hw10$multiple)

# Instrumental variable for whether the first and second child have the same sex
data_hw10$temp <- (data_hw10$sex1st == data_hw10$sex2nd)
data_hw10$samesex[data_hw10$temp == FALSE] <- 0
data_hw10$samesex[data_hw10$temp == TRUE] <- 1

# Proportion of such households
summary(data_hw10$samesex)

# Run an OLS model for labor supply: 
data_hw10$third <- 0
data_hw10$third[data_hw10$numberkids == 3] <- 1

ols1 <- lm(formula = workedm ~ third + blackm + hispm + othracem, data = data_hw10)
summary(ols1)

ols2 <- lm(formula = weeksm ~ third + blackm + hispm + othracem, data = data_hw10)
summary(ols2)

# Run regression for the first stage separately using the two IV's 
ols_multpreg <- lm(formula = third ~ multiple + blackm + hispm + othracem, data = data_hw10)
summary(ols_multpreg)

ols_sex1st2nd <- lm(formula = third ~ samesex + blackm + hispm + othracem, data = data_hw10)
summary(ols_sex1st2nd)

# Run IV regression
ivreg_mult <- ivreg(formula = workedm ~ third + blackm + hispm + othracem | blackm + hispm + othracem + multiple, data = data_hw10)
summary(ivreg_mult) # Dep. var = whether mother works, instr. var. = multiple 2nd pregnancy

ivreg_mult_weeks <- ivreg(formula = weeksm ~ third + blackm + hispm + othracem | blackm + hispm + othracem + multiple, data = data_hw10)
summary(ivreg_mult_weeks) # Dep. var = Number of weeks mother works, instr. var. = multiple 2nd pregnancy

ivreg_samesex <- ivreg(formula = workedm ~ third + blackm + hispm + othracem | blackm + hispm + othracem + samesex, data = data_hw10)
summary(ivreg_samesex) # Dep. var = whether mother works, instr. var. = whether sex of 1st and 2nd kid the same

ivreg_samesex_weeks <- ivreg(formula = weeksm ~ third + blackm + hispm + othracem | blackm + hispm + othracem + samesex, data = data_hw10)
summary(ivreg_samesex_weeks) # Dep. var = Number of weeks mother works, instr. var. = whether sex of 1st and 2nd kid the same

