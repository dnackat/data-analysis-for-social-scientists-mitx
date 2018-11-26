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
data_mult2nd <- filter(data_hw10, ageq2nd == ageq3rd)

# Proportion of such households
nrow(data_mult2nd)/nrow(data_hw10)

# Instrumental variable for whether the first and second child have the same sex
