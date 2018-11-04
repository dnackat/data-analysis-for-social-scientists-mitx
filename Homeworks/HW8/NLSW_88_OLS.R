# HW8: OLS Estimation

# Preliminaries
rm(list = ls())
library(tidyverse)
library(ggplot2)

# Read in the data
hw_data <- as.data.frame(read.csv("nlsw88.csv"))

# The model
fit <- lm(lwage ~ yrs_school, data = hw_data)
print(fit)

# 90% CI for parameters
confint(fit, level=0.90)

# Residuals of fit
sum(residuals(fit))

# Next model
fit2 <- lm(lwage ~ black, data = hw_data)
print(fit2)

# Analytical calculation as a check
y_other <- sum((1-hw_data$black)*hw_data$lwage)/sum(1-hw_data$black)
y_black <- sum(hw_data$black*hw_data$lwage)/sum(hw_data$black)
beta_0 <- y_other
beta_1 <- y_black - y_other

# Test the hypothesis that beta_1 is zero. Run the resticted model:
n_obs <- nrow(hw_data)


