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
fit3 <-lm(lwage ~ 1, data = hw_data)

# Compare the two fits
anova(fit2, fit3)

# Check test statistic to confirm
beta_0_rest <- 1.869
ssr_unrest <- sum((hw_data$lwage - beta_0 - beta_1*hw_data$black)**2)
ssr_rest <- sum((hw_data$lwage - beta_0_rest)**2)
r <- 1
n <- nrow(hw_data)
k_1 <- 2
T_stat <- ((ssr_rest - ssr_unrest)/r)/(ssr_unrest/(n-k_1))
print(T_stat)

# Mincer equations
fit4 <- lm(lwage ~ yrs_school + ttl_exp, data = hw_data)
b_0 <- 0.33694422
b_1 <- 0.07914776
b_2 <- 0.03955915
SSR <- sum(hw_data$lwage - )

