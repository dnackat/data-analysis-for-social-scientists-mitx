# HW9: Replicating the results from David Card and Alan Krueger's study:
# "Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania."

# Preliminaries
library(tidyverse)

# Import the dataset
hw9_data <- as.data.frame(read.csv("fastfood.csv"))

# Avg diff in full-time employees between NJ and PA before the wage change
ft_pa_subset <- filter(hw9_data, pa1 == 1 | pa2 == 1)
avg_ft_pa <- sum(ft_pa_subset$empft)/nrow(ft_pa_subset)
avg_wage_st_pa <- sum(ft_pa_subset$wage_st, na.rm = TRUE)/nrow(ft_pa_subset)

ft_nj_subset <- filter(hw9_data, pa1 == 0, pa2 == 0)
avg_ft_nj <- sum(ft_nj_subset$empft)/nrow(ft_nj_subset)
avg_wage_st_nj <- sum(ft_nj_subset$wage_st, na.rm = TRUE)/nrow(ft_nj_subset)

diff_ft <- avg_ft_nj - avg_ft_pa

# Do this with a linear model
lm_ft <- lm(formula = empft ~ state, data = hw9_data)
summary(lm_ft)

# Statring wage difference (before wage change) between NJ and PA
lm_wage_st <- lm(formula = wage_st ~ state, data = hw9_data)
summary(lm_wage_st)