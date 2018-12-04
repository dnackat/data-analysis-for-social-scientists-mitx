# Final Exam Q6: Monitoring Corruption: Evidence from a Field Experiment in Indonesia

# Preliminaries
rm(list = ls())
library(tidyverse)

# Load the data
df_q6 <- as.data.frame(read.csv("olken.csv"))

# Kernel density plots of treatment and control

# Head's education
df_t <- filter(df_q6, treat_invite == 1)
df_c <- filter(df_q6, treat_invite == 0)
ggplot(data = df_q6, aes(head_edu)) +
  geom_density(data = df_t, kernel = "gaussian", aes(head_edu), color = "darkred") +
  geom_density(data = df_c, kernel = "gaussian", aes(head_edu), color = "darkblue")

# Number of mosques
ggplot(data = df_q6, aes(mosques)) +
  geom_density(data = df_t, kernel = "gaussian", aes(mosques), color = "darkred") +
  geom_density(data = df_c, kernel = "gaussian", aes(mosques), color = "darkblue")

# Percentage of poor households
ggplot(data = df_q6, aes(pct_poor)) +
  geom_density(data = df_t, kernel = "gaussian", aes(pct_poor), color = "darkred") +
  geom_density(data = df_c, kernel = "gaussian", aes(pct_poor), color = "darkblue")

# Total budget
ggplot(data = df_q6, aes(total_budget)) +
  geom_density(data = df_t, kernel = "gaussian", aes(total_budget), color = "darkred") +
  geom_density(data = df_c, kernel = "gaussian", aes(total_budget), color = "darkblue")

# Calculate the average treatment effect (ATE) for pct_missing
t_avg <- mean(df_t$pct_missing)
c_avg <- mean(df_c$pct_missing)
ate_pct_missing <- t_avg - c_avg

# Neyman standard error
n_t <- nrow(df_t)
n_c <- nrow(df_c)
s_t_2 <- (1/(n_t-1))*sum((df_t$pct_missing - t_avg)^2)
s_c_2 <- (1/(n_c-1))*sum((df_c$pct_missing - c_avg)^2)
v_neyman <- sqrt((s_t_2/n_t) + (s_c_2/n_c))

# Test statistic for hypothesis testing
t_stat <- ate_pct_missing/v_neyman

# The associated p-value
p_val <- 2*(1-pnorm(0.7538))

# p_val > 0.05 (95% CI), so we fail to reject the null
