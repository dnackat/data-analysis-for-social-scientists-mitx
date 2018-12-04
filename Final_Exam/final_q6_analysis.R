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
ate_pct_missing <- mean(df_t$pct_missing) - mean(df_c$pct_missing)