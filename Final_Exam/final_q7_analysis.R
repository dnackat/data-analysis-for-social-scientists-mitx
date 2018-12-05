# Final Exam Q7: Regression with Democracy Time-Series Dataset

# Preliminaries
rm(list = ls())
library(tidyverse)

# Load the data
df_q7 <- as.data.frame(read.csv("demo.csv"))

# Scatterplot of freedom-house rating vs. GDP
ggplot(data = df_q7, aes(x = FHouse, y = GDP)) +
  geom_point(color = "darkred")

# Run a regression
lm1 <- lm(formula = GDP ~ FHouse, data = df_q7)
summary(lm1)

# Add regression line to the scatterplot
ggplot(data = df_q7, aes(x = FHouse, y = GDP)) +
  geom_point(color = "darkred") +
  geom_smooth(method = 'lm')
