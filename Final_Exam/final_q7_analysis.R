# Final Exam Q7: Regression with Democracy Time-Series Dataset

# Preliminaries
rm(list = ls())
library(tidyverse)

# Load the data
df_q7 <- as.data.frame(read.csv("demo.csv"))

# Scatterplot of Freedom-House (FHouse) rating vs. GDP
plot <- ggplot(data = df_q7, aes(x = FHouse, y = GDP)) +
  geom_point(color = "darkred")
plot

# Run a regression
lm1 <- lm(formula = GDP ~ FHouse, data = df_q7)
summary(lm1)

# Add regression line to the scatterplot
plot <- plot + geom_smooth(method = 'lm')
plot

# Compute the conditional sample mean of the gdp for FHouse ratings
df_q7 %>% 
  group_by(FHouse) %>%
  summarise(mean_gdp = mean(GDP)) %>%
  as.data.frame -> df_gdp

# Plot the mean gdp (cond. on FHouse rating) on top of the scatterplot for comparison
plot <- plot + geom_line(data = df_gdp, aes(y = mean_gdp), color = "darkgreen")
plot

# Add the square of FHouse rating to data to capture the non-linear relationship with gdp
df_q7$FHouse2 <- (df_q7$FHouse)^2

# Run another regression
lm2 <- lm(formula = GDP ~ FHouse + FHouse2, data = df_q7)
summary(lm2)

# Create a data frame with the predictions
df_predict <- data.frame(pred_gdp = predict(lm2, df_q7), FH = df_q7$FHouse)

# Add this regression line to the exisitng plot
plot <- plot + geom_line(data = df_predict, aes(x = FH, y = pred_gdp), color = "black")
plot

ggsave("Democracy.pdf", plot = plot)
