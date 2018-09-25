# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

# Read data
data <- read_csv()

# Look at data
print(data)

# Data for females only
data_females <- filter(data, adult=1, female=1)

# Look at female data
print(data_females)

# Remove the outliers
filter(data_females, height_cm > 120, height_cm < 200)

# Plot the histogram
ggplot(data_females, aes(height_cm)) +
  geom_histogram(data = data_females, aes(height_cm, ..density..), fill = "white", color = "darkred") + 
  geom_density(kernel="gaussian", aes(height_cm), bw = 5) +
  xlab("bw = 5")

# Save the plot
ggsave("output/femaleHeights.pdf")