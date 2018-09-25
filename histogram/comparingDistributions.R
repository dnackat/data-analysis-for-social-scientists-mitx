# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

# Read data
data1 <- read_csv()
data2 <- read_csv()

# Look at data
print(data1)
print(data2)

# Data for females only
data_females1 <- filter(data1, adult=1, female=1)
data_females2 <- filter(data2, adult=1, female=1)

# Look at female data
print(data_females1)
print(data_females2)

# Remove the outliers
filter(data_females1, height_cm > 120, height_cm < 200)
filter(data_females2, height_cm > 120, height_cm < 200)

# Plot the histogram
ggplot(aes(height_cm)) +
  geom_density(data = data_females1, color = "darkblue") + 
  geom_density(data = data_females2, color = "darkred") +
  xlab("Height in cm")

# Save the plot
ggsave("output/kernelHeights.pdf")