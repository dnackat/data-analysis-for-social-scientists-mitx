# Misc calcs for final exam 

# Preliminaries
rm(list = ls())
library(tidyverse)

nCk <- function(n,k) {
  factorial(n)/(factorial(k)*factorial(n-k))
}

# Calc prob Q1, part 5 (world cup ties)
prob <- 0
for (i in 1:4) {
  j <- i-1
  prob <- prob + (nCk(3,j)*((0.7)^j)*((0.3)^(3-j)))^2
}
print(prob)

# Prob. that team 1 will win, part 6
prob_3_2 <- (nCk(3,1)*(0.7)*(0.3^2))*(nCk(3,0)*(0.3^3))
prob_4_2 <- (nCk(3,2)*((0.7)^2)*0.3)*(nCk(3,0)*(0.3^3))
prob_4_3 <- (nCk(3,2)*((0.7)^2)*0.3)*(nCk(3,1)*0.7*(0.3^2))
prob_5_2 <- (nCk(3,3)*((0.7)^3))*(nCk(3,0)*(0.3^3))
prob_5_3 <- (nCk(3,3)*((0.7)^3))*(nCk(3,1)*0.7*(0.3^2))
prob_5_4 <- (nCk(3,3)*((0.7)^3))*(nCk(3,2)*((0.7)^2)*0.3)

prob_1_win <- prob_3_2 + prob_4_2 + prob_4_3 + prob_5_2 + prob_5_3 + prob_5_4
print(prob_1_win)

# Q3: score for 90 percentile
qnorm(0.9, mean = 70, sd = 20)

# Q4: CLT simulation
df5 <- data.frame()
nsims <- 10000
for (i in 1:nsims) {
  df5 <- rbind(df5, rexp(5, rate = 2))
}

df5$mean <- rowMeans(df5, na.rm = TRUE, dims = 1) # Add means column

# Plot a histogram of means
ggplot(data = df5, aes(mean)) +
  geom_histogram(aes(mean, ..density..), binwidth = 0.01, fill = "darkblue")

# Repeat this process with n = 1, 10, and 30.Compare these.
df1 <- data.frame()
nsims <- 10000
for (i in 1:nsims) {
  df1 <- rbind(df1, rexp(1, rate = 2))
}

df1$mean <- rowMeans(df1, na.rm = TRUE, dims = 1)

# n = 10
df10 <- data.frame()
nsims <- 10000
for (i in 1:nsims) {
  df10 <- rbind(df10, rexp(10, rate = 2))
}

df10$mean <- rowMeans(df10, na.rm = TRUE, dims = 1)

# n = 30
df30 <- data.frame()
nsims <- 10000
for (i in 1:nsims) {
  df30 <- rbind(df30, rexp(30, rate = 2))
}

df30$mean <- rowMeans(df30, na.rm = TRUE, dims = 1)

# Plot all of these in the same graph
ggplot(data = df5, aes(mean)) +
  geom_histogram(data = df1, aes(mean, ..density..), binwidth = 0.01, color = "darkblue") +
  geom_histogram(data = df5, aes(mean, ..density..), binwidth = 0.01, color = "darkred") +
  geom_histogram(data = df10, aes(mean, ..density..), binwidth = 0.01, color = "darkgreen") +
  geom_histogram(data = df30, aes(mean, ..density..), binwidth = 0.01, color = "black")

# Compare sample means
mean_1 <- mean(df1$mean)
mean_5 <- mean(df5$mean)
mean_10 <- mean(df10$mean)
mean_30 <- mean(df30$mean)

# Compare standard deviations
sd_1 <- sd(df1$mean)
sd_5 <- sd(df5$mean)
sd_10 <- sd(df10$mean)
sd_30 <- sd(df30$mean)