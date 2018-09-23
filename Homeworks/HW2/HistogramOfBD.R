library(dplyr)
library(ggplot2)

successes <- rbinom(1000,8,0.2)

binom_draws <- as_tibble(data.frame(successes))

estimated_pf <- binom_draws %>%
  group_by(successes) %>%
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))
  
ggplot(estimated_pf, aes(x=successes, y=freq)) + geom_col() + ylab("Estimated Density")