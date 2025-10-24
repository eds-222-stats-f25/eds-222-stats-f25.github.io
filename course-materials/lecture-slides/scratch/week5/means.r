library(tidyverse)
theme_set(theme_classic(18))
set.seed(123)

# Choose population parameter2
mu1 <- 80
mu2 <- 120
sigma <- 30

# Simulate sample
restoration_n <- 100
restoration_1mean <- tibble(
  plot_id = 1:restoration_n,
  treatment = "Popsicle stick",
  shoot_density_m2 = rnorm(restoration_n, mean = mu2, sd = sigma)
)

ggplot(restoration_1mean, aes(shoot_density_m2)) + 
  geom_histogram(binwidth = 10,
                 color = "grey10",
                 fill = "grey90") +
  labs(x = "Shoot density (m^-2)")

expand_grid(
  i = 1:1e3,
  n = c(8, 12)
)

