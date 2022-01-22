# Método de Monte Carlo

library(tidyverse)
library(ggforce)
#install.packages("ggforce")

N = 20

#set.seed(123)

# base <- tibble(E = 1:N,
               # x = runif(N, min = -1, max = 1),
               # y = runif(N, min = -1, max = 1),
               # flag = if_else(x^2 + y^2 <= 1, "dentro", "fora"),
               # cumsum = cumsum(flag == "dentro"),
               # freq_rel = cumsum/E)
Replicas = 1000
N = 10

base_boot <- expand_grid(R = 1:Replicas, E = 1:N) %>% 
  mutate(x = runif(n(), min = -1, max = 1),
         y = runif(n(), min = -1, max = 1),
         flag = if_else(x^2 + y^2 <= 1, "dentro", "fora")
         ) %>% 
  group_by(R) %>% 
  mutate(cumsum = cumsum(flag == "dentro"),
         freq_rel = cumsum/E) %>% 
  select(R, E, freq_rel) %>% 
  slice_max(n = 1, order_by = E)





base_boot %>% 
  ggplot() + 
  geom_histogram(aes(x = freq_rel)) +
  xlim(0,1)


summary(base_boot$freq_rel)
