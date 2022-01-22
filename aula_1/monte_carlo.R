# Método de Monte Carlo

library(tidyverse)
library(ggforce)

# Construção do circulo
ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1),
              color = "red",
              size = 1) +
  geom_rect(aes(xmin = -1, xmax = 1, 
                ymin = -1, ymax = 1),
            color = "blue",
            size = 1, alpha =0) +
  coord_fixed() +
  theme_void()

N = 100

#set.seed(123)
base <- tibble(E = 1:N, 
               x = runif(N, min = -1, max = 1),
               y = runif(N, min = -1, max = 1),
               flag = if_else(x^2 + y^2 <= 1, "dentro", "fora"))




ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1),
              color = "red",
              size = 1) +
  geom_rect(aes(xmin = -1, xmax = 1, 
                ymin = -1, ymax = 1),
            color = "blue",
            size = 1, alpha =0) +
  geom_point(aes(x = x,  y = y, color = flag), 
             data = base) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

###

base %>% 
  mutate(freq_rel = cumsum(flag == "dentro")/E) %>%
  ggplot() + 
  geom_line(aes(x = E, y = freq_rel), 
            linetype = 2, 
            color = "red") +
  geom_hline(yintercept=pi/4, color="blue") +
  labs(x = "Experimento", y = "Frequencia Relativa") +
  ggtitle("Monto Carlo", subtitle = "Valor de p")

