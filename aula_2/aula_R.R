library(tidyverse)
a <-0; b <-100
pop_mean <- (a+b)/2 
pop_sd <- sqrt(((b-a)^2)/12)
mean_function <- function(x) mean(runif(x, a, b))

tibble(X = runif(n = 1000000, min = a, max = b)) %>% ggplot(aes(x = X, y = ..density..)) +
  geom_histogram(binwidth = 2.5, fill = rgb(89, 201, 165, maxColorValue = 255), alpha = .8, 
                 color = rgb(91, 108, 93, maxColorValue = 255)) +
  labs(x = "Amostra de uma distribuição uniform", y = "Densidade") +
  geom_hline(yintercept = 1/(b-a), color = "red", size = 1)

#### Estimadores da variância

mean_function <-function(x) mean(rnorm(n = x, 
                                       mean =  0, 
                                       sd =  5))

S1_function <-function(n) {
 amostra <- rnorm(n = n, mean =  0, sd =  5) 
 sum((amostra - mean(amostra))^2)/(n-1)
}

S2_function <-function(n) {
  amostra <- rnorm(n = n, mean =  0, sd =  5) 
  sum((amostra - mean(amostra))^2)/(n)
}

Replicas <-10000

amostra_S1_1 <-replicate(Replicas, {S1_function(10)})
amostra_S1_2 <-replicate(Replicas, {S1_function(1000)})

amostra_S2_1 <-replicate(Replicas, {S2_function(10)})
amostra_S2_2 <-replicate(Replicas, {S2_function(1000)})

base <- tibble(sample_size = rep(c("N = 10","N = 100"), 
                                 times = c(Replicas, Replicas)), 
               S1 = c(amostra_S1_1, amostra_S1_2))

base %>%  group_by(sample_size) %>% 
  summarise(`Réplicas` = n(), 
            Média = mean(S1), 
            Desvio = sd(S1)) %>%
  pull(Média)


base %>% 
  ggplot() + 
  geom_density(aes(x = S1, y = ..density.., 
                   color = sample_size, 
                   fill = sample_size), alpha = .5) +
  geom_vline(xintercept = 25, color = "red", size = 1)


#####

base <- tibble(sample_size = rep(c("N = 100","N = 1000"), 
                                 times = c(Replicas, Replicas)), 
               S2 = c(amostra_S2_1, amostra_S2_2))

base %>%  group_by(sample_size) %>% 
  summarise(`Réplicas` = n(), 
            Média = mean(S2), 
            Desvio = sd(S2)) %>%
  pull(Média)

base %>% 
  ggplot() + 
  geom_density(aes(x = S2, y = ..density.., 
                   color = sample_size, 
                   fill = sample_size), alpha = .5) +
  geom_vline(xintercept = 25, color = "red", size = 1)
