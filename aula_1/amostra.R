
amostra = rnorm(10, mean = 1.70, sd = 0.5)

x_barra = mean(amostra)

sum(amostra)/10


sample_nor <- 
  tibble(y1 = rnorm(n = 100000, 
                    mean =  1.7, 
                    sd = .5))

ggplot() +
  geom_histogram(aes(x = y1, y = ..density..), 
                 color="grey60",
                 fill="cornsilk",
                 size=0.2,
                 bins = 300,
                 data = sample_nor) +
  # geom_density(aes(x = y1),
  #              linetype="dotted",
  #              size=0.75, 
  #              data = sample_nor) +
  stat_function(
    fun = dnorm, 
    colour="red", 
    size = 1,
    args = list(mean = 1.7, sd = .5))


tamanhos <- c(100, 500, 2500) 

base <- tibble(Amostra = 10, 
               Valores = rnorm(Amostra, mean = 1.7, sd = .5)) %>%
  bind_rows(
    tibble(Amostra = 50, 
         Valores = rnorm(Amostra, mean = 1.7, sd = .5))) %>% 
  bind_rows(
    tibble(Amostra = 100, 
           Valores = rnorm(Amostra, mean = 1.7, sd = .5)))

base %>% 
  group_by(Amostra) %>% 
  summarise(Média = mean(Valores),
            Desvio = sd(Valores))
  
Replicas = 10
n = 15

base <- expand_grid(Tamanho = c(10, 500, 25000),
                    R = , 
                    Indivíduo = 1:n) %>% 
  mutate(Valores = rnorm(n(), mean = 1.7, sd = .5)) 

base %>% count(Tamanho)

base %>% 
  group_by(Tamanho, R) %>% 
  summarise(Média = mean(Valores)) %>% 
  group_by(Tamanho) %>% 
  summarise(x_barra = mean(Média), 
            desvio_x_barra = sd(Média), 
            q1 = quantile(Média, .01),
            q99 = quantile(Média, .99))  
