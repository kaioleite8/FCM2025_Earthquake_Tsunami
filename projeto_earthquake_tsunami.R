# Trabalho final - Ferramentas Computacionais de Modelagem
# Grupo: João Lucas Sacomani Gardenal, Beatriz Moreira Magiore e Kaio Murilo Leite
# Banco de dados: Global Earthquake-Tsunami Risk Assessment


### Primeira exploração e análises simples ###

library(tidyverse)


# Importar o conjunto de dados
dados <- read.csv("earthquake_data_tsunami.csv")

# Descrição dos dados
str(dados)
head(dados)
glimpse(dados)       
nrow(dados)          
colnames(dados)

dados %>% 
  summarise(
    n = n(),
    media_magnitude = mean(magnitude, na.rm = TRUE),
    mediana_magnitude = median(magnitude, na.rm = TRUE),
    sd_magnitude = sd(magnitude, na.rm = TRUE),
    media_depth = mean(depth, na.rm = TRUE),
    mediana_depth = median(depth, na.rm = TRUE),
    sd_depth = sd(depth, na.rm = TRUE)
  ) %>% 
  print()

# Contagem e proporção de tsunami
dados %>% 
  count(tsunami) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(tsunami)) %>% 
  print()

# Frequência de cada magnitude dos terremotos 
ggplot(dados, aes(x = magnitude)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
  labs(
    title = "Distribuição das magnitudes dos terremotos (2001–2022)",
    x = "Magnitude (Richter)",
    y = "Frequência"
  ) +
  theme_minimal()
