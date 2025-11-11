# Trabalho final - Ferramentas Computacionais de Modelagem
# Grupo: João Lucas Sacomani Gardenal, Beatriz Moreira Magiore e Kaio Murilo Leite
# Banco de dados: Global Earthquake-Tsunami Risk Assessment


### Primeira exploração e análises simples ###

library(tidyverse)
library(maps)


# Importar o conjunto de dados
dados <- read.csv("earthquake_data_tsunami.csv")

# Descrição dos dados
str(dados)
head(dados)
glimpse(dados)       
nrow(dados)          
colnames(dados)

dados$tsunami <- factor(dados$tsunami, levels = c("0", "1"))

dados %>%
  group_by(tsunami) %>% 
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

ggplot(dados, aes(x = magnitude, fill = tsunami)) +
  geom_histogram(binwidth = 0.2, color = "white") +
  labs(
    title = "Distribuição das magnitudes dos terremotos (2001–2022)",
    x = "Magnitude (Richter)",
    y = "Frequência"
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"),
                    name = "Tsunami", labels = c("0" = "Não", "1" = "Sim")) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

# profundidade vs magnitude

dados %>% 
  group_by(tsunami) %>% 
  ggplot(aes(x = magnitude, y = depth, color = tsunami)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"),
                     name = "Tsunami", labels = c("0" = "Não", "1" = "Sim")) +
  labs(title = "Relação entre Profundidade e Magnitude",
       y = "Profundidade (km)", x = "Magnitude")+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  #scale_y_continuous(trans = "log10", limits = c(2,700))+
  facet_wrap(.~tsunami)+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )

# latitude e longitude

dados %>% 
  arrange(tsunami) %>% 
  ggplot(aes(x = longitude, y = latitude, color = tsunami)) +
  annotation_borders("world", colour = "gray70", fill = "gray90") +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"),
                     name = "Tsunami", labels = c("0" = "Não", "1" = "Sim")) +
  coord_fixed() +
  labs(title = "Distribuição Geográfica dos Terremotos (2001 - 2022)",
       x = "Longitude", y = "Latitude")+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )

# por ano

dados %>%
  count(Year, tsunami) %>%
  ggplot(aes(x = Year, y = n, fill = tsunami)) +
  geom_col(position = position_stack(reverse = TRUE), alpha = 0.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"),
                     name = "Tsunami", labels = c("0" = "Não", "1" = "Sim")) +
  labs(title = "Número de Eventos Sísmicos por Ano (2001 - 2022)",
       x = "Ano", y = "Número de eventos")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )
