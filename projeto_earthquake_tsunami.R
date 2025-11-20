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

# ---------------------------
# Clustering geográfico simples (K-means) - versão enxuta
# ---------------------------

set.seed(42)

# selecionar e limpar colunas necessárias
proc <- dados %>%
  select(latitude, longitude, magnitude) %>%
  filter(!is.na(latitude) & !is.na(longitude) & !is.na(magnitude))

# matriz para clustering (apenas lat, long, mag) e padronizar
X <- proc %>% select(latitude, longitude, magnitude) %>% as.matrix()
X_scaled <- scale(X)

# escolher k simples
k <- 4
km <- kmeans(X_scaled, centers = k, nstart = 20)

# anexar cluster
proc$zone <- factor(km$cluster)

# calcular centróides (no espaço original)
centroids <- proc %>%
  group_by(zone) %>%
  summarise(
    cen_lat = mean(latitude),
    cen_long = mean(longitude),
    cen_mag = mean(magnitude),
    n = n()
  )

# plot simples: mapa + pontos coloridos por cluster
world <- map_data("world")

p_map <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray96", colour = "gray80", size = 0.2) +
  geom_point(data = proc, aes(x = longitude, y = latitude, color = zone, size = magnitude),
             alpha = 0.7) +
  geom_point(data = centroids, aes(x = cen_long, y = cen_lat),
             color = "black", shape = 8, size = 3) +
  scale_size_continuous(range = c(1, 4)) +
  coord_quickmap() +
  labs(title = paste0("K-means (k=", k, ") - Zonas geográficas"),
       x = "Longitude", y = "Latitude", color = "Zone", size = "Magnitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p_map)

# gráfico simples: magnitude média por zona
zone_stats <- proc %>%
  group_by(zone) %>%
  summarise(
    total_events = n(),
    mean_magnitude = mean(magnitude)
  ) %>%
  arrange(desc(mean_magnitude))

p_bar <- ggplot(zone_stats, aes(x = reorder(zone, mean_magnitude), y = mean_magnitude)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Magnitude média por zona", x = "Zone", y = "Magnitude média") +
  theme_minimal()

print(p_bar)


###### 1. Gutenberg-Richter
# Criar tabela de frequência acumulada
gr_data <- dados %>%
  count(magnitude) %>%
  arrange(desc(magnitude)) %>%
  mutate(
    cumsum_n = cumsum(n),
    log_n = log10(cumsum_n)
  )

# Plotar
ggplot(gr_data, aes(x = magnitude, y = log_n)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(title = "Lei de Gutenberg-Richter", y = "Log(N >= M)", x = "Magnitude")

# 2. Energia Liberada
dados %>%
  group_by(Year) %>%
  summarise(total_energy = sum(energy, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = total_energy)) +
  geom_col(fill = "firebrick") + 
  # Usamos 'scientific = FALSE' para evitar notação científica se possível, 
  # ou usamos labels customizados
  scale_y_continuous(labels = scales::scientific) +
  labs(
    title = "Energia Sísmica: A Desproporção dos Grandes Eventos",
    subtitle = "2004 (Sumatra) e 2011 (Tohoku)",
    y = "Energia Total (Joules)",
    x = "Ano"
  ) +
  theme_minimal()
