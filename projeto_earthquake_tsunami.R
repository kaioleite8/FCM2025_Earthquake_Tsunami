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

# Correlação entre variáveis numéricas
##obs.: excluindo "nst" pois tem muitos zeros 
###(pode ser falta de dados? ou realmente não tem estação?)

vars_corr <- dados %>%
  select(magnitude, depth, cdi, mmi, sig, dmin, gap)

# Matriz de correlação
corr_mat <- cor(vars_corr, use = "pairwise.complete.obs") %>%
  round(2)

# Converter pra formato longo
corr_long <- corr_mat %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "cor")

# Plot com números
ggplot(corr_long, aes(var1, var2, fill = cor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = cor), size = 4) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "tomato",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlação entre Variáveis Sísmicas",
    x = "",
    y = "",
    fill = "Correlação"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# profundidade vs magnitude (correlação baixa!)
dados %>% 
  group_by(tsunami) %>% 
  ggplot(aes(x = magnitude, y = depth, color = tsunami)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"),
                     name = "Tsunami", labels = c("0" = "Não", "1" = "Sim")) +
  labs(title = "Relação entre Profundidade e Magnitude",
       y = "Profundidade (km)", x = "Magnitude")+
  scale_y_reverse() +
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

# biplot de variaveis com maiores correlações
## sig vs cdi
dados %>% 
  ggplot(aes(y = sig, x = cdi)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_x_continuous(limits = c(-0.5, 9.5), breaks = 0:9) +
  labs(title = "Relação entre Significância e Impacto populacional",
       x = "Impacto populacional", y = "Significância")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

## sig vs magnitude
dados %>% 
  ggplot(aes(y = sig, x = magnitude)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relação entre Significância e Magnitude",
       x = "Magnitude", y = "Significância")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

## sig vs mmi
dados %>% 
  ggplot(aes(y = sig, x = mmi)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_x_continuous(limits = c(0.5, 9.5), breaks = 1:9) +
  labs(title = "Relação entre Significância e Danos estruturais",
       x = "Danos estruturais", y = "Significância")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

## profundidade vs mmi
dados %>% 
  ggplot(aes(x = mmi, y = depth)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relação entre Profundidade e Danos estruturais",
       y = "Profundidade (km)", x = "Danos estruturais")+
  scale_x_continuous(limits = c(0.5, 9.5), breaks = 1:9) +
  scale_y_reverse() +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

## cdi vs mmi (arrumar proporção dos pontos!)
dados %>% 
  ggplot(aes(x = mmi, y = cdi)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relação entre Imapcto populacional e Danos estruturais",
       y = "Imapcto populaciona", x = "Danos estruturais")+
  scale_x_continuous(limits = c(0.5, 9.5), breaks = 1:9) +
  scale_y_continuous(limits = c(-0.5, 9.5), breaks = 0:9) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

## nst vs dmin (não usar -> muitos zeros); (gráfico feio...)
dados %>% 
  ggplot(aes(y = nst, x = dmin)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Relação entre Número de estações e Distância",
       x = "Distância mais próxima (graus)", y = "Número de estações")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

## latitude e longitude
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

# numero de eventos por ano
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


# Clustering geográfico simples (K-means) 

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
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )

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


# Séries temporais (apenas ideia...)
annual_summary <- dados %>%
  group_by(Year) %>%
  summarise(
    n_events = n(),
    mean_magnitude = mean(magnitude, na.rm = TRUE),
    sd_magnitude = sd(magnitude, na.rm = TRUE),
    mean_depth = mean(depth, na.rm = TRUE),
    sd_depth = sd(depth, na.rm = TRUE),
    n_tsunami = sum(as.numeric(as.character(tsunami)) == 1, na.rm = TRUE)
  ) %>%
  mutate(
    prop_tsunami = n_tsunami / n_events
  ) %>%
  arrange(Year)

print(annual_summary)

# magnitude média por ano
ggplot(annual_summary, aes(x = Year, y = mean_magnitude)) +
  geom_line() + geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Magnitude média por ano", y = "Magnitude média", x = "Ano") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )

ggplot(annual_summary, aes(x = Year, y = mean_depth)) +
  geom_line() + geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Profundidade média por ano", y = "Profundidade média (km)", x = "Ano") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 11)
  )
