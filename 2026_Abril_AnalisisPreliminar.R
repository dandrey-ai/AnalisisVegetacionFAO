ls()
rm(list = ls())
########## Analisis preliminar de la vegetación posabandono y posfuego de la región chaqueña -----

# librerias
library(dplyr)
library(ggplot2)

# datos
dataA <- read.csv("data_ParcelaA.csv", fileEncoding = "latin1")
dataB <- read.csv("data_ParcelaB.csv", fileEncoding = "latin1")
data_Ren <- read.csv("data_Parcela_Abu.csv", fileEncoding = "latin1")
data_Cob <- read.csv("data_Parcela_Cob.csv", fileEncoding = "latin1")

dataA <- filter(dataA, !is.na(ID.Parcela) & ID.Parcela > 0)
dataB <- filter(dataB, !is.na(ID.Parcela) & ID.Parcela > 0)
data_Ren <- filter(data_Ren, !is.na(ID.Parcela) & ID.Parcela > 0)
data_Cob <- filter(data_Cob, !is.na(ID.Parcela) & ID.Parcela > 0)

### Riqueza ----
### Adultos

riquezaA <- dataA %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_especies = n_distinct(Especie))

riquezaB <- dataB %>%
  group_by(ID.individuo) %>%
  filter(all(DAP.calc <= 10, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_especies = n_distinct(Especie), .groups = "drop")


riqueza_Ren <- data_Ren %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_especies = n_distinct(Especie))

library(ggplot2)

plot_riqueza <- function(df, ylabel, filename) {
  
  p <- ggplot(df, aes(x = Condición, y = n_especies, fill = Condición)) +
    geom_col() +
    facet_wrap(~ Estancia) +
    scale_fill_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    labs(x = "Condición", y = ylabel) +
    theme_bw()
  
  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
}

# Adultos
plot_riqueza(
  riquezaA,
  "Número de especies de adultos",
  "Graficos/Riqueza Adultos.png"
)

# Otro dataset (ej: B)
plot_riqueza(
  riquezaB,
  "Número de especies como juveniles",  # ajustá texto
  "Graficos/Riqueza de Juveniles.png"
)

# Juveniles / Renovales
plot_riqueza(
  riqueza_Ren,
  "Número de especies de renovales",
  "Graficos/Riqueza Renovales.png"
)


###

ind_total_A <- dataA %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_individuos = n_distinct(ID.individuo), .groups = "drop")

ind_total_B <- dataB %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_individuos = n_distinct(ID.individuo), .groups = "drop")

ind_total_Ren <- data_Ren %>%
  group_by(Estancia, Condición) %>%
  summarise(n_individuos = sum(Cantidad, na.rm = TRUE),
            .groups = "drop")

### Graficos
plot_abundancia <- function(df, ylabel, filename) {
  
  q <- ggplot(df, aes(x = Condición, y = n_individuos, fill = Condición)) +
    geom_col() +
    facet_wrap(~ Estancia) +
    scale_fill_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    labs(x = "Condición", y = ylabel) +
    theme_bw()
  
  ggsave(filename, plot = q, width = 8, height = 6, dpi = 300)
}

# Adultos
plot_abundancia(
  ind_total_A,
  "Número de individuos adultos",
  "Graficos/Abundancia_Adultos.png"
)

# Otro dataset (ej: B)
plot_abundancia(
  ind_total_B,
  "Número de individuos juveniles",  # ajustá texto
  "Graficos/AbundanciaJuveniles.png"
)

# Juveniles / Renovales
plot_abundancia(
  ind_total_Ren,
  "Número de individuos renovales",
  "Graficos/AbundanciaRenovales.png"
)
