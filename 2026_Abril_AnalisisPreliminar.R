ls()
rm(list = ls())
########## Analisis preliminar de la vegetación posabandono y posfuego de la región chaqueña -----
library(dplyr)

# datos
dataA <- read.csv("data_ParcelaA.csv", fileEncoding = "latin1")
dataB <- read.csv("data_ParcelaB.csv", fileEncoding = "latin1")
data_Ren <- read.csv("data_Parcela_Abu.csv", fileEncoding = "latin1")
data_Cob <- read.csv("data_Parcela_Cob.csv", fileEncoding = "latin1")

dataA <- filter(dataA, !is.na(ID.Parcela) & ID.Parcela > 0)
dataB <- filter(dataB, !is.na(ID.Parcela) & ID.Parcela > 0)
data_Ren <- filter(data_Ren, !is.na(ID.Parcela) & ID.Parcela > 0)
data_Cob <- filter(data_Cob, !is.na(ID.Parcela) & ID.Parcela > 0)

########################## Riqueza --------------------------------------
#### Rarefacción ----
library(tidyr)
library(iNEXT)
library(ggplot2)
library(dplyr)

hacer_inext <- function(data, nombre) {
  
  # Filtrar especies inválidas
  data <- filter(data, Especie != " -")
  
  # Remover remediciones
  data_uni <- data %>%
    distinct(ID.individuo, Especie, Condición, .keep_all = TRUE)
  
  # =========================
  # Curvas por condición
  # =========================
  
  lista_inext <- split(
    data_uni$Especie,
    data_uni$Condición
  )
  
  lista_inext <- lapply(lista_inext, table)
  
  out_par <- iNEXT(
    lista_inext,
    q = 0,
    datatype = "abundance"
  )
  
  inext_par <- ggiNEXT(
    out_par,
    type = 1,
    color.var = "Assemblage"
  ) +
    
    scale_color_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    
    scale_fill_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    
    labs(
      title = paste("Rarefacción por condición -", nombre),
      x = "Número de individuos",
      y = "Riqueza de especies"
    ) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
  ggsave(
    paste0("Graficos/iNext_", nombre, "_PorCondicion.png"),
    plot = inext_par,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # =========================
  # Curva global
  # =========================
  
  lista_inext_G <- table(data_uni$Especie)
  
  lista_inext_G <- setNames(
    as.numeric(lista_inext_G),
    names(lista_inext_G)
  )
  
  out_glob <- iNEXT(
    lista_inext_G,
    q = 0,
    datatype = "abundance"
  )
  
  inext_glob <- ggiNEXT(
    out_glob,
    type = 1
  ) +
    
    labs(
      title = paste("Rarefacción global -", nombre),
      x = "Número de individuos",
      y = "Riqueza de especies"
    ) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
  ggsave(
    paste0("Graficos/iNext_", nombre, "_Global.png"),
    plot = inext_glob,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  return(list(
    out_par = out_par,
    out_glob = out_glob,
    grafico_par = inext_par,
    grafico_global = inext_glob
  ))
}

res_A <- hacer_inext(dataA, "Adultos")

res_B <- hacer_inext(dataB, "Juveniles")

#### Numero de especies por parcela ----

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

### Grafico de diferencias de numero de especies ----

library(dplyr)
library(ggplot2)

hacer_grafico_dif <- function(data, titulo, archivo) {
  
  df_diff <- data %>%
    group_by(Estancia) %>%
    mutate(ref = n_especies[Condición == "Referencia"]) %>%
    ungroup() %>%
    filter(Condición != "Referencia") %>%
    mutate(diferencia = n_especies - ref)
  
  grafico <- ggplot(df_diff,
                    aes(x = Estancia,
                        y = diferencia,
                        fill = Condición)) +
    
    geom_col(position = position_dodge2(width = 0.8,
                                        preserve = "single")) +
    
    geom_hline(yintercept = 0, color = "black") +
    
    geom_text(
      data = subset(df_diff, diferencia == 0),
      aes(label = "*", color = Condición),
      position = position_dodge2(width = 0.8,
                                 preserve = "single"),
      hjust = -0.3,
      size = 6,
      show.legend = FALSE
    ) +
    
    coord_flip() +
    
    labs(
      title = titulo,
      y = "Diferencia respecto de la referencia",
      x = "Estancia",
      fill = "Condición"
    ) +
    
    scale_fill_manual(values = c(
      "Chacra" = "#1b9e77",
      "Fuego" = "#d95f02",
      "Rolado" = "#7570b3"
    )) +
    
    scale_color_manual(values = c(
      "Chacra" = "#1b9e77",
      "Fuego" = "#d95f02",
      "Rolado" = "#7570b3"
    )) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
  ggsave(
    filename = archivo,
    plot = grafico,
    width = 8,
    height = 8,
    dpi = 300
  )
  
  return(grafico)
}

dif_adu_S <- hacer_grafico_dif(
  riquezaA,
  "Diferencia en riqueza de Adultos respecto de la Referencia",
  "Graficos/DiferenciaRiquezaAdultos.png"
)

dif_juv_S <- hacer_grafico_dif(
  riquezaB,
  "Diferencia en riqueza de Juveniles respecto de la Referencia",
  "Graficos/DiferenciaRiquezaJuveniles.png"
)

dif_ren_S <- hacer_grafico_dif(
  riqueza_Ren,
  "Diferencia en riqueza de Renovales respecto de la Referencia",
  "Graficos/DiferenciaRiquezaRenovales.png"
)
######################### Abundancia ------------------------------------
#### Abundancia total por parcela ----

ind_total_A <- dataA %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_individuos = n_distinct(ID.individuo), .groups = "drop")

ind_total_B <- dataB %>%
  group_by(ID.Parcela, Condición, Estancia) %>%
  summarise(n_individuos = n_distinct(ID.individuo), .groups = "drop")

data_Ren$Cantidad.juveniles <- as.numeric(data_Ren$Cantidad.juveniles)
ind_total_Ren <- data_Ren %>%
  group_by(Estancia, Condición) %>%
  summarise(n_individuos = sum(Cantidad.juveniles, na.rm = TRUE),
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

### Diferencias 
library(dplyr)
library(ggplot2)

hacer_grafico_dif_abund <- function(data, titulo, archivo) {
  
  # Calcular diferencia respecto de Referencia
  df_diff <- data %>%
    group_by(Estancia) %>%
    mutate(ref = n_individuos[Condición == "Referencia"]) %>%
    ungroup() %>%
    filter(Condición != "Referencia") %>%
    mutate(diferencia = n_individuos - ref)
  
  # Gráfico
  grafico <- ggplot(
    df_diff,
    aes(
      x = Estancia,
      y = diferencia,
      fill = Condición
    )
  ) +
    
    geom_col(
      position = position_dodge2(
        width = 0.8,
        preserve = "single"
      )
    ) +
    
    geom_hline(
      yintercept = 0,
      color = "black"
    ) +
    
    geom_text(
      data = subset(df_diff, diferencia == 0),
      aes(
        label = "*",
        color = Condición
      ),
      position = position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      hjust = -0.3,
      size = 6,
      show.legend = FALSE
    ) +
    
    coord_flip() +
    
    labs(
      title = titulo,
      y = "Diferencia de abundancia respecto de la referencia",
      x = "Estancia",
      fill = "Condición"
    ) +
    
    scale_fill_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    
    scale_color_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    
    theme_bw() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
  # Guardar
  ggsave(
    filename = archivo,
    plot = grafico,
    width = 8,
    height = 8,
    dpi = 300
  )
  
  return(grafico)
}

dif_abund_A <- hacer_grafico_dif_abund(
  ind_total_A,
  "Diferencia en abundancia de Adultos respecto de la Referencia",
  "Graficos/DiferenciaAbundanciaAdultos.png"
)

dif_abund_B <- hacer_grafico_dif_abund(
  ind_total_B,
  "Diferencia en abundancia de Juveniles respecto de la Referencia",
  "Graficos/DiferenciaAbundanciaJuveniles.png"
)

dif_abund_R <- hacer_grafico_dif_abund(
  ind_total_Ren,
  "Diferencia en abundancia de Renovales respecto de la Referencia",
  "Graficos/DiferenciaAbundanciaRenovales.png"
)

#### Abundancia por especie y por parcela ----

ind_total_A <- dataA %>%
  group_by(ID.Parcela, Condición, Estancia, Especie) %>%
  summarise(n_individuos = n_distinct(ID.individuo), .groups = "drop")

ind_total_A <- filter(ind_total_A, Especie != " -")

ind_total_B <- dataB %>%
  group_by(ID.Parcela, Condición, Estancia, Especie) %>%
  summarise(n_individuos = n_distinct(ID.individuo), .groups = "drop")

ind_total_B <- filter(ind_total_B, Especie != " -")

data_Ren$Cantidad.juveniles <- as.numeric(data_Ren$Cantidad.juveniles)
ind_total_Ren <- data_Ren %>%
  group_by(Estancia, Condición, Especie) %>%
  summarise(n_individuos = sum(Cantidad.juveniles, na.rm = TRUE),
            .groups = "drop")

data_Ren <- filter(data_Ren, Especie != " -")
data_Ren <- filter(data_Ren, Especie != "-")
data_Ren <- filter(data_Ren, Especie != " ")

### Graficos
plot_abundancia <- function(df, ylabel, filename) {
  
  q <- ggplot(df, aes(x = Condición, y = n_individuos, fill = Especie)) +
    geom_col() +
    facet_wrap(~ Estancia)  +
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

#### Abundancia de las especies mas frecuentes ----
library(dplyr)


# ESPECIES FRECUENTES


total_sitios <- n_distinct(dataA$ID.Parcela)

especies_frecuentes <- dataA %>%
  
  filter(
    Especie != "-",
    Especie != " -",
    Especie != " "
  ) %>%
  
  group_by(Especie) %>%
  
  summarise(
    sitios_ocupados = n_distinct(ID.Parcela),
    frecuencia = sitios_ocupados / total_sitios,
    .groups = "drop"
  ) %>%
  
  filter(frecuencia > 0.25) %>%
  
  pull(Especie)

ind_total_A2 <- ind_total_A %>%
  
  mutate(
    Especie2 = ifelse(
      Especie %in% especies_frecuentes,
      Especie,
      "Otras"
    )
  ) %>%
  
  group_by(
    ID.Parcela,
    Condición,
    Estancia,
    Especie2
  ) %>%
  
  summarise(
    n_individuos = sum(n_individuos),
    .groups = "drop"
  )

ind_total_B2 <- ind_total_B %>%
  
  mutate(
    Especie2 = ifelse(
      Especie %in% especies_frecuentes,
      Especie,
      "Otras"
    )
  ) %>%
  
  group_by(
    ID.Parcela,
    Condición,
    Estancia,
    Especie2
  ) %>%
  
  summarise(
    n_individuos = sum(n_individuos),
    .groups = "drop"
  )

ind_total_Ren2 <- ind_total_Ren %>%
  
  mutate(
    Especie2 = ifelse(
      Especie %in% especies_frecuentes,
      Especie,
      "Otras"
    )
  ) %>%
  
  group_by(
    Condición,
    Estancia,
    Especie2
  ) %>%
  
  summarise(
    n_individuos = sum(n_individuos),
    .groups = "drop"
  )

plot_abundancia <- function(df, ylabel, filename) {
  
  q <- ggplot(
    df,
    aes(
      x = Condición,
      y = n_individuos,
      fill = Especie2
    )
  ) +
    
    geom_col() +
    
    facet_wrap(~ Estancia) +
    
    labs(
      x = "Condición",
      y = ylabel,
      fill = "Especie"
    ) +
    
    theme_bw()
  
  ggsave(
    filename,
    plot = q,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  return(q)
}

# Adultos
plot_abundancia(
  ind_total_A2,
  "Número de individuos adultos",
  "Graficos/Abundancia_Adultos_Freq.png"
)

# Juveniles
plot_abundancia(
  ind_total_B2,
  "Número de individuos juveniles",
  "Graficos/Abundancia_Juveniles_Freq.png"
)

# Renovales
plot_abundancia(
  ind_total_Ren2,
  "Número de individuos renovales",
  "Graficos/Abundancia_Renovales_Freq.png"
)

##### Espeies de interes ----

library(dplyr)
library(ggplot2)


# ESPECIES FOCALES


especies_focales <- c(
  "Asp que",
  "Nel fle",
  "Cer pra"
)

# Después podés agregar más:
# especies_focales <- c(
#   "Asp que",
#   "Nel fle",
#   "Cer pra",
#   "Sar mis"
# )


# FUNCIÓN PARA RESUMIR


library(dplyr)
library(tidyr)

library(dplyr)
library(tidyr)

calcular_abundancia_focal <- function(data,
                                      especie_col = "Especie") {
  
  # -------------------------
  # Combinaciones reales
  # -------------------------
  
  combos_reales <- data %>%
    
    distinct(
      ID.Parcela,
      Condición,
      Estancia
    )
  
  # -------------------------
  # Abundancia observada
  # -------------------------
  
  abund <- data %>%
    
    filter(
      .data[[especie_col]] %in% especies_focales
    ) %>%
    
    group_by(
      ID.Parcela,
      Condición,
      Estancia,
      Especie
    ) %>%
    
    summarise(
      n_individuos = n_distinct(ID.individuo),
      .groups = "drop"
    )
  
  # -------------------------
  # Completar especies faltantes
  # -------------------------
  
  combos_reales %>%
    
    crossing(
      Especie = especies_focales
    ) %>%
    
    left_join(
      abund,
      by = c(
        "ID.Parcela",
        "Condición",
        "Estancia",
        "Especie"
      )
    ) %>%
    
    mutate(
      n_individuos = replace_na(
        n_individuos,
        0
      )
    )
}


# GENERAR BASES


ind_total_A_focal <- calcular_abundancia_focal(dataA)

ind_total_B_focal <- calcular_abundancia_focal(dataB)


# GRAFICO DE ABUNDANCIA


plot_abundancia_focal <- function(df, ylabel, filename) {
  
  q <- ggplot(
    df,
    aes(
      x = Condición,
      y = n_individuos,
      fill = Condición
    )
  ) +
    
    geom_col() +
    
    facet_grid(
      Especie ~ Estancia,
      scales = "free_y"
    ) +
    
    scale_fill_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    
    labs(
      x = "Condición",
      y = ylabel
    ) +
    
    theme_bw()
  
  ggsave(
    filename,
    plot = q,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  return(q)
}


# GRAFICOS


plot_abundancia_focal(
  ind_total_A_focal,
  "Número de individuos adultos",
  "Graficos/Abundancia_Adultos_Focales.png"
)

plot_abundancia_focal(
  ind_total_B_focal,
  "Número de individuos juveniles",
  "Graficos/Abundancia_Juveniles_Focales.png"
)

# DIFERENCIA VS REFERENCIA


hacer_grafico_dif_abund_focal <- function(data, titulo, archivo) {
  
  df_diff <- data %>%
    
    group_by(Estancia, Especie) %>%
    
    mutate(
      ref = sum(
        n_individuos[Condición == "Referencia"],
        na.rm = TRUE
      )
    ) %>%
    
    ungroup() %>%
    
    filter(Condición != "Referencia") %>%
    
    mutate(
      diferencia = n_individuos - ref
    )
  
  grafico <- ggplot(
    df_diff,
    aes(
      x = Estancia,
      y = diferencia,
      fill = Condición
    )
  ) +
    
    geom_col(
      position = position_dodge2(
        width = 0.8,
        preserve = "single"
      )
    ) +
    
    geom_hline(
      yintercept = 0,
      color = "black"
    ) +
    
    coord_flip() +
    
    facet_wrap(
      ~ Especie,
      scales = "free_y"
    ) +
    
    labs(
      title = titulo,
      y = "Diferencia respecto de Referencia",
      x = "Estancia"
    ) +
    
    scale_fill_manual(values = c(
      "Referencia" = "#64B97B",
      "Fuego" = "#B9A564",
      "Chacra" = "#6478B9",
      "Rolado" = "#B964A2"
    )) +
    
    theme_bw()
  
  ggsave(
    archivo,
    plot = grafico,
    width = 14,
    height = 6,
    dpi = 300
  )
  
  return(grafico)
}


# GRAFICOS DIFERENCIA


dif_abund_A_focal <- hacer_grafico_dif_abund_focal(
  ind_total_A_focal,
  "Diferencia de abundancia de Adultos",
  "Graficos/Dif_Adultos_Focales.png"
)

dif_abund_B_focal <- hacer_grafico_dif_abund_focal(
  ind_total_B_focal,
  "Diferencia de abundancia de Juveniles",
  "Graficos/Dif_Juveniles_Focales.png"
)

######################### Area Basal -----------------------------------
######################### Cobertura ------------------------------------