ls()
rm(list = ls())
########## Analisis preliminar de la vegetación posabandono y posfuego de la región chaqueña -----
setwd("D:/FAO/Analisis Preliminar/AnalisisVegetacionFAO")
# librerias
library(dplyr)


### Riqueza ----
### Adultos
data <- read.csv("data_ParcelaA.csv", fileEncoding = "latin1")
data <- filter(data, !is.na(ID.Parcela) & ID.Parcela > 0)

### Juveniles
### Renovales