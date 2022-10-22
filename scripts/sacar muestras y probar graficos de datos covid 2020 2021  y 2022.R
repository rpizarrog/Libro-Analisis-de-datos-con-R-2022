# Muestra de casos covid para eño 2022

library(readr)
library(ggplot2)
library(cowplot)


# FALTA CARGAR AÑO 2019 Y 2022 LUEGO SACR MUESTRAS

# Cargar datos

getwd()
datos2020 <- read.csv("datos/COVID20-21-22/COVID19MEXICO2020.csv", encoding= "UTF-8")
datos2021 <- read.csv("datos/COVID20-21-22/COVID19MEXICO2021.csv", encoding= "UTF-8")
datos2022 <- read.csv("datos/COVID20-21-22/COVID19MEXICO2022.csv", encoding= "UTF-8")


# Tamaño de la muestra para 2021
# 99% confianza 1 margen de error
# De acuerdo a : https://www.questionpro.com/es/calculadora-de-muestra.html
size = 16639 

n <- nrow(datos2021)

# Los registros de la muestra 
r.muestra <- sample(x = 1:nrow(datos2021), size = size, replace = FALSE)


muestra.COVID.2021 <- datos2021[r.muestra, ]

write.csv(muestra.COVID.2021, file = "muestra.COVID.2021.csv")

# Histograma

g1 <- ggplot(data = datos2021) + 
  geom_histogram(aes(x=EDAD))

g2 <- ggplot(data = muestra.COVID.2021) +
  geom_histogram(aes(x=EDAD))

plot_grid(g1, g2)


# Gráfico de puntos NO CONVIENE SN MUCHOS

n <- nrow(datos2021) # Total de datos de la muestra
g <- ggplot(data = muestra.COVID.2021) + 
  geom_point(aes(x=1:n, y = EDAD))

g


# Gráficos de barra para genero
g <- ggplot(data = datos2021) + 
  geom_bar(aes(EDAD))

g