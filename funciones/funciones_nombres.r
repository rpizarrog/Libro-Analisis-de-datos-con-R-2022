# 04-ENERO-2023
# Función que construye nombres de personas y su géneros a partir de 
# Un conjunto de datos
# Los datos son con semilla 2023 usando función sample()

library(readr)
library(dplyr)
f_construye_nombres <- function(semilla) {
  set.seed(semilla)
  nombres <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/Nombres_mas_populares_2021.csv", encoding = "UTF-8")
  #summary(nombres)
  
  # Partir los datos
  datos1 <- nombres %>%
    select(NOMBRE, SEXO, PORC)
  colnames(datos1) <- c('nombre', 'genero', 'porc')
  
  datos2 <- nombres %>%
    select(NOMBRE.1, SEXO.1, PORC.1)
  colnames(datos2) <- c('nombre', 'genero', 'porc')
  
  datos <- union(datos1, datos2)
  
  
  # Generar nombres aleatorios
  n <- 500000
  nombres <- datos %>%
    sample_n(size=n,weight=porc, replace = TRUE) %>%
    select(nombre, genero)
  
  nombres$nombre <- as.factor(nombres$nombre)
  nombres$genero <- as.factor(nombres$genero)
  
  return(nombres)
}