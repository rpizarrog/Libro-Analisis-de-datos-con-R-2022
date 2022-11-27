# Librerías
library(readr)
library(dplyr)
library(ggplot2)


# 26-Nov-2022
# Función que devuelve los estadísticos descriptivos de un df
# Recibe df con variables numéricas y devuelve parámetros y estadísticos
f_summary_all <- function (poblacion, muestra1=NULL, muestra2=NULL, muestra3 = NULL, ...) {
  if (is.data.frame(poblacion) && nrow(poblacion) > 0) {
    s_poblacion <- summary(poblacion)
    n_vars <- ncol(poblacion)
    s_sd_poblacion <- NULL
    for (v in 1:n_vars) {
      s_sd_poblacion[v] <- sd(poblacion[,v])
    }
    g_hist_poblacion <- f_histograma(poblacion)
  } 
  
  if (is.data.frame(muestra1) && nrow(muestra1) > 0) {
    s_muestra1 <- summary(muestra1)
    n_vars <- ncol(muestra1)
    s_sd_muestra1 <- NULL
    for (v in 1:n_vars) {
      s_sd_muestra1[v] <- sd(muestra1[,v])
    }
  } else {
    s_muestra1 <- NULL
    s_sd_muestra1
  }
  
  if (is.data.frame(muestra2) && nrow(muestra2) > 0) {
    s_muestra2 <- summary(muestra2)
    n_vars <- ncol(muestra2)
    s_sd_muestra2 <- NULL
    for (v in 1:n_vars) {
      s_sd_muestra2[v] <- sd(muestra2[,v])
    }
  } else {
    s_muestra2 <- NULL
    s_sd_muestra2
  }
    
  if (is.data.frame(muestra3) && nrow(muestra3) > 0) {
    s_muestra3 <- summary(muestra3)
    n_vars <- ncol(muestra3)
    s_sd_muestra3 <- NULL
    for (v in 1:n_vars) {
      s_sd_muestra3[v] <- sd(muestra3[,v])
    }
  } else {
    s_muestra3 <- NULL
    s_sd_muestra3
  }
  
  estadisticos <- list(s_poblacion = s_poblacion, s_sd_poblacion = s_sd_poblacion, 
                       g_hist_poblacion = g_hist_poblacion,
                       s_muestra1 = s_muestra1, s_sd_muestra1 = s_sd_muestra1,
                       s_muestra2 = s_muestra2, s_sd_muestra2 = s_sd_muestra2,
                       s_muestra3 = s_muestra3, s_sd_muestra3 = s_sd_muestra3)
  
  return (estadisticos)
}

# 28-nov-2022
# Función que devuelve histogramas, Funciona para un df con dos variables numéricas
# Devuelve una lista con los dos histogramas
f_histograma_dos <- function(datos) {
  variables <- colnames(datos)
  g1 <- ggplot(data = datos) +
    geom_histogram(aes(x = datos[,1]), bins=30) +
    geom_vline(xintercept = mean(datos[,1]), color ='red', lty=2)+
    ggtitle(label = "Histograma") +
    xlab(variables[1]) +
    ylab("Count") 
    
  
  g2 <- ggplot(data = datos) +
    geom_histogram(aes(x = datos[,2]), bins=30)+
    geom_vline(xintercept = mean(datos[,1]), color ='red', lty=2)+
    ggtitle(label = "Histograma") +
    xlab(variables[2])+
    ylab("Count") 
  
  graf <- list(g1 = g1, g2 = g2)
  return(graf)
} 
