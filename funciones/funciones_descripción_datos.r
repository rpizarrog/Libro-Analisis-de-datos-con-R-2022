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
    g_hist_poblacion <- f_histograma_dos(poblacion)
  } 
  
  if (is.data.frame(muestra1) && nrow(muestra1) > 0) {
    s_muestra1 <- summary(muestra1)
    n_vars <- ncol(muestra1)
    s_sd_muestra1 <- NULL
    for (v in 1:n_vars) {
      s_sd_muestra1[v] <- sd(muestra1[,v])
    }
    g_hist_muestra1 <- f_histograma_dos(muestra1)
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
    g_hist_muestra2 <- f_histograma_dos(muestra2)
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
    g_hist_muestra3 <- f_histograma_dos(muestra3)
  } else {
    s_muestra3 <- NULL
    s_sd_muestra3
  }
  
  
  estadisticos <- list(s_poblacion = s_poblacion, s_sd_poblacion = s_sd_poblacion, 
                       g_hist_poblacion = g_hist_poblacion,
                       s_muestra1 = s_muestra1, s_sd_muestra1 = s_sd_muestra1,
                       g_hist_muestra1 = g_hist_muestra1,
                       s_muestra2 = s_muestra2, s_sd_muestra2 = s_sd_muestra2,
                       g_hist_muestra2 = g_hist_muestra2,
                       s_muestra3 = s_muestra3, s_sd_muestra3 = s_sd_muestra3,
                       g_hist_muestra3 = g_hist_muestra3)
  
  return (estadisticos)
}

# 28-nov-2022
# Función que devuelve histogramas, Funciona para un df con dos variables numéricas
# Devuelve una lista con los dos histogramas
f_histograma_dos <- function(datos) {
  variables <- colnames(datos)
  nombre_datos <- substitute(datos)
  g1 <- ggplot(data = datos) +
    geom_histogram(aes(x = datos[,1]), bins=30) +
    geom_vline(xintercept = mean(datos[,1]), color ='red', lty=2)+
    ggtitle(label = paste("Histograma", nombre_datos),
            paste("Media aritmética:", round(mean(datos[,1]), 4))) +
    xlab(variables[1]) +
    ylab("Count") 
    
  
  g2 <- ggplot(data = datos) +
    geom_histogram(aes(x = datos[,2]), bins=30)+
    geom_vline(xintercept = mean(datos[,2]), color ='red', lty=2)+
    ggtitle(label = paste("Histograma", nombre_datos), 
            subtitle = paste("Media aritmética:", round(mean(datos[,2]), 4))) +
    xlab(variables[2])+
    ylab("Count") 
  
  graf <- list(g1 = g1, g2 = g2)
  return(graf)
} 

f_tabla_comparativa <- function(poblacion, muestra1, muestra2, muetra3) {
  tabla_comparativa <- data.frame(media.temperatura = resultado$s_poblacion[4,1], media.humedad = resultado$s_poblacion[4,2], sd.temperatura = resultado$s_sd_poblacion[1], sd.humedad = resultado$s_sd_poblacion[2])
  
  
  tabla_comparativa <- rbind(tabla_comparativa, c(resultado$s_muestra1[4,1], resultado$s_muestra1[4,2], resultado$s_sd_muestra1[1], resultado$s_sd_muestra1[2]))
  
  tabla_comparativa <- rbind(tabla_comparativa, c(resultado$s_muestra2[4,1], resultado$s_muestra2[4,2], resultado$s_sd_muestra2[1], resultado$s_sd_muestra2[2]))
  
  tabla_comparativa <- rbind(tabla_comparativa, c(resultado$s_muestra3[4,1], resultado$s_muestra3[4,2], resultado$s_sd_muestra3[1], resultado$s_sd_muestra3[2]))
  
  row.names(tabla) <- c("Población", "Muestra1", "Muestra2", "Muestra3")
  
  return(tabla_comparativa)
}
