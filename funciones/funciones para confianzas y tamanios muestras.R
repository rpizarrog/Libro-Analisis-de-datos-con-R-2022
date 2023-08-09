# Funciones para niveles d confianza y z
# Niveles de confianza y valores de z dos colas

# Esta función devuelve valores de z a niveles de confianza de 90 a 99
# Julio 2023
f_tabla_confianza_z <- function() {
  confianza <- NULL
  alfa <- NULL
  z <- NULL
  for (i in 0:9) {
    conf <- i+90
    confianza[i+1] <- conf
    alfa[i+1] <- (1 - (conf / 100)) / 2
    z <- abs(qnorm(p = alfa))
  }
  
  
  confianza.z <- data.frame(confianza, alfa = alfa, z = z)
  
  return (confianza.z)
}

# Esta función devuelve valo de z a un coeficiente de confianza específico
# Julio 2023
f_confianza_z <- function(confianza) {
    alfa <- (1 - (confianza / 100)) / 2
    z <- abs(qnorm(p = alfa))
    return (z)
}

# Recibe coeficiente de confianza, valor de desviaión estándar y margen de error y 
# devuelve tamaño de muestra
# Julio 2023
f_nmuestra_coef_desv_E <- function(confianza, desv_std, E) {
  n = (f_confianza_z(confianza)^2 * desv_std^2) / E^2
  return (n)
}

# Recibe coeficiente de confianza, 
# el margen de error y la proporción p
# devuelve tamaño de muestra
# Julio 2023
f_nmuestra_coef_E_p <- function(confianza, E, p) {
  n = (f_confianza_z(confianza)^2 * p * (1-p)) / E^2
  return (n)
}

# Recibe coeficiente de confianza, 
# el margen de error, la proporción p y el valor de la población N
# devuelve tamaño de muestra
# Julio 2023
f_nmuestra_coef_E_p_N <- function(confianza, E, p, N) {
  numerador <- (f_confianza_z(confianza)^2 * N * p * (1-p))
  denominador <- (E^2 * (N-1) + f_confianza_z(confianza)^2 * p * (1-p))
  n = numerador / denominador
  return (n)
}

# Recibe coeficiente de confianza, 
# el margen de error, la proporción p y el valor de la población N
# devuelve tamaño de muestra
# Julio 2023
# Es alternativa 2
f_nmuestra_2_coef_E_p_N <- function(confianza, E, p, N) {
  numerador <- (f_confianza_z(confianza)^2 * p * (1-p) / E^2)
  denominador <- 1 + (f_confianza_z(confianza)^2 * p * (1-p)) / (E^2 * N)
  n = numerador / denominador
  return (n)
}

# Función para desplegar el histograma y densidad 
# De una población o muestra
# Se reciben data.frames con dos columnas en donde la segunda contiene la variable de interés
# Se incorporan las leyendas adecuadas
# Del contexto de la población y de la muestra es el mismo
# Los errores muestrales de la media aritmética y de la desviación estándar
# Julio 2023
f_hist_dens_em <- function(poblacion, muestra, contexto="datos de") {
  poblacion
  # Los parámetros de la población
  minimo.p <- min(poblacion[,2])
  maximo.p <- max(poblacion[,2])
  media.p <- round(mean(poblacion[,2]),4)
  desv.p <- round(sd(poblacion[,2]),4)
  
  muestra
  # Los parámetros de la muestra
  minimo.m <- min(muestra[,2])
  maximo.m <- max(muestra[,2])
  media.m <- round(mean(muestra[,2]),4)
  desv.m <- round(sd(muestra[,2]),4)
  
  # Los errores muestrales
  # Solo se visualizarán la media aritmética y la desviación std
  dif.media <- abs(round(media.p - media.m, 4))
  dif.desv <- abs(round(desv.p - desv.m, 4))
  dif.max <- abs(round(maximo.p - minimo.p, 4))
  dif.min <- abs(round(minimo.p - minimo.m, 4))
  
  # Construyendo la tabla para identificar los errores muestrales
  datos_err_muestrales <- data.frame("parámetros" = c(media.p, desv.p, maximo.p, minimo.p), "estadísticos" = c(media.m, desv.m, maximo.m, minimo.m))
  
  rownames(datos_err_muestrales) <- c("media", "desv. std.", "máximo", "mínimo")
  tabla_err_muestrales <- cbind(datos_err_muestrales, "err muestral" = c(dif.media, dif.desv, dif.max, minimo.p))  
  
  
  # Construyen el histograma de población y de muestra
  # Histograma con densidad
   
  # La población
  gp <- ggplot(poblacion, aes(x = poblacion[,2])) + 
    geom_histogram(aes(y = ..density..),
                   bins = 30, colour = 1, fill = "gray") +
    labs(title = paste("Población", contexto),
         subtitle = paste("ME=", media.p, "; ds=", desv.p),
         caption = "Fuente propia") + 
    geom_vline(xintercept = media.p, col='red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media.p - desv.p, col='blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media.p + desv.p, col='blue', linetype = "dashed", size = 1) +
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2)
  
    gp <- gp + labs(x = colnames(poblacion[2]))
  
    gp <- gp + theme(
    plot.title = element_text(color = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "black",size=6)
  )

    # La muestra
  gm <- ggplot(muestra, aes(x = muestra[,2])) + 
    geom_histogram(aes(y = ..density..),
                   bins = 30, colour = 1, fill = "gray") +
    geom_vline(xintercept = media.m, col='red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media.m - desv.m, col='blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media.m + desv.m, col='blue', linetype = "dashed", size = 1) +
    labs(title = paste("Muestra",contexto),
         subtitle = paste("me=", media.m, "; ds.=",desv.m,"; er me=",dif.media,"; er ds=",dif.desv), 
         caption = "Fuente propia") +
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2)
  gm <- gm + labs(x = colnames(poblacion[2]))
  
  gm <- gm + theme(
    plot.title = element_text(color = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "black",size=6)
  )
  
  lista <- list(tabla_err_muestrales = tabla_err_muestrales, gp = gp, gm = gm)
  
  return (lista)
}


# Funicón 
# Julio 2023
f_genera_muestras <- function (poblacion, q, n) {
  
  # Genera y construye las muestras
  muestras = as.list(NULL)
  m_muestras = NULL
  for (i in 1:q) {
    muestras[[i]] <- sample(x = poblacion[,2], size = n, replace = FALSE)
    
    m_muestras[i] <- mean(muestras[[i]])
  }
  
  media_muestral = mean(m_muestras)
  
  # Construye la distribución muestral
  lasmuestras <- data.frame(muestras)
  lasmuestras <- data.frame(t(lasmuestras))
  colnames(lasmuestras) <- paste0(colnames(poblacion[2]),1:n)
  rownames(lasmuestras) <- paste0("M",1:q)
  tabla_dist_muestral <- data.frame(lasmuestras[,1:3], "..."="...", lasmuestras[,(n-2):n], medias_muestrales = m_muestras)
  
  
  lista <- list(muestras = muestras, m_muestras = m_muestras, 
                media_muestral = media_muestral,
                tabla_dist_muestral = tabla_dist_muestral)
  
  return(lista)
  
}


# Recibe un conjunto de datos con una estructura de 2 columnas y N registros
# La primera columna es un valor consecutivo y 
# La segunda columna es la variable de interés
# Devuelve histograma
# Julio 2023
f_histograma <- function (datos, contexto) {
  media <- round(mean(datos[,2]), 4)
  desv_std <- round(sd(datos[,2]), 4)
  
  g <- ggplot(datos, aes(x = datos[,2])) + 
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "gray", bins = 30) +
    labs(title = paste("Datos de ", contexto),
         subtitle = paste("N=",nrow(datos)," Me=", media, "ds", desv_std),
         caption = "Fuente propia") +  
    geom_vline(xintercept = media, col='red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media - desv_std, col='blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = media + desv_std, col='blue', linetype = "dashed", size = 1) +
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2)
  
    geom_density(lwd = 1.2,
                 linetype = 2,
                 colour = 2)
  g <- g + theme(
    plot.title = element_text(color = "black", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "black",size=6),
    plot.caption = element_text(color = "black", face = "italic", size=6)
  )
  
  g <- g + labs(x = colnames(datos[2]))
  
  return (g)

}

# Se calcula la desviación estándar de la población
f_calcular_ds <- function(poblacion) {
  media_p <- mean(poblacion)
  ds <- sqrt((sum((poblacion - media_p) ^ 2)) / (N))
  return(ds)
}


# Recibe vector que contienen la población y muestra, 
# además del tamaño de cada muestra 
# Devuelven el ua lista con rtes estadísticos:
# error estándar de una población infinita, desviación estándar de la población y 
# desviación de la distribución muestral o la muestra.
# Agosto 2023
f_er_std_finitas <- function(poblacion, muestra, n) {
  N <- length(poblacion)

  desv_std_p <- sd(poblacion)
  desv_std_m <- sd(muestra)
  er_std <- sqrt((N-n)/(N-1)) * (desv_std_p / sqrt(n))
  
  lista <- list(er_std = er_std, desv_std_p = desv_std_p, desv_std_m = desv_std_m)
  return(lista)
}


# Reciben la desviación estándar de una población no conocida y 
# un vector con la muestra
# Devuelven el error estándar para población infinita
f_er_std_infinitas <- function(desv_std_p, muestra, n) {
  
  er_std <- sd(desv_std_p / n)
  desv_std_m <- sd(muestra)
  
  lista <- list(er_std = er_std, desv_std_p = desv_std_p, desv_std_m = desv_std_m)
  return(lista)
}