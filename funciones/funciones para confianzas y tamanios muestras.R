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

# Esta función devuelve valor de z a un coeficiente de confianza específico
# Se recibe el valor de la confianza en valor entero 90-99
# Julio 2023
f_confianza_z <- function(confianza) {
    alfa <- (1 - (confianza / 100)) / 2
    z <- abs(qnorm(p = alfa))
    return (z)
}

# Recibe coeficiente de confianza entero, 
# Se valor de desviación estándar y margen de error en valore srelativos y 
# Se devuelve tamaño de muestra
# Julio 2023
f_nmuestra_coef_desv_E <- function(confianza, desv_std, E) {
  n = (f_confianza_z(confianza)^2 * desv_std^2) / E^2
  return (n)
}

# Recibe coeficiente de confianza, valor entero, 
# el margen de error y la proporción valores relativos
# devuelve tamaño de muestra
# Julio 2023
f_nmuestra_coef_E_p <- function(confianza, E, p) {
  n = (f_confianza_z(confianza)^2 * p * (1-p)) / E^2
  return (n)
}

# Recibe coeficiente de confianza valor entero, 
# el margen de error, la proporción p y el valor de la población N
# devuelve tamaño de muestra
# Julio 2023
f_nmuestra_coef_E_p_N <- function(confianza, E, p, N) {
  numerador <- (f_confianza_z(confianza)^2 * N * p * (1-p))
  denominador <- (E^2 * (N-1) + f_confianza_z(confianza)^2 * p * (1-p))
  n = numerador / denominador
  return (n)
}

# Recibe coeficiente de confianza en valor entero, 
# Se recibe el margen de error, la proporción p en valores relativos 
# Se recibe el valor de la población N
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


# Función que construye las n muestras
# que sirven para construir la distribución muestral de la media
# Recibe la población como data.frame con una estructura ...2 columnas y 
# la segunda columna es la variable de interés
# Recibe la cantidad de muestras a generar q
# Recibe el tamaño de cada muestra n
# Devuelve una lista con las muestras, las medias de cada muestra y la media muestral
f_genera_muestras <- function (poblacion, q, n) {
  #q = 100 # Número de muestras
    muestras = as.list(NULL)
    m_muestras = NULL
    for (i in 1:q) {
      muestras[[i]] <- sample(x = poblacion[,2], size = n, replace = FALSE)
    
      m_muestras[i] <- mean(muestras[[i]])
    }
  
    media_muestral = mean(m_muestras)
    
    # Construye la tabla de distribución muestral formateada invertida
    lasmuestras <- data.frame(muestras)
    lasmuestras <- t(lasmuestras)
    colnames(lasmuestras) <- paste0(colnames(poblacion[2]),1:n)
    rownames(lasmuestras) <- paste0("M",1:q)
    
    tabla_dist_muestral <- data.frame(lasmuestras[,1:3], "..."="...", 
                                      lasmuestras[,(n-2):n], 
                                      medias.muestrales = m_muestras)

  lista <- list(muestras = muestras, m_muestras = m_muestras, 
                media_muestral = media_muestral,
                tabla_dist_muestral = tabla_dist_muestral)

  return(lista)

}


