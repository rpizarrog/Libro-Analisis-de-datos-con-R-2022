# Librerías: 
library(dplyr)
library(mosaic)
library(ggplot2)  # Para gráficos
library(cowplot) #Imágenes en el mismo renglón
library("visualize")

options(scipen=999) # Notación normal

# Funciones para gráficos

# Construye diagrama de dispersión 
# octubre 2023
f_diag.dispersion <- function (datos) { 
  # datos <- data.frame(datos)
  nom.x = colnames(datos[1])
  nom.y = colnames(datos[2])
  x = datos[,1]
  y = datos[,2]
  
  media.x <- round(mean(x, na.rm = TRUE), 4)
  media.y <- round(mean(y, na.rm = TRUE), 4)
  
  ggplot() +
    geom_point(aes(x = x, y = y), col='red') +
    geom_vline(xintercept = media.x, col='blue') +
    geom_hline(yintercept = media.y, col='blue') +
    ggtitle(label = paste("Dispersión de ", nom.x, " y ", nom.y) , 
            subtitle = paste("Media ", nom.x, " =", media.x, 
                             " , ", "Media ", nom.y, "=", media.y))+
    xlab( nom.x)+
    ylab( nom.y)
  
}

# Construye diagrama de dispersión 
# Recibe unos datos de dos variables y la correlación de Pearson 
# que se utiliza para mostrarla en el diagrama de dispersión
# octubre 2023
f_diag.dispersion_r_Pearson <- function (datos, r) { 
  # datos <- data.frame(datos)
  nom.x = colnames(datos[1])
  nom.y = colnames(datos[2])
  x = datos[,1]
  y = datos[,2]
  
  media.x <- round(mean(x, na.rm = TRUE), 4)
  media.y <- round(mean(y, na.rm = TRUE), 4)
  
  ggplot() +
    geom_point(aes(x = x, y = y), col='red') +
    geom_vline(xintercept = media.x, col='blue') +
    geom_hline(yintercept = media.y, col='blue') +
    ggtitle(label = paste("Dispersión de ", nom.x, " y ", nom.y) , 
            subtitle = paste("Me", nom.x, "=", media.x, 
                             ";", "Me", nom.y, "=", media.y, 
                             "; r Pearson=", round(r, 4) )) +

    xlab(nom.x) +
    ylab(nom.y)
  
}

# Función para crear tabla para identificar la correlación de Pearson
# La función recibe un data.frame de dos variable numéricas 
# Devuelve la tabla con los cálculos que determina la covarianza
# Devuelve la sumatoria de la columna multiplicación que es la covarianza
# Devuelve la división de la covarianza entre el producto de las desviaciones estádnar 
# de las dos variables que es la correlación de Pearson
f_construye_tabla_Pearson <- function (datos) { 
  xi <- datos[, 1]
  yi <- datos[, 2]
  n <- nrow(datos)
  tabla <- data.frame(i = 1:n, xi, yi, 
                      x_med = mean(xi), 
                      y_med = mean(yi))
  
  tabla <- cbind(tabla, ximenos_x_med = tabla$xi - mean(xi))
  tabla <- cbind(tabla, yimenos_y_med = tabla$yi - mean(yi))
  
  tabla <- cbind(tabla, multiplica = (tabla$xi - mean(xi)) * (tabla$yi - mean(yi)))
  # tabla <- rbind(tabla, apply(tabla, 2, sum))
  #tabla[nrow(tabla), c(1:7)] <- '***'
  #row.names(datos[n+1, ]) = 'Suma'
  
  suma_multiplica <- sum(tabla[,'multiplica'])
  covarianza <- suma_multiplica / (n-1)
  r_pearson <- covarianza / (sd(xi) * sd(yi))  # r Pearson
  # kable(tabla, caption = paste("Construyendo tabla para correlación de Pearson. Datos", "algo"))
    
  lista <- list(tabla = tabla, covarianza = covarianza, r_pearson =   r_pearson)
  return (lista)
}

f_prueba_significancia_corr <- function(r, n) {
  t <- (r * sqrt(n-2))/ (sqrt(1 - r^2))
  t
}


# Función que genera el gráfico de contraste para prueba de signficancia de la correlaicón
# Recibe el valor de t calculado,  el valor de n 
# Recibe el nivel de confianza en valor relativo 0.90, 0.95 0 .099
# Devuele el gráfico de contraste t contra valores críticos
# Octubre 2023
f_diag_prueba_signif_corr <- function (t, n, confianza, cola = 1) {
  # confianza = 0.95
  t.critico <- abs(qt(p = (1 - confianza) / 2, df = n-2, lower.tail = FALSE))
  t.critico
  t_critico_izq = -(t_critico)
  t_critico_der = +(t_critico) # No importa el signo +, es valor absoluto
  conf <- paste(as.character(confianza * 100), "%", sep = "")
  
  if (cola == 1) { # a dos colas
    
    if (t < t_critico_izq | t > t_critico_der) 
      decision <- paste("t=",round(t, 4),"fuera del intervalo; se rechaza H0")
    if (t >= t_critico_izq & t <= t_critico_der) 
      decision <- paste("t=",round(t, 4),"dentro del intervalo; se acepta H0")
    
    visualize.t(stat = c(t_critico_izq, t_critico_der), df = n-2, section = "tails") +
        text(0, 0.2, conf, col = "red") +
        text(0, 0.1, decision, col="red", cex = .6) +
        abline(v = t, col = "red", lwd = 3, lty = 2)
  }
}

# 22-11-2022
# Genera gráfica dispersión tabla y estadísticos como 
# la medias de x e y, desviación std., covarianza, correlación
f_reg_lineal_simple <- function (datos) {
  
  tabla <- datos
  media_x <- round(mean(datos[, 1], na.rm = TRUE), 4)
  media_y <- round(mean(datos[, 2], na.rm = TRUE), 4)
  n <- nrow(datos)
  
  
  tabla <- cbind(tabla , media_x, media_y)
  tabla <- cbind(tabla, Xi_menos_media.x = tabla[,1] - media_x, 
                 Yi_menos_media.y = tabla[, 2] - media_y)
  tabla <- cbind(tabla, Xi_menos_media.x_cuad = tabla$Xi_menos_media.x^2, 
                 Yi_menos_media.y_cuad = tabla$Yi_menos_media.y^2)
  tabla <- cbind(tabla, Xi_menos_media.x_POR_Yi_menos_media.y = tabla$Xi_menos_media.x * tabla$Yi_menos_media.y)
  tabla <- rbind(tabla, round(apply(tabla, MARGIN = 2, sum), 4))
  
  row.names(tabla) <- c(1:(nrow(tabla) - 1), 'Sumatorias')
  columnas <- colnames(datos)
  columnas <- c(columnas, c("media.x", "media.y", 
                            "x_menos_media.x", "y_menos_media.y",
                            "x_menos_media.x_cuad", "y_menos_media.y_cuad",
                            "x_menos_media.x_cuad_POR_y_menos_media.y_cuad"))
 # columnas
  
  colnames(tabla) <- columnas
  
  desv.std_x <-sqrt(tabla[nrow(tabla), 7] / (n- 1))
  desv_std_y <-sqrt(tabla[nrow(tabla), 8] / (n- 1)) 
  covarianza <- tabla[nrow(tabla), 9] / (n- 1)
  r <- covarianza / (desv.std_x * desv_std_y)
  
  estadisticos <- data.frame(media_x = media_x, media_y = media_y,
                             desv.std_x, desv_std_y, 
                         covarianza, r)
  colnames(estadisticos) <- c("media_x", "media.y", "desv.std.x", "desv.std.y", 
                              "Covarianza", "Correlación")
  
  regresion <- list(tabla = tabla, 
                    n = n, 
                    dispersion = f_diag.dispersion(datos[,c(1,2)]),
                    estadisticos = estadisticos)
  
  return (regresion)
}

# 22-11-2022
# Genera la linea de tendencia lineal, recibe los datos y un modelo construído
f_linea_tendencia_reg_lineal <- function(datos, modelo) {
  ggplot(data = datos) + 
    geom_point(aes(x = datos[,1], y = datos[,2]), colour='blue') +
    geom_point(aes(x= mean(datos[,1]), y = mean(datos[,2])), col = 'green') +
    geom_line(aes( x = datos[,1], y = predict(modelo, datos)), color = "red") +
    xlab(colnames(datos[1])) + 
    ylab(colnames(datos[2])) + 
    ggtitle("Linea de tendencia sobre Conjunto de Datos")
}




