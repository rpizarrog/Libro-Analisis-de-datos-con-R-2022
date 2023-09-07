# Función para probar una hipótesis
# La función devuelve la decisión de rechazar o aceptar una hipótesis
# Recibe el valor de Ho y H1 en valores numéricos a comparar
# Recibe la expresión de una hipótesis en caso de que no se proporcione
# la expresión por omisión en la llamada de la función de la hipótesis nula es 
# "Realidad actual verdadera"
# La función devuelve la decisión en la expresión return()
# Agosto 2023
f_probar_hipotesis <- function (h0, h1, h0_string ="Realidad actual verdadera") {
  if (h1 > h0) {
    
    conclusion <- "Se rechaza Ho:"
    conclusion = paste(conclusion, "'", h0_string, "'")
  } else {
    conclusion <- "Se acepta Ho:"
    conclusion = paste(conclusion, "'", h0_string, "'")
  }
  return (conclusion)
}


# La función recibe el nivel de confianza como numérico entero
# La función recibe si hipótesis de dos colas, derecha o izquierda
# tipo =1 dos colas
# tipo =2 cola izquierda o less <=
# tipo =3 cola a la dereca greather >=
# Agosto 2023
f_probar_hipotesos_con_p <- function(confianza, p.tipo=1, z = FALSE, t = FALSE) {
  # Realizar la prueba de proporciones
  if (tipo == 1) {
    alfa <- (1 - confianza / 100) / 2
    
  }
  
  
  resultado <- prop.test(ph1, 100, p = ph0, alternative = "greater")
  
  # Imprimir el resultado
  print(result)
  
  if(result$p.value < 0.05)  {
    print ("Se rechaza H0")
  } else {
    print("Se acepta H0")
  }
  
}


# Función para probar una hipótesis
# La función devuelve la decisión de rechazar o aceptar una hipótesis
# Recibe el valor de Ho y H1 en valores numéricos a comparar
# Recibe la expresión de una hipótesis en caso de que no se proporcione
# Recibe el tipo de comparación dos colas = 1, cola izquierda 2 y cola a la derecha 3
# Por omisión el tipo es a dos colas tipo = 1, pero puede modificarse en la llamada de la función
# La expresión por omisión en la llamada de la función de la hipótesis nula es 
# "Realidad actual verdadera"
# La función devuelve la decisión en la expresión return()
# Agosto 2023
f_probar_hipotesis_all <- function (h0, h1, tipo = 1, h0_string ="Realidad actual verdadera") {
    if (h1 > h0) {
      conclusion <- "Se rechaza Ho:"
      conclusion = paste(conclusion, "'", h0_string, "'")
    } else {
      conclusion <- "Se acepta Ho:"
      conclusion = paste(conclusion, "'", h0_string, "'")
    }
    return (conclusion)
}


# Septiembre 2023
# La función recibe los parámetros de acuerdo a la fórmula y devuelve 
# el valor de z de prueba para su contraste.
# Recibe la media de la muestra, el valor de la media pobacoinal a comparar,
# la desviación estándar conocida de la población y e valor de n tamaño de la muestra.
# Devuelve el valor de z.
f_devolver_z_prueba <- function(media_m, desv_std, media_p, n) {
  z <- (media_m - media_p) / (desv_std / sqrt(n))
  z
}

# Septiembre 2023
# Recibe los parámetros del de confianza en valores relativos 
# entre 0 y 1, por ejemplo 0.90, 0.95, 0.99; 
# recibe el valor de z previamene calculado con la función f_dvevoler_z_prueba()
# # recibe la expresión de una hipótesis en caso de que no se proporcione
# la expresión por omisión en la llamada de la función de la hipótesis nula es 
# "Realidad actual verdadera"
# recibe el parámetro de cola dependiendo el tipo de hipótesis a tratar:
# a dos colas = 1, cola izquierda = 2, cola derecha = 3, 
# por definición default es a dos colas 
# Devuelve una lista con la gráfica de aceptación o rechazo de Ho y la decisión de Ho
f_probar_hipotesis_z <- function(confianza, z, h0_string ="Realidad actual verdadera", cola = 1) {
  library(visualize)
  
  H0 <- NULL
  if (cola == 1)  { 
    # Dos colas
    alfa <- (1 - confianza) / 2
    z.critico <- qnorm(p = alfa)
    z.critico <- abs(z.critico)
    z.critico
    
    tipo_cola <- "dos colas"
    g <- visualize.norm(stat = c(-z.critico, z.critico), section = "tails") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = z, col='red', lwd = 1, lty= 4)
    
    if (z > -z.critico & z < z.critico) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
    
  }
  if (cola == 2)  { 
    # Cola a la derecha
    alfa <- (1 - confianza) 
    z.critico <- qnorm(p = alfa)
    z.critico
    
    tipo_cola <- "cola a la izquierda"
    g <- visualize.norm(stat = c(z.critico), section = "lower") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = z, col='red', lwd = 1, lty= 4)
    if (z > z.critico ) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
  }
  if (cola == 3)  { 
    # Cola a la derecha
    alfa <- (1 - confianza) 
    z.critico <- qnorm(p = alfa, lower.tail = FALSE)
    z.critico
    
    tipo_cola <- "cola a la derecha"
    g <- visualize.norm(stat = c(z.critico), section = "upper") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = z, col='red', lwd = 1, lty= 4)
    
    if (z < z.critico ) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
  }
  
  lista <- list(g = g, H0 = H0)
  return(lista)
}


# Septembre 2023
# La función f_probar_hipotesis_p() devuelve la decisión de aceptar o rechazar la hipótesis nula
# Recibe los parámetros de z obtenido a partir de función f_dvevoler_z_prueba(), 
# recibe el valor de significancia que por default es 0.05, pero pudiera ser 0.10, 0.01, 0.001 u otro y 
# recibe el valor del tipo de hiótesis, si es a dos colas el valor es 1, 
# cola a la izquieura el valor es 2 y cola a la derecha el valor es 3. 
# Por omisión, el valor por deault es a dos colas = 1
f_probar_hipotesis_p <- function(z, significancia, cola=1) {
  if (tipo == 1) {
    # dos colas
    p <- 1 - pnorm(abs(z))
    p.valor = 2 * p
  }
  
  if (tipo == 2) {
    # dos colas
    p <- pnorm(z)
    p.valor = p
  }
  if (tipo == 3) {
    # dos colas
    p <- pnorm(z, lower.tail = FALSE)
    p.valor = p
  }
  
  if (p.valor < significancia & significancia == 0.10) {
    decision = "Se rechaza Ho de a"
  }
  
  lista = list(p.valor = p.valor, decision = decision)
}



# Septiembre 2023
# La función recibe los parámetros de acuerdo a la fórmula y devuelve 
# el valor de t de prueba para su contraste.
# Recibe la media de la muestra, el valor de la media poblacional a comparar,
# la desviación estándar conocida de la muestra y el valor 
# de n tamaño de la muestra pequeña menor oo igual a 30.
# Devuelve el valor de t.
f.devolver.t.prueba <- function(media_m, desv_std_m, media_p, n) {
  t <- (media_m - media_p) / (desv_std_m / sqrt(n))
  t
}


