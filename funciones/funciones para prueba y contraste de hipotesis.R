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
# recibe el valor de z previamente calculado con la función f_devoler_z_prueba()
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


# Septiembre 2023
# La función f_probar_hipotesis_p_z() devuelve la decisión de aceptar o rechazar la hipótesis nula
# Recibe los parámetros de z obtenido a partir de función f_dvevoler_z_prueba(), 
# recibe el valor de significancia que por default es 0.05, pero pudiera ser 0.10, 0.01, 0.001 u otro y 
# recibe el valor del tipo de hiótesis, si es a dos colas el valor es 1, 
# cola a la izquierda el valor es 2 y cola a la derecha el valor es 3. 
# Por omisión, el valor por default es a dos colas = 1
f_probar_hipotesis_p_z <- function(z, significancia, cola=1) {
  decision <- "Se acepta Ho"
  rechaza <- NULL
  if (isTRUE(all.equal(significancia, 0.10))) {
    rechaza <- "hay cierta evidencia de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.05))) {
    rechaza <- "hay evidencia fuerte de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.01))) {
    rechaza <- "hay evidencia muy fuerte de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.001))) {
    rechaza <- "hay evidencia extremadamente fuerte de que H0 no es verdadera"
  }
  if (cola == 1) {
    # dos colas
    p <- 1 - pnorm(abs(z))
    p.valor = 2 * p
  }
  
  if (cola == 2) {
    # dos colas
    p <- pnorm(z)
    p.valor = p
  }
  if (cola == 3) {
    # dos colas
    p <- pnorm(z, lower.tail = FALSE)
    p.valor = p
  }
  
  if (p.valor < significancia ) {
    decision = paste("Se rechaza Ho porque ", rechaza)
  }
  
  lista = list(p.valor = p.valor, decision = decision)
  
  return(lista)
}



# Septiembre 2023
# La función recibe los parámetros de acuerdo a la fórmula y devuelve 
# el valor de t de prueba para su contraste.
# Recibe la media de la muestra, el valor de la media poblacional a comparar,
# la desviación estándar conocida de la muestra y el valor 
# de n tamaño de la muestra pequeña menor o igual a 30.
# Devuelve el valor de t.
f_devolver_t_prueba <- function(media_m, desv_std_m, media_p, n) {
  t <- (media_m - media_p) / (desv_std_m / sqrt(n))
  t
}

# Septiembre 2023
# Recibe los parámetros del de confianza en valores relativos 
# entre 0 y 1, por ejemplo 0.90, 0.95, 0.99; 
# recibe el valor de t previamente calculado con la función f_devoler_t_prueba()
# # recibe la expresión de una hipótesis en caso de que no se proporcione
# la expresión por omisión en la llamada de la función de la hipótesis nula es 
# "Realidad actual verdadera"
# recibe el parámetro de cola dependiendo el tipo de hipótesis a tratar:
# a dos colas = 1, cola izquierda = 2, cola derecha = 3, 
# por definición default es a dos colas, 
# finalmente recibe el valor de n que define los grados de libertad 
# Devuelve una lista con la gráfica de aceptación o rechazo de Ho y la decisión de Ho
f_probar_hipotesis_t <- function(confianza, t, h0_string ="Realidad actual verdadera", cola = 1, n) {
  library(visualize)
  
  H0 <- NULL
  gl <- (n -1)
  if (cola == 1)  { 
    # Dos colas
    alfa <- (1 - confianza) / 2
    t.critico <- qt(p = alfa, df = gl)
    t.critico <- abs(t.critico)
    t.critico
    
    tipo_cola <- "dos colas"
    g <- visualize.t(stat = c(-t.critico, t.critico), section = "tails") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = t, col='red', lwd = 1, lty= 4)
    
    if (t > -t.critico & t < t.critico) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
    
  }
  if (cola == 2)  { 
    # Cola a la derecha
    alfa <- (1 - confianza) 
    t.critico <- qt(p = alfa, df = gl)
    t.critico
    
    tipo_cola <- "cola a la izquierda"
    g <- visualize.t(stat = c(t.critico), section = "lower") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = t, col='red', lwd = 1, lty= 4)
    if (t > t.critico ) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
  }
  if (cola == 3)  { 
    # Cola a la derecha
    alfa <- (1 - confianza) 
    t.critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
    t.critico
  
    tipo_cola <- "cola a la derecha"
    g <- visualize.t(stat = c(t.critico), section = "upper") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = t, col='red', lwd = 1, lty= 4)
    
    if (t < t.critico ) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
  }
  
  lista <- list(g = g, H0 = H0)
  return(lista)
}

# Septiembre 2023
# La función f_probar_hipotesis_p_t() devuelve la decisión de aceptar o rechazar la hipótesis nula
# Recibe los parámetros de t obtenido a partir de función f_devoler_t_prueba(), 
# recibe el valor de significancia que por default es 0.05, pero pudiera ser 0.10, 0.01, 0.001 u otro y 
# recibe el valor del tipo de hipótesis, si es a dos colas el valor es 1, 
# cola a la izquierda el valor es 2 y cola a la derecha el valor es 3. 
# Por omisión, el valor por default es a dos colas = 1
# Recibe el valor de n que define los grados de libertad

f_probar_hipotesis_p_t <- function(t, significancia, cola=1, n) {
  decision <- "Se acepta Ho"
  rechaza <- NULL
  gl <- (n-1)
  if (isTRUE(all.equal(significancia, 0.10))) {
    rechaza <- "hay cierta evidencia de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.05))) {
    rechaza <- "hay evidencia fuerte de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.01))) {
    rechaza <- "hay evidencia muy fuerte de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.001))) {
    rechaza <- "hay evidencia extremadamente fuerte de que H0 no es verdadera"
  }
  if (cola == 1) {
    # dos colas
    p <- 1 - pt(abs(t), df = gl)
    p.valor = 2 * p
  }
  
  if (cola == 2) {
    # dos colas
    p <- pt(t, df = gl)
    p.valor = p
  }
  if (cola == 3) {
    # dos colas
    p <- pt(t, df = gl, lower.tail = FALSE)
    p.valor = p
  }
  
  if (p.valor < significancia ) {
    decision = paste("Se rechaza Ho porque ", rechaza)
  }
  
  lista = list(p.valor = p.valor, decision = decision)
  
  return(lista)
}


# Septiembre 2023
# La función f_devolver_chisq_prueba recibe los parámetros de acuerdo a la fórmula y devuelve 
# el valor de chi cuadrada de prueba para su contraste en 
# prueba de hipótesis de variabilidad de los datos
# Recibe la varianza de la muestra,
# la varianza de la población o valor hipótetico a comparar en la H0, 
# de n tamaño de la muestra para calcular los grados de libertad
# Devuelve el valor de prueba de chi cuadrada en p_chisq.
f_devolver_chisq_prueba <- function(var_m, var_p, n) {
  p_chisq <- (gl * var_m) / var_a_comparar
  p_chisq
}



# Septiembre 2023
# La función f_probar_hipotesis_chisq()
# Recibe los parámetros del de confianza en valores relativos 
# entre 0 y 1, por ejemplo 0.90, 0.95, 0.99; 
# recibe el valor de p_chisq previamente calculado con la función f_devolver_chisq_prueba()
# recibe la expresión de una hipótesis en caso de que no se proporcione
# la expresión por omisión en la llamada de la función de la hipótesis nula es 
# "Realidad actual verdadera"
# recibe el parámetro de cola dependiendo el tipo de hipótesis a tratar:
# a dos colas = 1, cola izquierda = 2, cola derecha = 3, 
# por definición default es a dos colas, 
# finalmente recibe el valor de n que define los grados de libertad 
# La función utiliza visualize.chisq() para representar el valor de p_chisq y los valores críticos
# Devuelve una lista con la gráfica de aceptación o rechazo de Ho y la decisión de Ho
f_probar_hipotesis_chisq <- function(confianza, p_chisq, h0_string ="Realidad actual verdadera", cola = 1, n) {
  library(visualize)
  
  H0 <- NULL
  gl <- (n - 1)
  if (cola == 1) {
    alfa <- (1 - confianza) 
    chisq_critico_izq = qchisq(p = alfa/2, df = gl)
    chisq_critico_der = qchisq(p = 1 - alfa/2, df = gl)
    
    # Visualizar
    tipo_cola <- "dos colas"
    g <- visualize.chisq(stat = c(chisq_critico_izq, chisq_critico_der), df = gl, section = "tails") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "\n",  "alfa / 2 = ", 
                         (1 - confianza) /  2, "\n", "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = p_chisq, col='red', lwd = 1, lty= 4)  
    
    if (p_chisq > -chisq_critico_izq & p_chisq < chisq_critico_der) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
    
  }
  if (cola == 2) {
    alfa <- (1 - confianza) 
    tipo_cola <- "cola izquierda"
    chisq_critico = qchisq(p = alfa, df = gl)
    
    # Visualizar
    g <- visualize.chisq(stat = c(p_chisq), df = gl, section = "lower") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = chisq_critico, col='red', lwd = 1, lty= 4)  
    
    if (p_chisq > chisq_critico) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
  }
  if (cola == 3) {
    alfa <- (1 - confianza)
    tipo_cola <- "cola derecha"
    chisq_critico = qchisq(p = alfa, df = gl, lower.tail = FALSE)

    # Visualizar
    g <- visualize.chisq(stat = c(p_chisq), df = gl, section = "upper") +
      text(0, 0.2, paste(tipo_cola, "\n", confianza * 100, "%", "\n", 
                         "alfa=", (1 - confianza), "Acepta Ho", sep = ""),  col = "black") + 
      abline(v = chisq_critico, col='red', lwd = 1, lty= 4) 
    if (p_chisq < chisq_critico) {
      H0 <- paste("Se acepta la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    } else {
      H0 <- paste("Se rechaza la hipótesis nula Ho:",h0_string,"con nivel de confianza", confianza*100,"%", "a",tipo_cola)
    }
    
  }
  
  lista <- list(g = g, H0 = H0)
  return(lista)
}


# Septiembre 2023
# La función f_probar_hipotesis_p_chisq() devuelve la decisión de aceptar o rechazar la hipótesis nula
# basado en la comparación de el valor de prueba chisq contra el valor de significncia
# Recibe los parámetros de p_chisq obtenido a partir de función f_devolver_chisq_prueba(), 
# recibe el valor de significancia que por default es 0.05, pero pudiera ser 0.10, 0.01, 0.001 u otro y 
# recibe el valor del tipo de hipótesis, si es a dos colas el valor es 1, 
# cola a la izquierda el valor es 2 y cola a la derecha el valor es 3. 
# Por omisión, el valor por default es a dos colas = 1
# Recibe el valor de n que define los grados de libertad

f_probar_hipotesis_p_chisq <- function(p_chisq, significancia, cola=1, n) {
  decision <- "Se acepta Ho"
  rechaza <- NULL
  gl <- (n-1)
  if (isTRUE(all.equal(significancia, 0.10))) {
    rechaza <- "hay cierta evidencia de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.05))) {
    rechaza <- "hay evidencia fuerte de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.01))) {
    rechaza <- "hay evidencia muy fuerte de que H0 no es verdadera"
  }
  if (isTRUE(all.equal(significancia, 0.001))) {
    rechaza <- "hay evidencia extremadamente fuerte de que H0 no es verdadera"
  }
  if (cola == 1) {
    # dos colas
    p <- pchisq(q = p_chisq, df = gl) # la probabilidad de chi cuadrada, 
    p.valor = 2 * p
  }
  
  if (cola == 2) {
    # dos colas
    p <- pchisq(q = p_chisq, df = gl)
    p.valor = p
  }
  if (cola == 3) {
    # dos colas
    p <- pchisq(q = p_chisq, df = gl, lower.tail = FALSE)
    p.valor = p
  }
  
  if (p.valor < significancia ) {
    decision = paste("Se rechaza Ho porque ", rechaza)
  }
  
  lista = list(p.valor = p.valor, decision = decision)
  
  return(lista)
}


# Septiembre 2023
# Función para visualizar densidad de chi cuadrada 
# Dependiendo si es hipótesis a dos colas cola izquierda o cola derecha
# pone linea punteada de color rojo con el valor de prueba p_chisq


f_probar_hipotesis_chisq_curve <- function(p_chisq, confianza, n, cola = 1) {
  gl <- (n-1)
  if (cola == 1) { # dos colas
    alfa <- (1 - confianza) / 2
    chisq_critico_izq = qchisq(p = alfa, df = gl)
    chisq_critico_der = qchisq(p = alfa, df = gl, lower.tail = FALSE)
    
    # Visualizar con curve a dos colas
    x <- 0:(chisq_critico_der + 10)
    min <- min(x)
    max <- max(x)
    # Graficar la densidad de la distribución chi cuadrada
    curve(dchisq(x, df = gl), from = min, to = max,
          main = paste("Valores críticos y valor de prueba chisq"), 
          sub=paste("Crítico izq=", round(chisq_critico_izq, 2), ";", "der=", round(chisq_critico_der, 2), "; chisq =", round(p_chisq, 4),"; gl=", gl),
          ylab = 'Densidad', lwd = 2, col = 'steelblue') 
    
    # Izquierda
    # Crear vector de valores izquierda x1 para dos colas
    x_vector_i <- seq(from = min, to = chisq_critico_izq, by = 0.01)
    
    # Crear vector de valores de densidad chi-cuadrado para el intervalo
    p_vector_i <- dchisq(x_vector_i, df = gl)
    
    # Rellenar el área bajo la curva del intervalo de confianza
    
    polygon(x = c(min, x_vector_i, chisq_critico_izq), y = c(min, p_vector_i, min), col = "orange", border = 'red')
    
    
    # Derecha
    # Crear vector de valores izquierda x1 para dos colas
    x_vector_d <- seq(from = chisq_critico_der, to = max, by = 0.01)
    
    # Crear vector de valores de densidad chi-cuadrado para el intervalo
    p_vector_d <- dchisq(x_vector_d, df = gl)
    
    # Rellenar el área bajo la curva del intervalo de confianza
    polygon(x = c(chisq_critico_der, x_vector_d, max), y = c(min, p_vector_d, min), col = "orange", border = 'red') 
    
    abline(v = p_chisq, col='red', lwd = 1, lty= 4)

  }
  
  if (cola == 2) { # cola izquierda
    alfa <- (1 - confianza) 
    chisq_critico_izq = qchisq(p = alfa, df = gl)
    #chisq_critico_der = qchisq(p = 1 - alfa, df = gl)
    
    # VisualiZar con curve a dos colas
    x <- 0:(chisq_critico_izq + 3 * chisq_critico_izq)
    min <- min(x)
    max <- max(x)
    # Graficar la densidad de la distribución chi cuadrada
    curve(dchisq(x, df = gl), from = min, to = max,
          main = paste("Valor crítico y valor de prueba chisq"), 
          sub=paste("Crítico izq=", round(chisq_critico_izq, 2),"; chisq =", round(p_chisq, 4), "; gl=", gl),
          ylab = 'Densidad', lwd = 2, col = 'steelblue') 
    
    # Izquierda
    # Crear vector de valores izquierda x1 para dos colas
    x_vector_i <- seq(from = min, to = chisq_critico_izq, by = 0.01)
    
    # Crear vector de valores de densidad chi-cuadrado para el intervalo
    p_vector_i <- dchisq(x_vector_i, df = gl)
    
    # Rellenar el área bajo la curva del intervalo de confianza
    
    polygon(x = c(min, x_vector_i, chisq_critico_izq), y = c(min, p_vector_i, min), col = "orange", border = 'red')
    
    
    # Derecha
    # Crear vector de valores izquierda x1 para dos colas
  #  x_vector_d <- seq(from = chisq_critico_der, to = max, by = 0.01)
    
    # Crear vector de valores de densidad chi-cuadrado para el intervalo
  #  p_vector_d <- dchisq(x_vector_d, df = gl)
    
    # Rellenar el área bajo la curva del intervalo de confianza
  #  polygon(x = c(chisq_critico_der, x_vector_d, max), y = c(min, p_vector_d, min), col = "orange", border = 'red') 
    
    abline(v = p_chisq, col='red', lwd = 1, lty= 4)
    
  }
  if (cola == 3) { # cola derecha
    alfa <- (1 - confianza) 
    # chisq_critico_izq = qchisq(p = alfa, df = gl)
    chisq_critico_der = qchisq(p = alfa, df = gl, lower.tail = FALSE)
    
    # Visualiaar con curve cola derecha
    x <- 0:(chisq_critico_der + 10)
    min <- min(x)
    max <- max(x)
    # Graficar la densidad de la distribución chi cuadrada
    curve(dchisq(x, df = gl), from = min, to = max,
          main = paste("Valor crítico y valor de prueba chisq"), 
          sub=paste("Critico der=", round(chisq_critico_der, 2), "; chisq =", round(p_chisq, 4),";gl=", gl),
          ylab = 'Densidad', lwd = 2, col = 'steelblue') 
    
    # Izquierda
    # Crear vector de valores izquierda x1 para dos colas
    #x_vector_i <- seq(from = min, to = chisq_critico_izq, by = 0.01)
    
    # Crear vector de valores de densidad chi-cuadrado para el intervalo
    #p_vector_i <- dchisq(x_vector_i, df = gl)
    
    # Rellenar el área bajo la curva del intervalo de confianza
    #polygon(x = c(min, x_vector_i, chisq_critico_izq), y = c(min, p_vector_i, min), col = "orange", border = 'red')
    
    
    # Derecha
    # Crear vector de valores izquierda x1 para dos colas
    x_vector_d <- seq(from = chisq_critico_der, to = max, by = 0.01)
    
    # Crear vector de valores de densidad chi-cuadrado para el intervalo
    p_vector_d <- dchisq(x_vector_d, df = gl)
    
    # Rellenar el área bajo la curva del intervalo de confianza
    polygon(x = c(chisq_critico_der, x_vector_d, max), y = c(min, p_vector_d, min), col = "orange", border = 'red') 
    
    abline(v = p_chisq, col='red', lwd = 1, lty= 4)
    
  }
  
}




