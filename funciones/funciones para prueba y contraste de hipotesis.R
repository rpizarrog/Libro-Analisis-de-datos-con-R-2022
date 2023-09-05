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
# el valor de z de prueba para su contraste
f.devolver.z.prueba <- function(media_m, desv_std_m, media_p, n) {
  z <- (media_m - media_p) / (desv_p / sqrt(n))
  z
}


# Septiembre 2023
# La función recibe los parámetros de acuerdo a la fórmula y devuelve 
# el valor de t de prueba para su contraste
f.devolver.t.prueba <- function(media_m, desv_std_m, media_p, n) {
  t <- (media_m - media_p) / (desv_std_m / sqrt(n))
  t
}


