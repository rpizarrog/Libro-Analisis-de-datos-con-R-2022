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
