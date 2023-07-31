# Funciones para niveles d confianza y z
# Niveles de confianza y valores de z dos colas

# Esta función devuelve valores de z a niveles de confianza de 90 a 99
f_tabla_confianza_z <- function() {
  confianza <- NULL
  z <- NULL
  for (i in 0:9) {
    conf <- i+90
    confianza[i+1] <- conf
    z[i+1] <- (1 - (conf / 100)) / 2
  }
  
  
  confianza.z <- data.frame(coeficiente_confianza = confianza, valor_z = z)
  
  return (confianza.z)
}

# Esta función devuelve valo de z a un coeficiente de confianza específico
f_confianza_z <- function(confianza) {
    z <- (1 - (confianza / 100)) / 2
    return (z)
}
