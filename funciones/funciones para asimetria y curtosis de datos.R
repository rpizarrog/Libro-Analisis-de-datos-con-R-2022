# Librerías
# Calcular la curtosis con la función kurtosis del paquete e1071
library(e1071)

# Función para calcular asimetria conforme fórmula
# La unción recibe los datos y devuelve el valor de la asimetría
# Los datos devueltos deben ser similares a la función skewness de la librería e1071 
f_asimetria <- function(datos) {
  n = length(datos) # Número de datos
  media = mean(datos) # Media de los datos
  desv_std = sd(datos) # Desviación estándar de los datos
  asimetria = (n / ((n-1)*(n-2))) * sum(((datos - media) / desv_std) ^ 3) # Cálculo de la asimetría
  return (asimetria)
}


# Función para calcular curtosis conforme fórmula
# La función recibe los datos y devuelve el valor de la curtosis. 
# El valor es similar al valor de la función  
f_curtosis <- function(datos) {
  n <- length(datos) # Número de datos
  media <- mean(datos) # Media de los datos
  desv_std <- sd(datos) # Desviación estándar de los datos
  suma <- sum(((datos - media) / desv_std) ^ 4) # Suma de los datos normalizados a la cuarta potencia
  curtosis <- (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * suma - (3 * (n - 1)^2 / ((n - 2) * (n - 3)))
  return(curtosis)
}

# Función para calcular curtosis conforme fórmula de lenguaje R del paquete e1071
# La funcion recibe los datos y devuelve el valor de la curtosis. 
# El valor es similar al valor de la función  
f_curtosis_r <- function(datos) {
  n <- length(datos) # Número de datos
  media <- mean(datos) # Media de los datos
  desv_std <- sd(datos) # Desviación estándar de los datos
  suma <- sum(((datos - media) / desv_std) ^ 4) # Suma de los datos normalizados a la cuarta potencia
  curtosis <- (1/n) * suma - 3
  return(curtosis)
}




