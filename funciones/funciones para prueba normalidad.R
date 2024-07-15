# Funciones para prueba KS
f_prueba_ks <- function(datos) {
  datos <- sort(datos)
  media <- mean(datos)
  ds <- sd(datos)
  n <- length(datos)
  fn <- fx <- NULL
  
  for (i in 1:n) {
    fn[i] <- (i -1)/ n
    fx[i] <- pnorm(datos[i], media, ds)
  }
  
  # Cálculo de las diferencias absolutas y el valor D
  diferencias <- abs(fn - fx)
  D <- max(diferencias)
  
  tabla <- data.frame(i=1:n, x = datos, fn=fn, fx = fx, diff = diferencias)
  
  return (list(table = tabla, D = D))
}

# Función para p_value
# Función para calcular el valor p de la distribución de Kolmogorov
f_kolmogorov_p_value <- function(D, n) {
  # Ajuste por el tamaño de la muestra
  d = D * sqrt(n)
  
  # Inicialización de la suma
  sum_term = 0
  k_max = 100  # número suficiente de términos para asegurar la convergencia
  
  # Suma de la serie
  for (k in 1:k_max) {
    sum_term = sum_term + (-1)^(k-1) * exp(-2 * k^2 * d^2)
  }
  
  # Cálculo del valor p
  p_value = 1 - 2 * sum_term
  
  return(p_value)
}

# Función para prueba Anderson-Darling 
# recibe un conjunto de datos y devuelve el valor de A de la prueba AD
f_pruebaAD <- function(datos) {
  # Ordenar los datos
  datos_ordenados <- sort(datos)
  
  # Tamaño de la muestra
  n <- length(datos)
  
  # Calcular los parámetros de la distribución normal
  media <- mean(datos)
  desviacion <- sd(datos)
  
  # Calcular los valores de la CDF teórica en los puntos de datos ordenados
  F <- pnorm(datos_ordenados, mean = media, sd = desviacion)
  
  # Calcular S
  S <- sum((2 * (1:n) - 1) * (log(F) + log(1 - rev(F))) / n)
  
  # Calcular el estadístico A^2
  A2 <- -n - S
  
  # Imprimir el valor de A^2
  cat("Estadístico A^2:", A2, "\n")
  
  # Opcional: Devolver el valor de A^2
  return(A2)
}
