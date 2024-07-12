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