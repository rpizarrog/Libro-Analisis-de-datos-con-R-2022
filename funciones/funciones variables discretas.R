# Algunas funciones para variables discretas
# Función que devuelve tabla de distribución VE, Varianza y Desv. Std, 
# recibe solo los casos
# Devuelve también el gráfico de barra y el gráfico de probabilidad acumulada
f.discretas.ve.v.sd <- function(casos) {
  library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = 0:(length(casos) - 1)
  
  # Calculando columnas
  f_prob_x <- casos / n
  F_acumulado <- cumsum(f_prob_x)
  x_f_prob_x <- x * f_prob_x
  
  
  # Valor esperado
  VE <- sum(x_f_prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE_CUAD <- x_menos_VE ^ 2
  x_menos_VE_CUAD_f_prob_x <- x_menos_VE_CUAD * f_prob_x
  varianza <- sum(x_menos_VE_CUAD_f_prob_x)
  desv.std <- sqrt(varianza)
  
  
  # Tabla de distribución
  tabla <- data.frame(x, casos, f_prob_x,
                      F_acumulado, x_f_prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE_CUAD,
                      x_menos_VE_CUAD_f_prob_x
  )
  
  # Gráfica de barras
  
  g <- ggplot(data = tabla, aes(x = x, y = f_prob_x, fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title="Distribución Acumulada de Probabilidad ", 
         subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), 
         x="Variable x", y="Prob. F(x)")
  # Gráfico acumulado
  g_acumulado <- ggplot(data = tabla, aes(x = x, y=F_acumulado)) +
    geom_point(colour="blue") + 
    geom_line(colour="red") +
    labs(title="Distribución Acumulada de Probabilidad ", x="Variable x", y="Prob. F(x)")
  
  
  
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, 
                       desv.std = desv.std, 
                       g = g, 
                       g_acumulado = g_acumulado)
  
  estadisticos
  
  
}


# Similar función a la anterior solo que recibe también los valores de la variable discreta
# Algunas veces los valores de la variable discreta comienzan por debajo de 0, o no inician en cero
f.discretas.ve.v.sd.val.disc <- function(discretas, casos) {
  library(ggplot2)
  # Inicializando valores
  n <- sum(casos)
  x = discretas
  
  # Calculando columnas
  f_prob_x <- casos / n
  F_acumulado <- cumsum(f_prob_x)
  x_f_prob_x <- x * f_prob_x
  
  
  # Valor esperado
  VE <- sum(x_f_prob_x)
  
  # Varianza y Desviación Std
  x_menos_VE <- x - VE
  x_menos_VE_CUAD <- x_menos_VE ^ 2
  x_menos_VE_CUAD_f_prob_x <- x_menos_VE_CUAD * f_prob_x
  varianza <- sum(x_menos_VE_CUAD_f_prob_x)
  desv.std <- sqrt(varianza)
  
  
  # Tabla de distribución
  tabla <- data.frame(x, casos, f_prob_x,
                      F_acumulado, x_f_prob_x,
                      VE,
                      x_menos_VE,
                      x_menos_VE_CUAD,
                      x_menos_VE_CUAD_f_prob_x
  )
  
  # Gráfica de barras
  
  g <- ggplot(data = tabla, aes(x = x, y = f_prob_x, fill=x)) +
    geom_bar(stat="identity") +
    geom_vline(xintercept = VE, color = 'red', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE - desv.std, color = 'blue', linetype = "dashed", size = 1) +
    geom_vline(xintercept = VE + desv.std, color = 'blue', linetype = "dashed", size = 1) +
    labs(title="Distribución Acumulada de Probabilidad ", 
         subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), 
         x="Variable x", y="Prob. F(x)")
  # Gráfico acumulado
  g_acumulado <- ggplot(data = tabla, aes(x = x, y=F_acumulado)) +
    geom_point(colour="blue") + 
    geom_line(colour="red") +
    labs(title="Distribución Acumulada de Probabilidad ", 
         subtitle = paste("VE", round(VE, 2), "± Desv. Std", round(desv.std, 2)), x="Variable x", y="Prob. F(x)")
  
  
  
  estadisticos <- list(tabla = tabla, x = x, N = n,
                       VE = VE, varianza = varianza, 
                       desv.std = desv.std, 
                       g = g, 
                       g_acumulado = g_acumulado)
  
  estadisticos
  
  
}