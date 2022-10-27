library(dplyr)
# Preparar los datos de diabetes.csv

# Función que modifica valores de cero a su media estadísticas 
# de los valores mayores a cero
# de las variables de diabetes
f.aidr.diabetes.mean <- function(datos) {


    # glucose
    datos <- datos  %>%
      mutate(glucose = ifelse(glucose == 0, 
            as.integer(mean(datos$glucose[datos$glucose > 0])), glucose))
    

    #bloodpressure
    datos <- datos  %>%
      mutate(bloodpressure = ifelse(bloodpressure == 0, 
            as.integer(mean(datos$bloodpressure[datos$bloodpressure > 0])), bloodpressure))
  
    # skinthickness
    datos <- datos  %>%
      mutate(skinthickness = ifelse(skinthickness == 0, 
            as.integer(mean(datos$skinthickness[datos$skinthickness > 0])), skinthickness))
  
    # insulin
    datos <- datos  %>%
      mutate(insulin = ifelse(insulin == 0, 
            as.integer(mean(datos$insulin[datos$insulin > 0])), insulin))

    # bmi
    datos <- datos  %>%
      mutate(bmi = ifelse(bmi == 0, 
            (mean(datos$bmi[datos$bmi > 0])), bmi))
  return (datos)
  
}


# Función que modifica a un valor aleatorio 
# de los valores existentes mayores a cero
# de las variables de diabetes
f.aidr.diabetes.sample <- function(datos) {
  
  set.seed(2022)
  # glucose
  n0 <- length(datos$glucose[datos$glucose == 0])
  datos <- datos  %>%
    mutate(glucose = ifelse(glucose == 0, 
                            sample(glucose[which(glucose > 0)], n0), glucose))
  
  
  #bloodpressure
  n0 <- length(datos$skinthickness[datos$bloodpressure == 0])
  datos <- datos  %>%
    mutate(bloodpressure = ifelse(bloodpressure == 0, 
                                  sample(bloodpressure[which(bloodpressure > 0)], n0), bloodpressure))
  
  # skinthickness
  n0 <- length(datos$skinthickness[datos$skinthickness == 0])
  datos <- datos  %>%
    mutate(skinthickness = ifelse(skinthickness == 0, 
                                  sample(skinthickness[which(skinthickness > 0)], n0), skinthickness))
  
  # insulin
  n0 <- length(datos$insulin[datos$insulin == 0])
  datos <- datos  %>%
    mutate(insulin = ifelse(insulin == 0, 
                            sample(insulin[which(insulin > 0)], n0), insulin))
  
  # bmi
  n0 <- length(datos$bmi[datos$bmi == 0])
  datos <- datos  %>%
    mutate(bmi = ifelse(bmi == 0, 
                        sample(bmi[which(bmi > 0)], n0), bmi))
  return (datos)
  
}
