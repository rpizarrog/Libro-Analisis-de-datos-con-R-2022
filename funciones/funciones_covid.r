# Librerías
library(readr)
library(dplyr)

f.aidr.covid <- function() {

  datos.covid.2020.mes <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/datos.covid.2020.mes.csv")
  datos.covid.2020.mes$agio <- '2020'
  names(datos.covid.2020.mes) <- c("no", "mes", "casos", "agnio")
  
  datos.covid.2021.mes <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/datos.covid.2021.mes.csv")
  datos.covid.2021.mes$agio <- '2021'
  names(datos.covid.2021.mes) <- c("no", "mes", "casos", "agnio")
  
  datos.covid.2022.mes <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/datos.covid.2022.mes.csv")
  datos.covid.2022.mes$agio <- '2022'
  names(datos.covid.2022.mes) <- c("no", "mes", "casos", "agnio")
  
  datos.covid <- select(datos.covid.2020.mes, mes,casos, agnio) %>%
    union (select(datos.covid.2021.mes, mes,casos, agnio)) %>%
    union (select(datos.covid.2022.mes, mes,casos, agnio))
  
  datos.covid
}

f.aidr.muestra.covid202122 <- function() {
  datos2020 <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/muestra.COVID.2020.csv", encoding= "UTF-8", stringsAsFactors = TRUE)
  datos2021 <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/muestra.COVID.2021.csv", encoding= "UTF-8", stringsAsFactors = TRUE)
  datos2022 <- read.csv("https://raw.githubusercontent.com/rpizarrog/Libro-Analisis-de-datos-con-R-2022/main/datos/muestra.COVID.2022.csv", encoding= "UTF-8", stringsAsFactors = TRUE)
  
  muestra202122 <- union(union(datos2020, datos2021), datos2022)
  
  muestra202122 <- cbind(muestra202122, agnio = substring(muestra202122$FECHA_INGRESO, 1,4))
  
  colnames(muestra202122) <- tolower(colnames(muestra202122))
  
  
  # Convertir algunas variables a factor, casi todas
  variables_factor <- c("x", "fecha_actualizacion", "id_registro","origen", "sector", "entidad_um", "sexo",
                        "entidad_nac", "entidad_res", "municipio_res",
                        "tipo_paciente", "fecha_ingreso", "fecha_sintomas",
                        "fecha_def", "intubado", "neumonia", "nacionalidad",
                        "embarazo", "habla_lengua_indig", "indigena",
                        "diabetes", "epoc", "asma", "inmusupr", 
                        "hipertension", "otra_com", "cardiovascular",
                        "obesidad", "renal_cronica", "tabaquismo",
                        "otro_caso", "toma_muestra_lab", "resultado_lab",
                        "toma_muestra_antigeno", "resultado_antigeno",
                        "clasificacion_final", "migrante", 
                        "pais_nacionalidad", "pais_origen", "uci", "agnio")
  
  muestra202122[, variables_factor] <- lapply(muestra202122[, variables_factor], as.factor)
  
  # setwd("~/Libro Analisis Inteligente de Datos")
  # write.csv(muestra202122, file = "datos/muestra.COVID.202122.csv")
  
  return(muestra202122)

}