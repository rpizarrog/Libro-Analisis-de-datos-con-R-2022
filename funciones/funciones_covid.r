f.aidr.covid <- function() {
  # Librerías
  library(readr)
  library(dplyr)
  
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
  
  muestra202122 <- cbind(muestra202122, substring(muestra202122$FECHA_INGRESO, 1,4))
  
  colnames(muestra202122) <- tolower(colnames(muestra202122))
  
  return(muestra202122)

}