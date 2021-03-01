#install.packages("shinycssloaders")

library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(readr)
library(cowplot)
library(data.table)
library(tidyverse)
library(dplyr)
library(fastmap)
library(plotly)
library(DT)
library(fastmap)


gastos_turistas <- read.csv('Data/Gastos turistas.csv', sep=';',encoding = 'UTF-8')

establecimientos_abiertos <- read.csv('Data/Establecimientos abiertos.csv',sep=';',encoding = 'UTF-8')

pernoctaciones <- read.csv('Data/Pernoctaciones.csv',sep=';',encoding = 'UTF-8')


colnames(gastos_turistas)[1] <- 'CCAA'
colnames(gastos_turistas)[2] <- 'Gastos'
colnames(gastos_turistas)[3] <- 'Tipo_Dato'

colnames(establecimientos_abiertos)[1] <- 'Operacion'
colnames(establecimientos_abiertos)[2] <- 'CCAA'
colnames(establecimientos_abiertos)[3] <- 'Establecimientos'

colnames(pernoctaciones)[1] <- 'Operacion'
colnames(pernoctaciones)[2] <- 'CCAA'
colnames(pernoctaciones)[4] <- 'Viajeros'


pernoctaciones$Total <- gsub(".","",pernoctaciones$Total,fixed=TRUE)
pernoctaciones$Total <- gsub("," , ".",pernoctaciones$Total,fixed=TRUE)
pernoctaciones$Total<- as.numeric(pernoctaciones$Total)


gastos_turistas$Total <- gsub("." , "",gastos_turistas$Total,fixed=TRUE)
gastos_turistas$Total <- gsub("," , ".",gastos_turistas$Total,fixed=TRUE)
gastos_turistas$Total <- as.numeric(gastos_turistas$Total)


establecimientos_abiertos$Total <- gsub(".", "",establecimientos_abiertos$Total,fixed=TRUE)
establecimientos_abiertos$Total <- gsub("," , ".",establecimientos_abiertos$Total,fixed=TRUE)
establecimientos_abiertos$Total <- as.numeric(establecimientos_abiertos$Total)

pernoctaciones$Periodo <- parse_datetime(pernoctaciones$Periodo, "%Y M%m")
pernoctaciones$Periodo <- as.Date(pernoctaciones$Periodo)

gastos_turistas$Periodo <- parse_datetime(gastos_turistas$Periodo, "%Y M%m")
gastos_turistas$Periodo <- as.Date(gastos_turistas$Periodo)

establecimientos_abiertos$Periodo <- parse_datetime(establecimientos_abiertos$Periodo, "%Y M%m")
establecimientos_abiertos$Periodo <- as.Date(establecimientos_abiertos$Periodo)


# pernoctaciones

pt <- pernoctaciones %>% dplyr::filter(CCAA == 'Total Nacional' 
                                       & Residencia == 'Total ' 
                                       & Viajeros == 'Pernoctaciones')


# Hoteles abiertos y cantidad de pernoctas

pernoc <- pt %>% dplyr::filter(Operacion == 'Encuesta de Ocupación Hotelera') 

