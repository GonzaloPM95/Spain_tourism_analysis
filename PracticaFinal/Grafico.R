library(ggplot2)
library(dplyr)
library(readr)
library(cowplot)
library(data.table)
library(tidyverse)
library(dplyr)
library(fastmap)
library(plotly)

setwd("C:/Users/Ernesto Pérez/OneDrive/Master/DV/Parte B/VisualizacionR/PracticaFinal/PracticaFinal")
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


# pernoctaciones evoluciones y establecimientos abiertos con lineas linechart

es_a <- establecimientos_abiertos %>%
  dplyr::filter(
    CCAA =='Total Nacional' 
    & Establecimientos == 'Número de plazas estimadas')


ggplotly(ggplot(es_a,aes(Periodo,Total,color=Operacion))+geom_line(size=0.9) +  
  labs(title = "Evolucion de pernoctaciones por sector", x = "A?o", y = "Gasto Total")+
  theme_minimal())

# ---------- Gasto turistico por CCAA -----------

gastos_turistasDic2020 <- gastos_turistas %>% filter(CCAA != "Total" & Periodo == '2020-12-01' & 
                                                       Tipo_Dato == 'Dato base' & Gastos == 'Gasto total')

grafico <- ggplot(gastos_turistasDic2020, aes(CCAA, Total, fill = CCAA))
grafico + geom_col() + aes(fill = CCAA) + labs(title = "Gasto Turistico por CCAA", y = "Gasto Total") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
#---------------- --------------Distribucion de la nacionalidad de los turistas en 2019 y 2020 ----------------


pernoctacionesDic2020 <- pernoctaciones %>% filter((Periodo == "2020-12-01") & Viajeros == 'Pernoctaciones' 
                                                   & CCAA == "Total Nacional" & Operacion == "Encuesta de Ocupación Hotelera" & Residencia != "Total ")

pernoctacionesDic2019 <- pernoctaciones %>% filter((Periodo == "2019-12-01") & Viajeros == 'Pernoctaciones' & Residencia != "Total ")

      
fig <- plot_ly()
fig <- fig %>% add_pie(data = pernoctacionesDic2019, labels = ~Residencia, values = ~Total,
                       name = "2019", domain = list(row = 0, column = 0))
fig <- fig %>% add_pie(data = pernoctacionesDic2020, labels = ~Residencia, values = ~Total,
                       name = "2020", domain = list(row = 0, column = 1))

fig <- fig %>% layout(title = "Distribucion de la nacionalidad de los turistas en 2019 y 2020", showlegend = T,
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


#------------- Evolucion Pernoctaciones y Establecimientos abiertos -----------

pernoc <- pernoctaciones %>% dplyr::filter(Operacion == 'Encuesta de Ocupación Hotelera' & CCAA == "Total Nacional" & Residencia == 'Total ' & Viajeros == 'Pernoctaciones') 

colnames(pernoc)[6] <- 'Total_pernoctaciones'

ocupacion <- es_a %>% dplyr::filter(Operacion == 'Encuesta de Ocupación Hotelera')

colnames(ocupacion)[5] <- 'Total_hoteles'


mezcla <- cbind(pernoc,ocupacion[,c(3,5)])

print(head(mezcla))
coeff <- 24.5

graficoPlot <- ggplot(mezcla, aes(x = Periodo))

ggplotly(graficoPlot + geom_line(aes(y = Total_hoteles), color = "Red2", size = 0.9) +
  geom_line(aes(y = Total_pernoctaciones/coeff), color="Blue3", size = 0.9) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Establecimientos abiertos",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Pernoctaciones")
  )  +
  
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                    colour = "lightgrey"), 
    panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                    colour = "lightgrey"),
    axis.title.y = element_text(color = "Red3", size=13),
    axis.title.y.right = element_text(color = "Blue3", size=13)
  ) + labs(title = "Evolucion Pernoctaciones y Establecimientos Abiertos"))



