# Carga de librerias
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(sqldf)
library(reshape2)


source('scripts/data_treatment.R')

#Parametros Loaders
options(spinner.type=6)

print(getwd())