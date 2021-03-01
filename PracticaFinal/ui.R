shinyUI(dashboardPage(skin='blue',
    dashboardHeader(title = 'Estudio Turismo'),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduccion', tabName = 'introduccion', icon = icon('desktop')),
            menuItem('Gastos', tabName = 'gastos', icon = icon('euro-sign')),
            menuItem('Pernoctaciones', tabName = 'pernoctaciones', icon = icon('bed')),
            menuItem('Establecimientos Abiertos', tabName = 'establecimientos', icon = icon('h-square')),
            menuItem('Sobre Nosotros', tabName = 'cracks', icon = icon('skull-crossbones'))
        )
    ),
    
    dashboardBody(
        tabItems(
            # Tab de introduccion ----
            tabItem(tabName = 'introduccion',
                    includeCSS('www/html/layout/styles/layout.css'),
                    includeCSS('www/html/layout/styles/framework.css'),


                    includeHTML('www/html/index.html')
            ),
            
            tabItem(tabName = 'cracks',
                    includeHTML('www/html/aboutUs.html')
            ),
            
            ###### TAB GASTOS ######## 
            
            tabItem(
                tabName = 'gastos',
                
                fluidRow(
                    
                    
                    column(6, 
                            valueBoxOutput('gastodic2019', width = 12), size = 0.3
                           
                           
                    ),
                    column(6,
                            valueBoxOutput('gastodic2020', width = 12), size = 0.3
                           
                    )
                    
                ),
                
                fluidRow(
                    column(6, box( width = 12,
                                   fluidRow( column(6, align = 'center',
                                                    radioGroupButtons('eleccionTotaloNo', 
                                                                      label = 'Ambito', 
                                                                      choices = list('Total', 'CCAA'), 
                                                                      status = 'primary',
                                                                      checkIcon = list(
                                                                          yes = icon("ok", 
                                                                                     lib = "glyphicon")))
                                   ),
                                   column(6, 
                                          align = 'center',
                                          uiOutput('seleccionCCAA')
                                   )
                                   
                                   ),
                                   fluidRow(
                                       column(12,
                                              conditionalPanel("input.eleccionTotaloNo == 'Total'", 
                                                               shinycssloaders::withSpinner(
                                                                    plotlyOutput('gastosTotal')
                                                                  )),
                                              
                                              conditionalPanel("input.eleccionTotaloNo == 'CCAA'", 
                                                               shinycssloaders::withSpinner(
                                                                    plotlyOutput('gastosPorComunidad')
                                                               )
                                              )
                                       )
                                       
                                   )
                    )    
                    
                    ),
                    
                    ## GRAFICO GASTOS DERECHA DIC 2020
                    
                    column(6,
                           box(title = "Gasto total diciembre 2020 por CCAA", solidHeader = T, 
                               width = 12,status = 'info',
                               collapsible = F,
                               shinycssloaders::withSpinner(
                                    plotlyOutput('graficoGastoDic2020')
                               )
                           )
                    )
                )
            ),
            
            tabItem(
                tabName = 'pernoctaciones',
                
                fluidRow(
                    
                    column(2,
                           pickerInput('seleccionCCAAPernocSector', label = 'Selecciona la Comunidad Autonoma', 
                                       choices = unique(pernoctaciones$CCAA), 
                                       options = list(
                                           style = "btn-primary"))
                    ),
                    
                    column(10, 
                           box(title = "Evolucion de las pernoctaciones por sector turistico", solidHeader = T, 
                               width = 12,status = 'info',
                               collapsible = F,
                               
                               shinycssloaders::withSpinner(
                                    plotlyOutput('graficoPernocSector')
                               )
                           )
                           
                           
                           
                    )
                    
                ),
                
                fluidRow(
                    column(2,
                           pickerInput('seleccionPeriodoPieChart', label = 'Selecciona el año 1', selected = "2019",
                                       choices = unique(year(pernoctaciones$Periodo)),
                                       options = list(
                                           style = "btn-primary"))
                    ),
                    
                    column(8,
                           box(title = "Distribucion de la nacionalidad de los turistas a lo largo de los años", solidHeader = T, 
                               width = 12, status = 'info',
                               collapsible = F,
                               shinycssloaders::withSpinner(
                                    plotlyOutput('graficoPieChart')
                               )
                           )
                           
                    ),
                    
                    column(2,
                           pickerInput('seleccionPeriodoPieChart2', label = 'Selecciona el año 2', 
                                       choices = unique(year(pernoctaciones$Periodo)), options = list(
                                           style = "btn-primary"))
                    ),
                    
                )
                
                
            ),
            
            ### PESTANIA ESTABLECIMIENTOS
            
            tabItem(
                tabName = 'establecimientos',
                
                ## PRIMERA FILA, BOXES Y FILTRO 
                
                fluidRow(
                    column(3, align = 'left',
                           fluidRow(
                               column(12,
                                      shinycssloaders::withSpinner(
                                            valueBoxOutput('TotalHoteles', width = 12), size = 0.3
                                      )
                               )
                           ),
                           fluidRow(
                               column(12,
                                      #shinycssloaders::withSpinner(
                                            valueBoxOutput('TotalCamping', width = 12)
                                      #)
                               )
                           )
                    ),
                    column(6, align = 'center',
                           box(width = 12, title = 'Escoge año y CCAA', status = 'primary',
                               align = 'center',
                               solidHeader = TRUE, collapsible = TRUE,
                               collapsed = FALSE,
                               h4(strong('Escoja una Comunidad y un Año')),
                               
                               pickerInput(inputId = 'CCAAEstablecimientosID', label = 'Comunidad Autonoma', choices = unique(establecimientos_abiertos$CCAA),
                                           options = list(
                                               style = "btn-primary")),
                               
                               pickerInput(inputId = 'AnoEstablecimientosID', label = 'Periodo', choices = unique(year(establecimientos_abiertos$Periodo)),
                                           options = list(
                                               style = "btn-primary"))
                               
                           )),
                    column(3, align = 'left',
                           fluidRow(
                               column(12,
                                      shinycssloaders::withSpinner(
                                            valueBoxOutput('TotalApartTuristico', width = 12), size = 0.3
                                      )
                               )
                           ),
                           fluidRow(
                               column(12,
                                      #shinycssloaders::withSpinner(
                                            valueBoxOutput('TotalAlojRural', width = 12)
                                      #)
                               )
                           )
                    )
                    
                ),
                

                fluidRow(
                    column(12, align = 'center',
                           box(
                               solidHeader = F, 
                               width = 12,
                               status = 'danger',
                               collapsible = F,
                               shinycssloaders::withSpinner(
                                    plotlyOutput('grafico_evol_establecimientos_abiertos')
                               )
                               
                           )
                           
                    )
                )
            )
        )
        
    )
    
)
)

