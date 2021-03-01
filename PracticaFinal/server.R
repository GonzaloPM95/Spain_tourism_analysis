shinyServer(function(input, output) {

    
    gastosCCAAReactive <- reactive({
        
        gastos_turistasEv <- gastos_turistas %>% 
            filter(CCAA == input$seleccionOrigendatos & Gastos == "Gasto total" & Tipo_Dato == "Dato base")
        
        gastos_turistasEv
    })
    
    gastostotalReactive <- reactive({
        
        gasto <- gastos_turistas %>% 
            filter(CCAA == "Total" & Gastos == "Gasto total" & Tipo_Dato == "Dato base")
        gasto
        
    })
    
    # REACTIVE VALUEBOX GASTO DIC 2020
    
    total_gasto_dic2020Reactive <- reactive({
        
        total_gasto <- gastos_turistas%>% 
            dplyr::filter(CCAA == 'Total' & Gastos == 'Gasto total' &
                              Tipo_Dato == 'Dato base' & Periodo == '2020-12-01'
            )
        total_gasto
    })
    
    # REACTIVE VALUEBOX GASTO DIC 2019
    
    total_gasto_dic2019Reactive <- reactive({
        
        total_gasto <- gastos_turistas%>% 
            dplyr::filter(CCAA == 'Total' & Gastos == 'Gasto total' &
                              Tipo_Dato == 'Dato base' & Periodo == '2019-12-01'
            )
        total_gasto
    })
    
    
    
    ## REACTIVE GASTO DIC 2020 BARRAS
    gasto_Dic2020Reactive <- reactive({
        
        dic2020 <-gastos_turistas %>% 
            filter(CCAA != "Total" & Periodo == '2020-12-01' & 
                       Tipo_Dato == 'Dato base' & Gastos == 'Gasto total')
        
        dic2020
        
    })
    
    #UI GASTOS SELECCIONADOR DE CCAA EN EL GRAFICO DERECHA
    
    
    output$seleccionCCAA <- renderUI({
        if(input$eleccionTotaloNo == 'CCAA'){
            seleccion <- unique(gastos_turistas$CCAA[gastos_turistas$CCAA != "Total"])
            pickerInput('seleccionOrigendatos', label = 'Selecciona la CCAA', choices = seleccion,
                        options = list(
                            style = "btn-primary"))
        }
    })
    
    # VALUE BOX GASTOS ----
    
    output$gastodic2019 <- renderValueBox({
        gasto <- total_gasto_dic2019Reactive()
        valueBox(gasto$Total, subtitle = 'Gasto total diciembre 2019', width = 12, color = 'green', icon = icon('comment-dollar'))
    })
    
    
    output$gastodic2020 <- renderValueBox({
        gasto <- total_gasto_dic2020Reactive()
        valueBox(gasto$Total, subtitle = 'Gasto total diciembre 2020', width = 12, color = 'aqua', icon = icon('comment-dollar'))
    })
    
    
    
    ### PLOTS CCAA ----
    
    ## GRAFICO GASTOS IZQUIERDA GASTO TOTAL TURISTAS
    
    output$gastosTotal <- renderPlotly({
        
        data <- gastostotalReactive()
        
        
        maximo <- data$Periodo[which.max(data$Total)]
        
        
        maximo1 <- max(data$Total)
        
        
        ggplotly(ggplot(data, aes( x = Periodo , y = Total)) +
                     geom_area(color = "grey", size = 0.5, fill= "lightblue")+
                     labs(title =" Evolución del gasto total de los turistas", x = "Evolucion por años", y = "Gasto Total")+
                     theme_minimal() +
                     geom_point(aes(x=maximo,y=maximo1)) +
                     scale_shape_manual(values = 4) +
                     geom_text(aes(x=maximo,y=maximo1, label=maximo1),nudge_x = 2, color="indianred1"))
        
        
        
    })
    
    ## GRAFICO GASTOS IZQUIERDA GASTO POR CCAA TURISTAS
    
    output$gastosPorComunidad <- renderPlotly({
        data <- gastosCCAAReactive()
        
        
        maximo <- data$Periodo[which.max(data$Total)]
        
        
        maximo1 <- max(data$Total)
        
        
        ggplotly(ggplot(data, aes( x = Periodo , y = Total)) + 
                     geom_area(color = "grey", size = 0.5, fill= "lightblue")+
                     labs(title =" Evolución del gasto total de los turistas", x = "Evolucion por años", y = "Gasto Total")+
                     theme_minimal() +
                     geom_point(aes(x=maximo,y=maximo1)) +
                     scale_shape_manual(values = 4) +
                     geom_text(aes(x=maximo,y=maximo1, label=maximo1),nudge_x = 2, color="indianred1"))
        
        
        
    })
    
    ## GRAFICO GASTOS DERECHA GASTO DIC 2020
    
    output$graficoGastoDic2020 <- renderPlotly({
        
        gastos_turistasDic2020 <- gasto_Dic2020Reactive()
        
        grafico <- ggplot(gastos_turistasDic2020, aes(CCAA, Total, fill = CCAA))
        ggplotly(grafico + geom_col() + aes(fill = CCAA) + labs(title = "Gasto Turistico por CCAA", y = "Gasto Total") + 
                     theme(panel.background = element_rect(fill = "white",
                                                           colour = "white",
                                                           size = 0.5, linetype = "solid"),
                           panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                           colour = "lightgrey"), 
                           panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                                           colour = "lightgrey"),
                           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
        
    })
    
    
    
    ########################## ESTABLECIMIENTOS ##############################
    
    
    ### REACTIVOS ESTABLECIMIENTOS ----
    
    
    ## REACTIVE PARA EL PLOT
    
    mezclaReactive <- reactive({
        
        es_a <- establecimientos_abiertos %>%
            dplyr::filter(
                CCAA =='Total Nacional' 
                & Establecimientos == 'Número de plazas estimadas')
        
        pernoc <- pernoctaciones %>% dplyr::filter(Operacion == 'Encuesta de Ocupación Hotelera' & CCAA == "Total Nacional" & Residencia == 'Total ' & Viajeros == 'Pernoctaciones') 
        
        colnames(pernoc)[6] <- 'Total_pernoctaciones'
        
        ocupacion <- es_a %>% dplyr::filter(Operacion == 'Encuesta de Ocupación Hotelera')
        
        colnames(ocupacion)[5] <- 'Total_hoteles'
        
        
        mezcla <- cbind(pernoc,ocupacion[,c(3,5)])
        
        mezcla
        
    })
    
    
    
    
    ## REACTIVE DE VALUEBOXES DE ESTABLECIMIENTOS
    
    valueBoxesEstablecimientos <- reactive({
        totales_hoteles <- establecimientos_abiertos %>% 
            dplyr::filter(Operacion =='Encuesta de Ocupación Hotelera'& 
                              CCAA == input$CCAAEstablecimientosID &        
                              Establecimientos == "Número de establecimientos abiertos estimados" &
                              year(Periodo) == input$AnoEstablecimientosID) %>%
            dplyr::summarise(Total = sum(Total))
        
        
        totales_camping <- establecimientos_abiertos %>% 
            dplyr::filter(Operacion =='Encuesta de Ocupación en Campings' & 
                              CCAA == input$CCAAEstablecimientosID &      
                              Establecimientos == "Número de establecimientos abiertos estimados"  &
                              year(Periodo)== input$AnoEstablecimientosID) %>%
            dplyr::summarise(Total = sum(Total, na.rm = T))
        
        
        totales_ApTuristicos <- establecimientos_abiertos %>% 
            dplyr::filter(Operacion =='Encuesta de Ocupación en Apartamentos Turísticos' & 
                              CCAA == input$CCAAEstablecimientosID &      
                              Establecimientos == "Número de plazas estimadas"  &
                              year(Periodo)== input$AnoEstablecimientosID) %>%
            dplyr::summarise(Total = sum(Total, na.rm = T))
        
        totales_AlojRur <- establecimientos_abiertos %>% 
            dplyr::filter(Operacion =='Encuesta de Ocupación en Alojamientos de Turismo Rural' & 
                              CCAA == input$CCAAEstablecimientosID &     
                              Establecimientos == "Número de establecimientos abiertos estimados"  &
                              year(Periodo)== input$AnoEstablecimientosID) %>%
            dplyr::summarise(Total = sum(Total, na.rm = T))
        
        list(hoteles = totales_hoteles,
             camping= totales_camping,
             apturisticos = totales_ApTuristicos,
             alojrurales = totales_AlojRur)
        
    })
    
    
    
    ## VALUE BOXES ESTABLECIMIENTOS
    
    output$TotalHoteles <- renderValueBox({
        valueBox(valueBoxesEstablecimientos()$hoteles, 'Hoteles abiertos', 
                 icon = icon('hotel'), width = 12)
    })
    
    output$TotalCamping <- renderValueBox({
        camping = valueBoxesEstablecimientos()$camping
        valueBox(camping, 'Campings abiertos',
                 icon = icon('campground'), width = 12)
    })
    
    output$TotalApartTuristico <- renderValueBox({
        apturisticos = valueBoxesEstablecimientos()$apturisticos
        valueBox(apturisticos, 'Apartamentos turisticos abiertos', 
                 icon = icon('building'), width = 12)
    })
    
    output$TotalAlojRural <- renderValueBox({
        alojrurales = valueBoxesEstablecimientos()$alojrurales
        valueBox(alojrurales, 'Alojamientos de turismo rural abiertos', 
                 icon = icon('home'), width = 12)
    })
    
    ## PLOT ESTABLECIMIENTOS 
    
    
    output$grafico_evol_establecimientos_abiertos <- renderPlotly({
        
        
        
        mezcla <- mezclaReactive()
        
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
        
    })
    
    
    output$graficoPernocSector <- renderPlotly({
        
        es_a1 <- establecimientos_abiertos %>%
            dplyr::filter(
                CCAA == input$seleccionCCAAPernocSector
                & Establecimientos == 'Número de plazas estimadas')
        
        
        ggplotly(ggplot(es_a1,aes(Periodo,Total,color=Operacion))+geom_line(size=0.9) +  
                     labs(title = "Evolucion de pernoctaciones por sector", x = "Fecha", y = "Gasto Total")+
                     theme_minimal())
        
    })
    
    output$graficoPieChart <- renderPlotly({
        
        pernoctacionesDic2020 <- pernoctaciones %>% filter((year(Periodo) == input$seleccionPeriodoPieChart2) & Viajeros == 'Pernoctaciones' 
                                                           & CCAA == "Total Nacional" & Residencia != "Total ")
        
        pernoctacionesDic2019 <- pernoctaciones %>% filter((year(Periodo) == input$seleccionPeriodoPieChart) & Viajeros == 'Pernoctaciones' & Residencia != "Total " &
                                                               CCAA == "Total Nacional")
        
        fig <- plot_ly()
        fig <- fig %>% add_pie(data = pernoctacionesDic2019, labels = ~Residencia, values = ~Total,
                               name = input$seleccionPeriodoPieChart, domain = list(row = 0, column = 0))
        fig <- fig %>% add_pie(data = pernoctacionesDic2020, labels = ~Residencia, values = ~Total,
                               name = input$seleccionPeriodoPieChart2, domain = list(row = 0, column = 1))
        
        fig <- fig %>% layout(showlegend = T,
                              grid=list(rows=1, columns=2),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        fig
        
    })
    
    
    
})

