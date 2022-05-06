server <- function(input, output) {
  
  # --------- VALUE BOX ACEH
  output$aceh2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("ACEH", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "ACEH" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX SUMATERA UTARA
  output$sumut2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SUMATERA UTARA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SUMATERA UTARA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX SUMATERA BARAT
  output$sumbar2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SUMATERA BARAT", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SUMATERA BARAT" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX RIAU
  output$riau2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("RIAU", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "RIAU" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })
  
  # --------- VALUE BOX JAMBI
  output$jambi2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("JAMBI", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "JAMBI" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })
  
  # --------- VALUE BOX SUMATERA SELATAN
  output$sumsel2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SUMATERA SELATAN", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SUMATERA SELATAN" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX BENGKULU
  output$bengkulu2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("BENGKULU", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "BENGKULU" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX LAMPUNG
  output$lampung2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("LAMPUNG", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "LAMPUNG" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })

  # --------- VALUE BOX BANGKA BELITUNG
  output$bangkabelitung2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KEP. BANGKA BELITUNG", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KEP. BANGKA BELITUNG" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX KEP. RIAU
  output$kepriau2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KEP. RIAU", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KEP. RIAU" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX BANTEN
  output$banten2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("BANTEN", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "BANTEN" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX DKI JAKARTA
  output$jakarta2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("DKI JAKARTA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "DKI JAKARTA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })
  
  # --------- VALUE BOX JAWA BARAT
  output$jabar2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("JAWA BARAT", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "JAWA BARAT" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })

  # --------- VALUE BOX JAWA TENGAH
  output$jateng2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("JAWA TENGAH", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "JAWA TENGAH" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX DI YOGYAKARTA
  output$yogya2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("DI YOGYAKARTA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "DI YOGYAKARTA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX JAWA TIMUR
  output$jatim2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("JAWA TIMUR", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "JAWA TIMUR" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX BALI
  output$bali2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("BALI", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "BALI" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX NUSA TENGGARA BARAT
  output$ntb2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("NUSA TENGGARA BARAT", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "NUSA TENGGARA BARAT" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX NUSA TENGGARA TIMUR
  output$ntt2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("NUSA TENGGARA TIMUR", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "NUSA TENGGARA TIMUR" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX KALIMANTAN BARAT
  output$kalbar2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KALIMANTAN BARAT", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KALIMANTAN BARAT" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })

  # --------- VALUE BOX KALIMANTAN TENGAH
  output$kalteng2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KALIMANTAN TENGAH", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KALIMANTAN TENGAH" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })
  
  # --------- VALUE BOX KALIMANTAN SELATAN
  output$kalsel2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KALIMANTAN SELATAN", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KALIMANTAN SELATAN" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX KALIMANTAN TIMUR
  output$kaltim2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KALIMANTAN TIMUR", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KALIMANTAN TIMUR" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX KALIMANTAN UTARA
  output$kalut2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("KALIMANTAN UTARA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "KALIMANTAN UTARA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX SULAWESI UTARA
  output$sulut2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SULAWESI UTARA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SULAWESI UTARA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX SULAWESI TENGAH
  output$sulteng2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SULAWESI TENGAH", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SULAWESI TENGAH" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX SULAWESI SELATAN
  output$sulsel2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SULAWESI SELATAN", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SULAWESI SELATAN" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX SULAWESI TENGGARA
  output$sulteg2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SULAWESI TENGGARA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SULAWESI TENGGARA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })
  
  # --------- VALUE BOX GORONTALO
  output$gorontalo2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("GORONTALO", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "GORONTALO" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "fuchsia"
    )
  })
  
  # --------- VALUE BOX SULAWESI BARAT
  output$sulbar2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("SULAWESI BARAT", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "SULAWESI BARAT" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "green"
    )
  })
  
  # --------- VALUE BOX MALUKU
  output$maluku2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("MALUKU", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "MALUKU" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # --------- VALUE BOX MALUKU UTARA
  output$malukuutara2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("MALUKU UTARA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "MALUKU UTARA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX PAPUA BARAT
  output$papuabarat2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("PAPUA BARAT", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "PAPUA BARAT" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "red"
    )
  })
  
  # --------- VALUE BOX PAPUA
  output$papua2022 <- renderValueBox({
    valueBox(
      icon = tags$i(class = "fas fa-city", style = "font-size: 60%"),
      subtitle = tags$p("PAPUA", style = "font-size: 90%;"),
      value = tags$p(paste0("Rp", umr[umr$Region == "PAPUA" & umr$Year == "2022",'Salary'], ",-"), style = "font-size: 50%;"),
      color = "blue"
    )
  })
  
  # -------- MAKS, MIN, MEAN INPUT_YEAR
  output$maxyear <- renderValueBox({
    valueBox(
      subtitle = tags$p((umr[umr$Year == input$input_year,])[(umr[umr$Year == input$input_year,])$Salary == max((umr[umr$Year == input$input_year,])$Salary),'Region'], style = "font-size: 90%;"),
      value = tags$p(paste0("NILAI UMR TERTINGGI: Rp", (umr[umr$Year == input$input_year,])[(umr[umr$Year == input$input_year,])$Salary == max((umr[umr$Year == input$input_year,])$Salary),'Salary'], ",-"), style = "font-size: 40%;"),
      color = "green"
    )
  })
  output$minyear <- renderValueBox({
    valueBox(
      subtitle = tags$p((umr[umr$Year == input$input_year,])[(umr[umr$Year == input$input_year,])$Salary == min((umr[umr$Year == input$input_year,])$Salary),'Region'], style = "font-size: 90%;"),
      value = tags$p(paste0("NILAI UMR TERENDAH: Rp", (umr[umr$Year == input$input_year,])[(umr[umr$Year == input$input_year,])$Salary == min((umr[umr$Year == input$input_year,])$Salary),'Salary'], ",-"), style = "font-size: 40%;"),
      color = "maroon"
    )
  })
  output$meanyear <- renderValueBox({
    infoBox(
      icon = icon("calculator"),
      title = glue("RATA-RATA UMR TAHUN {input$input_year}"),
      value = tags$p(paste0("Rp",round(as.numeric(mean(umr[umr$Year == input$input_year,"Salary"]))),",-"), style = "font-size: 200%;"),
      color = "yellow"
    )
  })
  
  # -------- LEAFLET
    output$mymap2022 <- renderLeaflet({
    df <- wage_data %>% 
      filter(Year == input$input_year) %>% 
      left_join(
        location_data %>% 
          mutate(
            Region = toupper(Region)
          ),
        by = "Region"
      ) %>%
      group_by(Region) %>%
      mutate(rank = row_number()) %>%
      ungroup() %>%
      filter(rank == 1) %>% 
      subset(Region != "INDONESIA") %>% 
      mutate(Salary = as.numeric(Salary))
    
    a <- min(df$Salary) * 0.9
    b <- max(df$Salary) * 1.2
    mybins <- seq(a, b, by = (b - a)/8)
    
    mypalette <- colorBin( 
      palette = "YlOrBr",
      domain = df$Salary,
      na.color = "transparent",
      bins = mybins
    )
    
    leaflet(df) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addCircleMarkers( 
        ~ lng,
        ~ lat,
        fillColor = ~ mypalette(Salary),
        fillOpacity = 0.7,
        color = "white",
        radius = 8,
        stroke = FALSE,
        label = ~ Region,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addMarkers(popup = paste0(df$Region, ': ',  
                                paste(
                                  'Rp', formatC(df$Salary, big.mark = "", format = 'fg'), ',-'
                                )
      )
      ) %>%
      addLegend(
        "bottomleft",
        pal = mypalette,  
        values =  ~ mybins, 
        opacity = 0.6,
        title = "Range of Regional Minimum Wage (Rp)"
      )
  })
    
  # -------- LINE PLOT
    output$plot_province <- renderPlotly({
      
    umr_line <- umr %>% 
      select(c("Region","Year","Salary")) %>% 
      filter(Region != "INDONESIA") %>% 
      mutate(
        Region = as.factor(Region),
        Year = as.numeric(Year)
      ) %>% 
      filter(Region == input$input_province) %>% 
      mutate(label = glue(
        "Year: {Year}
        Salary: Rp{Salary},-"
      ))
    
    plot_line <- ggplot(umr_line, aes(x = Year, y = Salary))+
      scale_x_continuous(limits = c(1997, 2022),
                         breaks=(seq(1997, 2022, 1))) +
      scale_y_continuous(labels = scales::comma) +
      geom_line(col="red") +
      geom_point(aes(text=label), col="black") +
      labs(
        title = glue("REGIONAL MINIMUM WAGE OF {input$input_province}"),
        x = "Year",
        y = "Salary (Rp)"
      ) +
      theme_update() +
      theme(
        axis.text.x = element_text(angle = 60, vjust = 1,
                                   hjust = 1),
        plot.title = element_text(family = "sans",
                                  face = "bold", size = 13, 
                                  margin = margin(15,0,9,0)),
        axis.text = element_text(family = "Times", size =
                                   8),
        axis.title.x = element_text(family = "Times", 
                                    size = 11, face = "bold", margin = 
                                      margin(6,0,9,0)),
        axis.title.y = element_text(family = "Times", 
                                    size = 11, face = "bold", margin = 
                                      margin(0,9,0,7)),
        plot.margin = margin(0,0.8,0,0, "cm")
      ) 
    
    ggplotly(plot_line, tooltip = "text")
    
    })
    
    # -------- DATASET
    output$dataset_table <- DT::renderDataTable(umr,
          options = list(scrollX = T, scrollY = T))
    
    # -------- MAKS, MIN, MEAN 2022
    output$max2022 <- renderValueBox({
      valueBox(
        subtitle = tags$p((umr[umr$Year == 2022,])[(umr[umr$Year == 2022,])$Salary == max((umr[umr$Year == 2022,])$Salary),'Region'], style = "font-size: 90%;"),
        value = tags$p(paste0("NILAI UMR TERTINGGI: Rp", (umr[umr$Year == 2022,])[(umr[umr$Year == 2022,])$Salary == max((umr[umr$Year == 2022,])$Salary),'Salary'], ",-"), style = "font-size: 40%;"),
        color = "green"
      )
    })
    output$min2022 <- renderValueBox({
      valueBox(
        subtitle = tags$p((umr[umr$Year == 2022,])[(umr[umr$Year == 2022,])$Salary == min((umr[umr$Year == 2022,])$Salary),'Region'], style = "font-size: 90%;"),
        value = tags$p(paste0("NILAI UMR TERENDAH: Rp", (umr[umr$Year == 2022,])[(umr[umr$Year == 2022,])$Salary == min((umr[umr$Year == 2022,])$Salary),'Salary'], ",-"), style = "font-size: 40%;"),
        color = "maroon"
      )
    })
    output$mean2022 <- renderValueBox({
      infoBox(
        icon = icon("calculator"),
        title = glue("RATA-RATA UMR"),
        value = tags$p(paste0("Rp",round(as.numeric(mean(umr[umr$Year == 2022,"Salary"]))),",-"), style = "font-size: 200%;"),
        color = "yellow"
      )
    })
    
    # -------- LEAFLET
    output$mymap <- renderLeaflet({
      df <- wage_data %>% 
        filter(Year == 2022) %>% 
        left_join(
          location_data %>% 
            mutate(
              Region = toupper(Region)
            ),
          by = "Region"
        ) %>%
        group_by(Region) %>%
        mutate(rank = row_number()) %>%
        ungroup() %>%
        filter(rank == 1) %>% 
        subset(Region != "INDONESIA") %>% 
        mutate(Salary = as.numeric(Salary))
      
      a <- min(df$Salary) * 0.9
      b <- max(df$Salary) * 1.2
      mybins <- seq(a, b, by = (b - a)/8)
      
      mypalette <- colorBin( 
        palette = "YlOrBr",
        domain = df$Salary,
        na.color = "transparent",
        bins = mybins
      )
      
      leaflet(df) %>%
        addTiles() %>%
        addProviderTiles("CartoDB.Positron") %>% 
        addCircleMarkers( 
          ~ lng,
          ~ lat,
          fillColor = ~ mypalette(Salary),
          fillOpacity = 0.7,
          color = "white",
          radius = 8,
          stroke = FALSE,
          label = ~ Region,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) %>%
        addMarkers(popup = paste0(df$Region, ': ',  
                                  paste(
                                    'Rp', formatC(df$Salary, big.mark = "", format = 'fg'), ',-'
                                  )
        )
        ) %>%
        addLegend(
          "bottomleft",
          pal = mypalette,  
          values =  ~ mybins, 
          opacity = 0.6,
          title = "Range of Regional Minimum Wage (Rp)"
        )
    })
    
}