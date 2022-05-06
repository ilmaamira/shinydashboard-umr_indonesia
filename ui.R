dashboardPage(
  
  skin = "purple",
  
  # --------- HEADER
  dashboardHeader(
    title = "UMR di Indonesia Th. 1997-2022",
    titleWidth = 330
  ),
  
  
  # --------- SIDEBAR
  dashboardSidebar(
    width = 330,
    
    sidebarMenu(
      menuItem("UMR Tahun 2022", tabName = "overview", icon = icon("eye")),
      menuItem("Grafik Per-Tahun", tabName = "year_analysis", icon = icon("calendar-day")),
      menuItem("Grafik Per-Provinsi", tabName = "province_analysis", icon = icon("map")),
      menuItem("Dataset", tabName = "data", icon = icon("server"))
    )
  ),
  
  
  # --------- BODY
  dashboardBody(
  tabItems(
  
  # --------- PAGE 1
    tabItem(tabName = "overview",
      
      # --------- ROW 1
      fluidPage(   
        h2("Rangkuman Nilai UMR Per-Provinsi pada Tahun 2022", align = "center", style = 'color:teal;'),
        br()
      ),
      
      # -------- ROW 1a
      fluidRow(
        valueBoxOutput("max2022", width = 6),
        valueBoxOutput("min2022", width = 6)
      ),
      
      # -------- ROW 1b
      fluidRow(
        infoBoxOutput("mean2022", width = 12)
      ),
      
      # -------- ROW 1c
      fluidPage(
        leafletOutput("mymap2022"),
        br()
      ),
      
      # --------- ROW 2
      fluidRow(
        valueBoxOutput("aceh2022", width = 3),
        valueBoxOutput("sumut2022", width = 3),
        valueBoxOutput("sumbar2022", width = 3),
        valueBoxOutput("riau2022", width = 3)
      ),
      
      # --------- ROW 3
      fluidRow(
        valueBoxOutput("jambi2022", width = 3),
        valueBoxOutput("sumsel2022", width = 3),
        valueBoxOutput("bengkulu2022", width = 3),
        valueBoxOutput("lampung2022", width = 3)
      ),
      
      # --------- ROW 4
      fluidRow(
        valueBoxOutput("bangkabelitung2022", width = 3),
        valueBoxOutput("kepriau2022", width = 3),
        valueBoxOutput("banten2022", width = 3),
        valueBoxOutput("jakarta2022", width = 3)
      ),
      
      # --------- ROW 5
      fluidRow(
        valueBoxOutput("jabar2022", width = 3),
        valueBoxOutput("jateng2022", width = 3),
        valueBoxOutput("yogya2022", width = 3),
        valueBoxOutput("jatim2022", width = 3)
      ),
      
      # --------- ROW 6
      fluidRow(
        valueBoxOutput("bali2022", width = 3),
        valueBoxOutput("ntb2022", width = 3),
        valueBoxOutput("ntt2022", width = 3),
        valueBoxOutput("kalbar2022", width = 3)
      ),
      
      # --------- ROW 7
      fluidRow(
        valueBoxOutput("kalteng2022", width = 3),
        valueBoxOutput("kalsel2022", width = 3),
        valueBoxOutput("kaltim2022", width = 3),
        valueBoxOutput("kalut2022", width = 3)
      ),
      
      # --------- ROW 8
      fluidRow(
        valueBoxOutput("sulut2022", width = 3),
        valueBoxOutput("sulteng2022", width = 3),
        valueBoxOutput("sulsel2022", width = 3),
        valueBoxOutput("sulteg2022", width = 3)
      ),
    
      # --------- ROW 9
      fluidRow(
        valueBoxOutput("gorontalo2022", width = 3),
        valueBoxOutput("sulbar2022", width = 3),
        valueBoxOutput("maluku2022", width = 3),
        valueBoxOutput("malukuutara2022", width = 3)
      ),
      
      # --------- ROW 10
      fluidRow(
        valueBoxOutput("papuabarat2022", width = 3),
        valueBoxOutput("papua2022", width = 3)
      )
    ),
    
  # --------- PAGE 2
    tabItem(tabName = "year_analysis",
            
      # -------- ROW 1
            fluidRow(
              box(width = 12,
                  selectInput(
                    inputId = "input_year",
                    label = "Pilih Tahun",
                    choices = unique(umr$Year)
                  ))
            ),
      
      # -------- ROW 2
          fluidRow(
            valueBoxOutput("maxyear", width = 6),
            valueBoxOutput("minyear", width = 6)
          ),
      
      # -------- ROW 3
          fluidRow(
            infoBoxOutput("meanyear", width = 12)
          ),
      
      # -------- ROW 4
      fluidPage(
              leafletOutput("mymap")
          )
    ),
  
  # --------- PAGE 3
    #---------- ROW 1
    tabItem(tabName = "province_analysis",

            # -------- ROW 1
            fluidRow(
              box(width = 12,
                  selectInput(
                    inputId = "input_province",
                    label = "Pilih Provinsi",
                    choices = unique(umr$Region)
                  )
              )
            ),
            
            # -------- ROW 2
            fluidRow(
              box(width = 12,
                  plotlyOutput("plot_province")
              )
            ),
    ),
  
  # --------- PAGE 4
  # -----PAGE 3: DATASET
    tabItem(tabName = "data",
          fluidRow(
            box(
              width = 12,
              title = h2("Data UMR Per-Provinsi Tahun 1997-2022", style = 'font-size: 30px; color:teal;', align = "center"),
              DT::dataTableOutput(outputId = "dataset_table")
            )
          )
    )
  )
  )
)

