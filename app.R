##### TASKS #####

  ##### TO DO #####

  # Implement all (future) time series plots with plotly/ts_plot
  
  # Fix dates on barplots
  
  # Implement data collecting from third parties
  
  # Implement monthly barplots 
  
  # Implement a demo of scatterplot/heatmap/causal visualization?
  
  ##### DONE #####
  
  # Fix map colours 

  # Fix Entry/Exit layout (kinda)

##### SOURCE AUXILIARY SCRIPTS #####

source("~/R/Shiny_demo/Shiny-demo-functions.R")

source("~/R/Shiny-demo-scraper.R")

source("~/R/Shiny_demo/Shiny-demo-auxiliary.R")

source("~/R/Shiny_demo/Shiny-demo-wrangling.R")

##### DEFINE UI #####

ui <- navbarPage(
    
    windowTitle="Yritysryhma",
    id="navbarID",
    
    tabPanel("Loadbearing tab"),
    
    ##### BANKRUPTCY SECTION #####
    tabPanel(title="Etusivu",
             value=etusivu_url,
             
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   column(12, valueBoxOutput("vbox1")),
                   column(12, valueBoxOutput("vbox2")),
                   column(12, highchartOutput("firmTreeMap", height="800px"))
                 )
               ),
               
               mainPanel(
                 tags$head(tags$style(".rightAlign{float:right;}")),
                 fluidRow(
                   column(12, img(id="image1", src="logo1.jpg", style="cursor:pointer;"), useShinyjs()),
                   column(12, img(id="image2", src="konkurssit.jpg", style="cursor:pointer;"), useShinyjs()),
                   column(12, img(id="image3", src="kokeelliset.jpg", style="cursor:pointer;"), useShinyjs()),
                   column(12, img(id="image4", src="tuottavuus.jpg", style="cursor:pointer;"), useShinyjs()),
                   column(12, tags$figure(class= "centerFigure", tags$img(
                     src="whitespace.png", align="center",
                     width=1200,
                     alt=" "
                   ),tags$figcaption(" "))),
                   column(12, includeMarkdown("Shiny-demo-markdown-1.markdown")),
                   column(12, tags$figure(class= "centerFigure", tags$img(
                     src="whitespace.png", align="center",
                     width=1200,
                     alt=" "
                   ),tags$figcaption(" "))),
                   
                   column(12, splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("bankruptcySeries"), plotlyOutput("payrollSeries"))),
                   column(12, splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("inflationSeries"), plotlyOutput("cycleSeries"))),
                   
                   #column(12, plotlyOutput("bankruptcySeries")),
                   
                   column(12, tags$figure(class= "centerFigure", tags$img(
                     src="whitespace.png", align="center",
                     width=1200,
                     alt=" "
                   ),tags$figcaption(" "))),
                   column(12, plotlyOutput("searchInterest")),
                 )
               )
             )
    ),
    
    navbarMenu(title="Bankruptcies",
               
               ##### BANKRUPTCY GRAPHPLOT id 12 #####
               
               tabPanel(
                 title="Time Series (Graph)",
                 #value=bankruptcy_time_series_graph_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates12", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries12", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries12", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions12", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions12", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     fluidRow(
                       column(12,plotOutput("timeSeries")),
                       column(12,plotOutput("MatPlotIndustry")),
                       column(12,plotOutput("MatPlotRegion")))
                   )  
                 )
               ),
               
               ##### BANKRUPTCY BARPLOT id 13 #####
               
               tabPanel(
                 title="Time Series (Bar)",
                 value=bankruptcy_time_series_bar_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates13", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries13", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries13", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions13", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions13", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     fluidRow(
                       column(12,plotOutput("DecompTimeSeriesRegion")),
                       column(12,plotOutput("DecompTimeSeriesIndustry")))
                   ) 
                 )
               ),
               
               ##### BANKRUPTCY HISTOGRAM id 11 #####
               
               tabPanel(
                 title="Histogram",
                 value=bankruptcy_histogram_url,
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates11", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries11", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries11", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions11", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions11", label="Select/Deselect all regions"),
                     
                     textInput("toptrim11", "Select the fraction of largest firms to remove from the sample", value="0.015")),
                   
                   mainPanel(
                     plotOutput("Histogram")
                   )
                   
                 )
               ),
               
               ##### BANKRUPTCY MAP id 14 #####
               
               tabPanel(
                 title="Map",
                 value=bankruptcy_map_url,
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates14", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries14", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries14", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions14", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions14", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     plotOutput("FinMapPlot")
                   )   
                 )
               ),   
               
               ##### BANKRUPTCY PIECHARTS id 15 #####
               
               tabPanel(
                 title ="Pie Chart",
                 value=bankruptcy_pie_chart_url,
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates15", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries15", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries15", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions15", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions15", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     fluidRow(
                       column(12,plotOutput("PieChartRegion")),
                       column(12,plotOutput("PieChartIndustry")))
                   )
                 )
               ),
    ),
    
    ##### MISCALLENIOUS SECTION #####
    
    navbarMenu(title= "Miscallenious", #id="tab2",
               
               ##### INVESTMENT CHANGE BARS id 31 #####
               
               tabPanel(
                 title="Investoinnit, muutos",
                 #value=miscallenious_investment_change_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons(inputId = "choice31", label="Valitse toimiala", 
                                  choices = investmentchoices31),
                     
                     checkboxGroupInput("quartals31", "Valitse vuosineljännekset:",
                                        quartalchoicevec,
                                        selected=quartalcharvec),
                     
                     actionLink("selectallquartals31", label="Valitse kaikki/Poista valinnat")),
                   
                   
                   mainPanel(
                     
                     fluidRow(
                       column(12,plotlyOutput("InvestmentChange"), height="100%"))
                   )
                 )
               ),
               
               ##### ENTRY AND EXIT BARS id 32 #####
               
               tabPanel(
                 title="Entry ja Exit",
                 value=miscallenious_entry_and_exit_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput(inputId = "regionchoice32", label="Valitse maakunta:", choices = regionchoicevec32,
                                 selected = regionchoicevec32[1], multiple = FALSE, selectize = FALSE),
                     
                     selectInput(inputId = "industrychoice32", label="Valitse toimiala:", 
                                 choices = TOLcharvec, multiple = FALSE, selectize = FALSE),
                     
                     checkboxGroupInput("years32", "Valitse vuodet:",
                                        yearchoicevec,
                                        selected=yearcharvec),
                     
                     actionLink("selectallyears32", label="Valitse kaikki/Poista valinnat"),
                     
                     checkboxGroupInput(inputId = "graphchoice32", "Muokkaa esitysmuotoa:",
                                        c("Samassa kuvaajassa", "Näytä vuosineljännekset", "Näytä normalisoitu vaihtuvuus (kartta)"), 
                                        selected = c("Näytä vuosineljännekset", "Näytä normalisoitu vaihtuvuus (kartta)")),
                     fluidRow(
                       column(12, plotlyOutput("yearlyentryexit1")),
                       column(12, plotOutput("turnoverMap"))
                     )),
                   
                   
                   mainPanel(
                     
                     fluidRow(
                       column(12,plotlyOutput("TotalEntry_TotalDouble")),
                       column(12,plotlyOutput("TotalExit_PropDouble")),
                       column(12,plotlyOutput("PropEntry")),
                       column(12,plotlyOutput("PropExit")),
                       column(12,plotlyOutput("turnoverIndudstry")))
                   )
                 )
               ),
               
               ##### SUBSIDY BARS id 33 #####
               
               tabPanel(
                 title="Yritystuet",
                 value=miscallenious_subsidies_url,
                 
                 sidebarLayout(      
                   sidebarPanel(
                     
                     selectInput(inputId = "industrychoice33", label="Valitse toimiala:", choices = TOLcharvec33,
                                 selected = TOLchoicevec33[1], multiple = FALSE, selectize = FALSE),
                     
                     selectInput(
                       inputId = "variablechoice33", label="Valitse muuttuja:", choices = variablechoicevec33,
                       selected = variablechoicevec33[1], multiple = FALSE, selectize = FALSE),
                     
                     checkboxGroupInput(inputId = "graphchoice33", "Muokkaa esitysmuotoa:",
                                        c("Näytä suhteelliset lukumäärät (yritykset)"))),
                   
                   mainPanel(
                     
                     fluidRow(
                       column(12,plotlyOutput("subsidyPlot", height="100%")))
                   )
                 )
               ), 
               
               ##### REVENUE SERIES id 34 #####
               
               tabPanel(
                 title="Liikevaihtoennakot",
                 value=miscallenious_revenue_url,
                 sidebarLayout(      
                   sidebarPanel(
                     
                     selectInput(inputId = "industrychoice34", label="Valitse toimiala:", choices = TOLchoicevec34,
                                 selected = TOLchoicevec34[1], multiple = FALSE, selectize = FALSE),
                     
                     selectInput(
                       inputId = "variablechoice34", label="Valitse sarja:", choices = variablechoicevec34,
                       selected = variablechoicevec34[1], multiple = FALSE, selectize = FALSE),
                     
                     checkboxGroupInput(inputId = "graphchoice34", "Muokkaa esitysmuotoa:",
                                        c("Näytä indeksin perusvuosi"))),
                   
                   mainPanel(
                     
                     fluidRow(
                       column(12,plotlyOutput("revenuePlot", height="100%"))
                   )
                 )
               )
               )
               
    ),
    ##### PRODUCTIVITY SECTION #####
    
    navbarMenu(title= "Productivity Estimates",
               
               ##### PRODUCTIVITY GRAPHPLOT id 22 #####
               
               tabPanel(
                 title="Productivity Time Series (Graph)",
                 #value=productivity_histogram_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     includeMarkdown("~/R/Shiny-demo-markdown-1.md")
                   )  
                 )
               ),
               
               ##### PRODUCTIVITY BARPLOT id 23 #####
               
               tabPanel(
                 title="Time Series (Bar)",
                 value=productivity_time_series_graph_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     includeMarkdown("~/R/Shiny-demo-markdown-1.md")
                   )
                 )
               ),
               
               ##### PRODUCTIVITY HISTOGRAM id 21 #####
               
               tabPanel(
                 title="Histogram",
                 value=productivity_time_series_bar_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     
                     actionLink("selectallregions", label="Select/Deselect all regions"),
                     
                     textInput("toptrim", "Select the fraction of largest firms to remove from the sample", value="0.015")),
                   
                   mainPanel(
                     includeMarkdown("~/R/Shiny-demo-markdown-1.md")
                   )
                   
                 )
               ),
               
               ##### PRODUCTIVITY MAP id 24 #####
               
               tabPanel(
                 title="Map",
                 value=productivity_map_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     dateRangeInput("dates", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries", "Select Industries:",
                                        industrychoicevec,
                                        selected = industrycharvec),
                     
                     actionLink("selectallindustries1", label="Select/Deselect all industries"),
                     
                     checkboxGroupInput("regions", "Select Regions:",
                                        regionchoicevec,
                                        selected = regioncharvec),
                     actionLink("selectallregions", label="Select/Deselect all regions")),
                   
                   mainPanel(
                     includeMarkdown("~/R/Shiny-demo-markdown-1.md")))
               )
      ),
               
               tabPanel(title="info",
               value="information_url",
               
               includeMarkdown("~/R/Shiny-demo-markdown-1.md")) 
    )
  


##### SERVER #####

server <- function(input, output, session) {
  
  observeEvent(input$navbarID, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarID)
    if(is.null(currentHash) || currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  
  observeEvent(session$clientData$url_hash, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    if(is.null(input$navbarID) || !is.null(currentHash) && currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateNavbarPage(session, "navbarID", selected = currentHash)
    }
  }, priority = 1)
  
  shinyjs::onclick("image1", updateNavbarPage(session, inputId="navbarID", selected="information_url"))
  shinyjs::onclick("image2", updateNavbarPage(session, inputId="navbarID", selected="Time Series (Graph)")) 
  shinyjs::onclick("image3", updateNavbarPage(session, inputId="navbarID", selected="Investoinnit, muutos"))
  shinyjs::onclick("image4", updateNavbarPage(session, inputId="navbarID", selected="Productivity Time Series (Graph)"))
  
  ##### SELECT ALL HISTOGRAM #####
  
  observe({
    if(input$selectallindustries11 == 0)  {return(NULL)}
    else if (input$selectallindustries11%%2==0) {
      updateCheckboxGroupInput(session,"industries11", "Select Industries:",choices=industrychoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"industries11", "Select Industries:",choices=industrychoicevec,selected=industrychoicevec)
    }
    
  })
  
  observe({
    if(input$selectallregions11 == 0) {return(NULL)}
    else if (input$selectallregions11%%2==0) {
      updateCheckboxGroupInput(session,"regions11", "Select Regions:",choices=regionchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"regions11", "Select Regions:",choices=regionchoicevec,selected=regionchoicevec)
    }
  })
  
  ##### SELECT ALL GRAPHPLOT #####
  
  observe({
    if(input$selectallindustries12 == 0)  {return(NULL)}
    else if (input$selectallindustries12%%2==0) {
      updateCheckboxGroupInput(session,"industries12", "Select Industries:",choices=industrychoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"industries12", "Select Industries:",choices=industrychoicevec,selected=industrychoicevec)
    }
    
  })
  
  observe({
    if(input$selectallregions12 == 0) {return(NULL)}
    else if (input$selectallregions12%%2==0) {
      updateCheckboxGroupInput(session,"regions12", "Select Regions:",choices=regionchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"regions12", "Select Regions:",choices=regionchoicevec,selected=regionchoicevec)
    }
  })
  
  ##### SELECT ALL BARPLOT #####
  
  observe({
    if(input$selectallindustries13 == 0)  {return(NULL)}
    else if (input$selectallindustries13%%2==0) {
      updateCheckboxGroupInput(session,"industries13", "Select Industries:",choices=industrychoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"industries13", "Select Industries:",choices=industrychoicevec,selected=industrychoicevec)
    }
    
  })
  
  observe({
    if(input$selectallregions13 == 0) {return(NULL)}
    else if (input$selectallregions13%%2==0) {
      updateCheckboxGroupInput(session,"regions13", "Select Regions:",choices=regionchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"regions13", "Select Regions:",choices=regionchoicevec,selected=regionchoicevec)
    }
  })
  
  ##### SELECT ALL MAP #####
  
  observe({
    if(input$selectallindustries14 == 0)  {return(NULL)}
    else if (input$selectallindustries14%%2==0) {
      updateCheckboxGroupInput(session,"industries14", "Select Industries:",choices=industrychoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"industries14", "Select Industries:",choices=industrychoicevec,selected=industrychoicevec)
    }
    
  })
  
  observe({
    if(input$selectallregions14 == 0) {return(NULL)}
    else if (input$selectallregions14%%2==0) {
      updateCheckboxGroupInput(session,"regions14", "Select Regions:",choices=regionchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"regions14", "Select Regions:",choices=regionchoicevec,selected=regionchoicevec)
    }
  })
  
  ##### SELECT ALL PIECHARTS #####
  
  observe({
    if(input$selectallindustries15 == 0)  {return(NULL)}
    else if (input$selectallindustries15%%2==0) {
      updateCheckboxGroupInput(session,"industries15", "Select Industries:",choices=industrychoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"industries15", "Select Industries:",choices=industrychoicevec,selected=industrychoicevec)
    }
    
  })
  
  observe({
    if(input$selectallregions15 == 0) {return(NULL)}
    else if (input$selectallregions15%%2==0) {
      updateCheckboxGroupInput(session,"regions15", "Select Regions:",choices=regionchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"regions15", "Select Regions:",choices=regionchoicevec,selected=regionchoicevec)
    }
  })
  
  ##### SELECT ALL QUARTALS #####
  
  observe({
    if (input$selectallquartals31 == 0) {return(NULL)}
    else if (input$selectallquartals31%%2==0) {
      updateCheckboxGroupInput(session,"quartals31", "Valitse vuosineljännekset:",choices=quartalchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"quartals31", "Valitse vuosineljännekset:",choices=quartalchoicevec,selected=quartalchoicevec)
    }
  })
  
  ##### SELECT ALL YEARS (id 32) #####
  
  observe({
    if (input$selectallyears32 == 0) {return(NULL)}
    else if (input$selectallyears32%%2==0) {
      updateCheckboxGroupInput(session,"years32", "Valitse vuosineljännekset:",choices=yearchoicevec)
    }
    else  {
      updateCheckboxGroupInput(session,"years32", "Valitse vuosineljännekset:",choices=yearchoicevec,selected=yearchoicevec)
    }
  })
  
  ##### REACTIVE DATA SELECTION #####
  
  data01 <- reactive({
    
    interestdata <- gtrendsR::gtrends(keyword = keywords, geo="FI", time="2017-05-24 2023-05-24", gprop="web")
    
    relevantinterestdata <- interestdata$interest_over_time[,c(1:3)]
    
    relevantinterestdata <- data.frame(date = as.POSIXct(relevantinterestdata$date[relevantinterestdata$keyword == keywords[1]]), lama = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "lama"]), konkurssit = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "konkurssit"]),
                                       yritystuet = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "yritystuet"]), ansiosidonnainen = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "ansiosidonnainen"]))
    
  })
  
  data11 <- reactive ({
    
    years <- fetchyears(x1=input$dates11[1], x2=input$dates11[2], x3=input$industries11, x4=input$regions11)
    industries <- fetchindustries(x1=input$dates11[1], x2=input$dates11[2], x3=input$industries11, x4=input$regions11)
    regions <- fetchregions(x1=input$dates11[1], x2=input$dates11[2], x3=input$industries11, x4=input$regions11)
    assets <- fetchassets(x1=input$dates11[1], x2=input$dates11[2], x3=input$industries11, x4=input$regions11)
    data <- data.frame(year = years , industries=industries, regions = regions, assets = assets)
    
  })
  
  data12 <- reactive ({
    
    years <- fetchyears(x1=input$dates12[1], x2=input$dates12[2], x3=input$industries12, x4=input$regions12)
    industries <- fetchindustries(x1=input$dates12[1], x2=input$dates12[2], x3=input$industries12, x4=input$regions12)
    regions <- fetchregions(x1=input$dates12[1], x2=input$dates12[2], x3=input$industries12, x4=input$regions12)
    assets <- fetchassets(x1=input$dates12[1], x2=input$dates12[2], x3=input$industries12, x4=input$regions12)
    data <- data.frame(year = years , industries=industries, regions = regions, assets = assets)
    
  })
  
  data13 <- reactive ({
    
    years <- fetchyears(x1=input$dates13[1], x2=input$dates13[2], x3=input$industries13, x4=input$regions13)
    industries <- fetchindustries(x1=input$dates13[1], x2=input$dates13[2], x3=input$industries13, x4=input$regions13)
    regions <- fetchregions(x1=input$dates13[1], x2=input$dates13[2], x3=input$industries13, x4=input$regions13)
    assets <- fetchassets(x1=input$dates13[1], x2=input$dates13[2], x3=input$industries13, x4=input$regions13)
    data <- data.frame(year = years , industries=industries, regions = regions, assets = assets)
    
  })
  
  data14 <- reactive ({
    
    years <- fetchyears(x1=input$dates14[1], x2=input$dates14[2], x3=input$industries14, x4=input$regions14)
    industries <- fetchindustries(x1=input$dates14[1], x2=input$dates14[2], x3=input$industries14, x4=input$regions14)
    regions <- fetchregions(x1=input$dates14[1], x2=input$dates14[2], x3=input$industries14, x4=input$regions14)
    assets <- fetchassets(x1=input$dates14[1], x2=input$dates14[2], x3=input$industries14, x4=input$regions14)
    data <- data.frame(year = years , industries=industries, regions = regions, assets = assets)
    
  })
  
  data15 <- reactive ({
    
    years <- fetchyears(x1=input$dates15[1], x2=input$dates15[2], x3=input$industries15, x4=input$regions15)
    industries <- fetchindustries(x1=input$dates15[1], x2=input$dates15[2], x3=input$industries15, x4=input$regions15)
    regions <- fetchregions(x1=input$dates15[1], x2=input$dates15[2], x3=input$industries15, x4=input$regions15)
    assets <- fetchassets(x1=input$dates15[1], x2=input$dates15[2], x3=input$industries15, x4=input$regions15)
    data <- data.frame(year = years , industries=industries, regions = regions, assets = assets)
    
  })
  
  data31 <- reactive({
    
    data <- investmentchangedata[-(19:50),]
    
    namerow <- data[2,]
    
    namerow[1] <- "vuosineljännes"
    
    data <- data[-(1:3),]
    data <- data[-nrow(data),]
    
    colnames(data) <- namerow
    
    data <- data.frame(data, vuosi = as.numeric(substr(data$vuosineljännes, 1, 4)),
                       neljännes = as.character(substr(data$vuosineljännes, 5, 6)))
    
    colnames(data) <- c("vuosineljännes", investmentchoices31, "vuosi", "neljännes")
    
    relevant <- data[colnames(data) == input$choice31]
    
    char = as.character(input$choice31)
    
    data <- data.frame(vuosineljännes = data$vuosineljännes, char = relevant, vuosi = data$vuosi, neljännes = data$neljännes)
    
    relevantquartals <- input$quartals31
    
    data <- data[is.element(as.character(data$vuosineljännes),relevantquartals),]
    
  })
  
  data32 <- reactive({
    
    ##### REGION #####
    
    indvec <- which(grepl(input$regionchoice32, as.character(entryexitdata$kategoria)))
    
    indvec1 <- which(grepl(input$regionchoice32, as.character(entryexitdata$kategoria)))
    
    len <- length(indvec)
    ncat <- 111
    
    for (k in 1:len) {
      for (i in 1:ncat) {
        indvec <- append(indvec, indvec1[k]+i-1, i + (k-1)*ncat)
      }
    }
    
    entryexitdata2 <- entryexitdata[indvec,]
    
    for (k in 1:length(bigquartalcharvec)) {
      for (i in 1:ncat) {
        entryexitdata2$vuosineljännes[(k-1)*ncat + i+1] <- bigquartalcharvec[k]
      }
    }
    
    entryexitdata2 <- data.frame(vuosineljännes = entryexitdata2$vuosineljännes, luokitus = entryexitdata2$luokitus, nentry = entryexitdata2$nentry, 
                                 nexit = entryexitdata2$nexit, ntotal = entryexitdata2$ntotal)
    
    entryexitdata2 <- entryexitdata2[-c(1),] 
    
    ##### TOL #####
    
    luokitusnimi <- as.character(input$industrychoice32)
    
    data1 <- entryexitdata2[entryexitdata2$luokitus == luokitusnimi,]
    data1 <- data1[is.element(as.character(substr(data1$vuosineljännes, 1, 4)), input$years32),]
    
    
  })
  
  data321 <- reactive({
    
    turnoverdata <- entryexitdata
    turnoverdata <- turnoverdata[-c(1:2),]
    turnoverdata$kategoria <- zoo::na.locf(turnoverdata$kategoria)
    turnoverdata <- turnoverdata[turnoverdata$luokitus == input$industrychoice32,]
    turnoverdata <- turnoverdata[(! grepl("MA1", turnoverdata$kategoria))&(!grepl("MA2", turnoverdata$kategoria)),]
    turnoverdata$turnoverrate <- (as.numeric(turnoverdata$nentry))/(as.numeric(turnoverdata$ntotal)) + (as.numeric(turnoverdata$nexit))/(as.numeric(turnoverdata$ntotal))
    turnoverregional <- aggregate(turnoverrate~kategoria, data=turnoverdata, FUN=mean)
    
    if ("Näytä normalisoitu vaihtuvuus (kartta)" %in% input$graphchoice32) {
      turnoverregional$normalized <- as.numeric(turnoverregional$turnoverrate)/(as.numeric(turnoverregional[1,2]))
    } else {
      turnoverregional$normalized <- as.numeric(turnoverregional$turnoverrate)
    }
    
    turnoverregional <- turnoverregional[-c(1),]
    turnoverregional$NUTS3 <- plotvec
    return(turnoverregional)
    
  })
  
  data33 <- reactive({
    
    muuttujanimi <- as.character(input$variablechoice33)
    
    newcolumn <- c()
    for (i in 1:length(cnamevec)) {
      if (muuttujanimi == as.character(cnamevec[i])) {
        newcolumn <- handoutdata[,i]
      }   
    }
    
    newcolumn <- as.numeric(newcolumn)
    
    if ((grepl("lukumäärä", muuttujanimi) == TRUE) & (length(input$graphchoice33)==1)) {
      newcolumn <- newcolumn/propvec
      
    }
    
    handoutdata <- data.frame(vuosineljännes=handoutdata$vuosineljännes, TOL = handoutdata$TOL, 
                              kokoluokka = handoutdata$kokoluokka, vuosi = as.numeric(substr(handoutdata$vuosineljännes, 1, 4)),
                              neljännes = as.character(substr(handoutdata$vuosineljännes, 5, 6)),
                              num = newcolumn)
    
    handoutdata <- handoutdata[handoutdata$TOL == as.character(input$industrychoice33),]
    
  })
  
  data34 <- reactive({
    
    data <- revenuedata
    
    print(revenuedata)
    
    if ((input$industrychoice34 != "Kaikki") & (input$variablechoice34 != "Kaikki")) {
      colnums <- which((grepl(input$industrychoice34,namerow)) & (grepl(input$variablechoice34,namerow)))
      data <- data[,c(1, colnums)]
      
    }
    
    else if ((input$industrychoice34 != "Kaikki") & (input$variablechoice34 == "Kaikki")) {
      colnums <- which(grepl(input$industrychoice34,namerow))
      data <- data[,c(1, colnums)]
    }
    
    else if ((input$variablechoice34 != "Kaikki") & (input$industrychoice34 == "Kaikki")) {
      colnums <- which(grepl(input$variablechoice34,namerow))
      data <- data[,c(1, colnums)]
    } else {
      data <- data
    }
    
    return(data)
    
  })
  
  data35 <- reactive({
    
    turnoverdata <- entryexitdata
    turnoverdata <- turnoverdata[-c(1:2),]
    turnoverdata$kategoria <- zoo::na.locf(turnoverdata$kategoria)
    turnoverdata <- turnoverdata[(grepl(input$regionchoice32, turnoverdata$kategoria)),]
    turnoverdata$turnoverrate <- (as.numeric(turnoverdata$nentry))/(as.numeric(turnoverdata$ntotal)) + (as.numeric(turnoverdata$nexit))/(as.numeric(turnoverdata$ntotal))
    turnoverindustry <- aggregate(turnoverrate~luokitus, data=turnoverdata, FUN=mean)
    turnoverindustrygroup <- turnoverindustry[is.na(as.numeric(substring(turnoverindustry$luokitus, 1, 2))),]
    
  })
  
  diffyears12 <- reactive({
    return(max(data12()$year)-min(data12()$year))
  })
  
  diffyears13 <- reactive({
    return(max(data13()$year)-min(data13()$year))
  })
  
  trimref <- reactive ({
    
    trimtop <- (1-as.numeric(input$toptrim11))
    trimmed <- unname(quantile(data11()$assets, trimtop))
    return(trimmed)
    
  })
  
  bins <- reactive({
    
    return(seq(min(data11()$assets, na.rm = TRUE), unname(trimref()), length.out = 50))
    
  })
  
  maxnobs <- reactive ({
    
    regions <- BRC$fakefinregions
    finrelevantnobsvec <- numeric(length(mydata$id))
    for (i in 1:length(mydata$id)) {
      finrelevantnobsvec[i] <- length(regions[regions == mydata$nutsname[i]])
    }
    maxnobs <- max(finrelevantnobsvec)
    
  })
  
  ##### DRAW PLOTS #####
  
    ##### BANKRUPTCY HISTOGRAM #####
  
    output$Histogram <- renderPlot({
        
        data <- data11()
      
        data <- data$assets[data$assets < trimref()]
        
        hist(data, xlim=c(min(data), max(data)), breaks = bins(), col = 'black', border = 'black',
          xlab = "Assets (USD millions, inflated)",
          main = "Histogram of bankruptcy sizes")
        
      })
  
    ##### BANKRUPTCY GRAPHPLOTS #####
      
    output$timeSeries <- renderPlot({
        
      data <- data12()
        
        h <- hist(data$year, breaks=diffyears12(), plot=TRUE)
        plot(x=h$mids, y=h$counts, type="o", col="darkblue", pch=16, xaxt="n", lwd=2, cex=1, ylab ="N.o. bankruptcies", xlab="year", 
             main = "N.o. bankruptcies, time series")
        axis(1, xaxp=c(1980,2020,40), las=2)
        
      })
  
    output$DecompTimeSeriesIndustry <- renderPlot({
      
      data <- data13()
      
      ggplot(data.frame(industry=data$industries, region=data$regions, year=data$year),
             aes(x=year, fill=industry), labs(y="N.o. bankrupcties", x="year")) +
        geom_histogram(bins=max(data$year)-min(data$year))
      
    })
    
    output$DecompTimeSeriesRegion <- renderPlot ({
      
      data <- data13()
      
      namesvec <- numeric(length(data$year))
      
      data <-data.frame(industry=data$industries, region=data$region, year=data$year, names=namesvec)
      
      for (i in 1:length(data$region)) {
        for (k in 1:length(regionlatnameconversion$name)) {
          if (data$region[i] == regionlatnameconversion$nutsname[k]) {
            data$names[i] <- regionlatnameconversion$name[k]
          }
        }
      }
      
      ggplot(data, aes(x=year, fill=names), labs(y="N.o. bankrupcties", x="year")) +
        geom_histogram(bins=max(data$year)-min(data$year))
      
    })
  
    ##### BANKRUPTCY BARPLOTS #####
    
    output$MatPlotIndustry <- renderPlot({
      
      fuel <- data12()
      
      hmatind <- matrix(0, diffyears12(), length(industrycharvec))
      hmatind <- as.data.frame((hmatind))
      
      for (i in 1:length(industrycharvec)) {
        for (k in min(fuel$year):max(fuel$year)) {
          hmatind[k-min(fuel$year),i] <- length(fuel$year[fuel$year == k & as.character(fuel$industries) == industrynamevec[i]])
        }
      }
      
      rownames(hmatind) <- c((min(fuel$year)+1):max(fuel$year))
      colnames(hmatind) <- industrycharvec
      
      for (i in 1:ncol(hmatind)) {
        if (sum(hmatind[, industrycharvec[i]]) == 0) {
          hmatind[, industrycharvec[i]] <- rep(NA, length(hmatind[,1]))
        }
      }
      
      matplot(hmatind, type="l", lty="solid", lwd="2", main="No. bankruptcies, decomposed by industry" ,xlab="year", ylab="N.o. bankruptcies")
      
    })
    
    output$MatPlotRegion <- renderPlot({
      
      fuel <- data12()
      
      hmatreg <- matrix(0, diffyears12(), length(regioncharvec))
      hmatreg <- as.data.frame((hmatreg))
      
      for (i in 1:length(regioncharvec)) {
        for (k in min(fuel$year):max(fuel$year)) {
          hmatreg[k-min(fuel$year),i] <- length(fuel$year[fuel$year == k & as.character(fuel$regions) == regioncharvec[i]])
        }
      }
      
      rownames(hmatreg) <- c((min(fuel$year)+1):max(fuel$year))
      colnames(hmatreg) <- regioncharvec
      
      for (i in 1:ncol(hmatreg)) {
        if (sum(hmatreg[, regioncharvec[i]]) == 0) {
          hmatreg[, regioncharvec[i]] <- rep(NA, length(hmatreg[,1]))
        }
      }
      
      matplot(hmatreg, type="l", lty="solid", lwd="2", main="No. bankruptcies, decomposed by region", xlab="year", ylab="N.o. bankruptcies")
      
    })
  
    ##### BANKRUPTCY MAPS #####
      
    output$FinMapPlot <-renderPlot({
      
      finrelevant <- data14()$regions
      finrelevantnobsvec <- numeric(length(mydata$id))
      
      for (i in 1:length(mydata$id)) {
        finrelevantnobsvec[i] <- length(finrelevant[finrelevant == mydata$nutsname[i]])
      }
      
      finrelevantnobsvec <- data.frame(nobs=finrelevantnobsvec, id=mydata$id)
      
      print(finrelevantnobsvec)
      
      mapplot <- map(finrelevantnobsvec, "nobs", "Map of bankruptcies", "This is a map of simulated bankruptcies", "id", 2)
      
      plot(mapplot)
      
      
    }, height=1000, width=800)
    
    output$turnoverMap <- renderPlot({
      
      turnoverregional <- data321()
      
      colnames(turnoverregional) <- c("name", "turnoverrate", "normalized", "nutsname")
      
      print(mydata)
      
      print(turnoverregional)
      
      mapdf <- join(mydata, turnoverregional, by = "nutsname")
      
      print(mapdf)
      
      mapplot <- map(mapdf, "normalized", "Yrityskannan Vaihtuvuus", "", "id", 4)
      
      plot(mapplot)
      
    }, height=850, width = 650) 
  
    ##### BANKRUPTCY PIECHARTS #####
  
    output$PieChartRegion <- renderPlot({
      
      regions <- data15()$regions
      finrelevantnobsvecreg <- numeric(length(mydata$id))
      for (i in 1:length(mydata$id)) {
        finrelevantnobsvecreg[i] <- length(regions[regions == mydata$nutsname[i]])
      }
      
      datareg <- data.frame(nobsreg = finrelevantnobsvecreg, id = mydata$id)
      
      datareg <- join(datareg, regionlatnameconversion, by="id")
      
      datareg <- na.omit(datareg)
      
      plotreg <- ggplot(datareg, aes(x="", y=nobsreg,fill=name)) + geom_bar(width = 1, stat = "identity")
      piereg <- plotreg + coord_polar("y", start=0)
      plot(piereg)
      
    })
      
    output$PieChartIndustry <- renderPlot({
      
      industries <- data15()$industries
      finrelevantnobsvecind <- numeric(length(industrynamevec))
      for (i in 1:length(industrynamevec)) {
        finrelevantnobsvecind[i] <- length(industries[industries == industrynamevec[i]])
      }
      
      dataind <- data.frame(nobsind = finrelevantnobsvecind, names = industrynamevec)
      
      dataind <- na.omit(dataind)
      
      plotind <- ggplot(dataind, aes(x="", y=nobsind,fill=names)) + geom_bar(width = 1, stat = "identity")
      pieind <- plotind + coord_polar("y", start=0)
      plot(pieind)
      
    })
    
    ##### INVESTMENT CHANGE BARS #####
    
    output$InvestmentChange <- renderPlotly({
      
      data <- data31()
      
      ggplot(data, aes(x = factor(neljännes), y = as.double(data[,2]), fill = as.character(vuosi), colour = vuosineljännes)) + geom_bar(position = "dodge", stat="identity", colour="black") + ylab("Vuosimuutos, prosenttia") + xlab("Vuosineljännes") + scale_fill_manual(values=DHcolors3)
      
      print(ggplotly())
      
    })
    
    ##### FIRM ENTRY AND EXIT BARS #####
    
    output$TotalEntry_TotalDouble <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, FALSE, FALSE)
        ggplotly()
      } else if (is.element("Samassa kuvaajassa", input$graphchoice32)& (is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, FALSE, FALSE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, TRUE, FALSE)
        ggplotly()
      } else if ((is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, TRUE, FALSE)
        ggplotly()
      }
    })
    
    output$TotalExit_PropDouble <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, FALSE, TRUE)
        ggplotly()
      } else if (is.element("Samassa kuvaajassa", input$graphchoice32)& (is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, FALSE, TRUE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, TRUE, TRUE)
        ggplotly()
      } else if ((is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, TRUE, TRUE)
        ggplotly()
      }
      
    })
    
    output$PropEntry <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, FALSE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, TRUE, FALSE)
        ggplotly()
      }
      
    })
    
    output$PropExit <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, TRUE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, TRUE, TRUE)
        ggplotly()
      }
      
    })
    
    output$yearlyentryexit1 <- renderPlotly({
      
      #if (is.element("Näytä yrityskanta",input$graphchoice32)) {
      
        plot <- stockplot(data32(), input$graphchoice32)
        ggplotly()
      
      #}
      
    })
  
    ##### FIRM SUBSIDY BARS #####
    
    output$subsidyPlot <- renderPlotly({
      
      handoutdata <- data33()

      if (grepl("lukumäärä", as.character(input$variablechoice33)) & length(input$graphchoice33)==1) {
        ggplot(handoutdata, aes(x=vuosineljännes, y = num, fill=factor(kokoluokka))) + xlab("Vuosineljännes") + ylab("Osuus") + geom_bar(position = "dodge", stat="identity") + scale_fill_manual(values=DHcolors)
        ggplotly()
        
      } else if (grepl("lukumäärä", as.character(input$variablechoice33)) & length(input$graphchoice33)==0) {
        ggplot(handoutdata, aes(x=vuosineljännes, y = num, fill=factor(kokoluokka))) + xlab("Vuosineljännes") + ylab("Yritystä") + geom_bar(position = "dodge", stat="identity") + scale_fill_manual(values=DHcolors)
        ggplotly()
        
      }
      
      else {
        ggplot(handoutdata, aes(x=vuosineljännes, y = num, fill=factor(kokoluokka))) + xlab("Vuosineljännes") + ylab("Tuhatta euroa") + geom_bar(position = "dodge", stat="identity") + scale_fill_manual(values=DHcolors)
        ggplotly()
        
      }

    })
    
    ##### REVENUE SERIES #####
    
    output$revenuePlot <- renderPlotly({
      
      revenuedata <- data34()
      
      if (length(input$graphchoice34) == 1) {
        
        revenueseries <- timeseries(revenuedata, "Yritysten liikevaihtoennakot, sarja", TRUE, "2015-08-01", DHcolors)
        
      } else if (length(input$graphchoice34) == 0) {
        
        revenueseries <- timeseries(revenuedata, "Yritysten liikevaihtoennakot, sarja", FALSE, "2015-08-01", DHcolors)
        
      }
      
    })
    
    ##### FIRM TYPE TREEMAP #####
    
    output$firmTreeMap <- renderHighchart({
      
        hchart(scrapedtable[-c(length(scrapedtable$Yritysmuoto)),],type="treemap",
        hcaes(x=Yritysmuoto, value=X2.1.2023, color=X2.1.2023), name = "Yritykset") %>%
        hc_colorAxis(stops = color_stops(colors = DHcolors))
      
    })
    
    output$vbox1 <- renderValueBox({
      
      valueBox(
        
        paste(substring(as.character(scrapedtable[length(scrapedtable$Yritysmuoto),2]), 
                        c(1,3), c(3,5)), collapse=" "),
        "Yritysten lukumäärä Suomessa vuoden 2023 alussa"
        
        )
      
    })
    
    output$vbox2 <- renderValueBox({
      
      valueBox(
        
        paste(substring(as.character(scrapedtable[length(scrapedtable$Yritysmuoto), 2] - scrapedtable[length(scrapedtable$Yritysmuoto), 6]), 
                        c(1,2), c(2,4)), collapse=" "),
        "Yritysten lukumäärän muutos vuoteen 2019 verrattuna"
      )
      
      
    })
    
    output$searchInterest <- renderPlotly({
      
      data <- data01()
      
      plot <- timeseries(data, "Tiettyihin asiasanoihin kohdistuvat Google-haut", FALSE, "2005-01-01", DHcolors2)
      
    })
    
    output$bankruptcySeries <- renderPlotly({
      
      konkurssisarja <- time_series_wrangler(konkurssisarja)
      
      plot <- timeseries(konkurssisarja, "Konkurssit", TRUE, "2005-01-01", DHcolors)
      
    })
    
    output$inflationSeries <- renderPlotly({
      
      kuluttajahintaindeksi <- time_series_wrangler(kuluttajahintaindeksi)
      
      plot <- timeseries(kuluttajahintaindeksi, "Kuluttajahintaindeksi", TRUE, "2005-01-01", DHcolors)
      
    })
    
    output$payrollSeries <- renderPlotly({
      
      palkkasummaindeksi <- time_series_wrangler(palkkasummaindeksi)
      
      plot <- timeseries(palkkasummaindeksi, "Palkkasumma, indeksisarja", TRUE, "2015-01-01", DHcolors)
      
    })
    
    output$cycleSeries <- renderPlotly({
      
      tuotantosuhdanneindeksi <- time_series_wrangler(tuotantosuhdanneindeksi)
      
      plot <- timeseries(tuotantosuhdanneindeksi, "tuotantosuhdanne, indeksisarja", TRUE, "2015-01-01", DHcolors)
      
    })
    
  ##### NOT YET IMPLEMENTED #####
    ##### PRODUCTIVITY HISTOGRAM #####
    ##### PRODUCTIVITY GRAPHLOT #####
    ##### PRODUCTIVITY BARPLOT #####
    ##### PRODUCTIVITY MAP #####
      
  ##### NOT USED #####
  
    ##### US BANKRUPTCY MAP #####
      
    output$USmapPlot <-renderPlot({
        
        valvec <- c()
        
        for (i in 1:length(charvec)) {
          if (charvec[i] %in% input$industries){
            for (k in 1:length(na.omit(newdf[i,]))){
              valvec <- append(valvec, newdf[k,i])
            }
          }
        }
        
        valvec <- na.omit(valvec)
        
        newdf <- statepop[,c(1, 4)]
        state_abbrs <- statepop[,c(2)]
        
        BRC1 <- subset(BRC, is.element(floor(BRC$SICPrimary/100), valvec))
        
        for (i in 1:51) {
          newdf$pop_2015[i] <-length(BRC1$DistFiled[substr(BRC1$DistFiled, start=1,stop=2) == toString(state_abbrs[i,])])
        }
        
        colnames(newdf) <- c('fips', 'bankruptcies')
        
        plot_usmap(data = newdf, values="bankruptcies") + 
          scale_fill_continuous(
            low = "white", high = "red", name = "Bankruptcies", label = scales::comma
          ) +
          labs(title = "Bankruptcies, state level, 1979-2021",
               subtitle = "This is a map of US States and n.o. bankruptcies between selected years") + 
          theme(panel.background = element_rect(color = "black", fill = "lightblue"))
      })
    
    ##### INVESTIGATE #####
    
    output$timeSeriesSeparate <- renderPlot({
      
      data <- data12()
      
      h <- hist(data12$year, breaks=diffyears12(), plot=TRUE)
      plot(x=h$mids, y=h$counts, type="o", col="darkblue", pch=16, xaxt="n", lwd=2, cex=1, ylab ="N.o. bankruptcies", xlab="year", 
           main = "N.o. bankruptcies, time series")
      
    })
      
    ##### EXIT/ENTRY DOUBLE CHARTS, CURRENTLY EMBEDDED INTO SINGLE CHARTS AS CONDITIONAL OUTPUTS #####
    
    output$TotalDouble <- renderPlot({
      
      data1 <- data32()
      
      fuelabs <- data.frame(vuosineljännes = data1$vuosineljännes, 
                            nentry = as.numeric(data1$nentry), nexit = as.numeric(data1$nexit), total = as.numeric(data1$ntotal),
                            vuosi = as.numeric(substr(data1$vuosineljännes, 1, 4)),
                            neljännes = as.character(substr(data1$vuosineljännes, 5, 6)))
      
      entrytoexit <- data.frame(
        group = c(rep("nentry", nrow(data1)), rep("nexit", nrow(data1))),
        vuosineljännes = fuelabs$vuosineljännes,
        vuosi = fuelabs$vuosi,
        neljännes = fuelabs$neljännes,
        y = c(fuelabs$nentry, - fuelabs$nexit))
      
      exitentryplot <- ggplot(entrytoexit, aes(x = factor(neljännes), y = as.double(y), 
                                               fill = vuosi, colour = vuosineljännes)) + geom_bar(position = "dodge", stat="identity")
      
      if (vector.is.empty(input$graphchoice32) == FALSE) {
        plot(exitentryplot)
      }
      
    })
    
    output$PropDouble <- renderPlot({
      
      data1 <- data32()
      
      fuelfraq <- data.frame(vuosineljännes = data1$vuosineljännes,
                             nentry = as.numeric(data1$nentry)/as.numeric(data1$ntotal), nexit = as.numeric(data1$nexit)/as.numeric(data1$ntotal),
                             vuosi = as.numeric(substr(data1$vuosineljännes, 1, 4)),
                             neljännes = as.character(substr(data1$vuosineljännes, 5, 6)))
      
      entrytoexit <- data.frame(
        group = c(rep("nentry", nrow(data1)), rep("nexit", nrow(data1))),
        vuosineljännes = fuelfraq$vuosineljännes,
        vuosi = fuelfraq$vuosi,
        neljännes = fuelfraq$neljännes,
        y = c(fuelfraq$nentry, - fuelfraq$nexit))
      
      exitentryplot <- ggplot(entrytoexit, aes(x = factor(neljännes), y = as.double(y), 
                                               fill = vuosi, colour = vuosineljännes)) + geom_bar(position = "dodge", stat="identity")
      
      if (vector.is.empty(input$graphchoice32) == FALSE) {
        plot(exitentryplot)
      }
      
      
    })
    
    output$turnoverIndudstry <- renderPlotly({
      
      turnoverindustrygroup <- data35()
      
      bar <- ggplot(turnoverindustrygroup, aes(x=turnoverindustrygroup$turnoverrate, y=substr(turnoverindustrygroup$luokitus, 1, 60)))
      bar <- bar + geom_col(aes(fill = turnoverindustrygroup$turnoverrate, color=turnoverindustrygroup$turnoverrate))
      bar <- bar + ggtitle("Toimialakohtainen vaihtuvuus maakuntatasolla")
      ggplotly(bar)
      
    })
    
    exitentryplot <- function(data, entry, double, yearly, proportional) {
      
      data1 <- data
      
      if (proportional == TRUE) {
        
        data <- data.frame(vuosineljännes = data1$vuosineljännes,
                           nentry = as.numeric(data1$nentry)/as.numeric(data1$ntotal), nexit = as.numeric(data1$nexit)/as.numeric(data1$ntotal),
                           vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                           neljännes = as.character(substr(data1$vuosineljännes, 5, 6)),
                           group = c(rep("nentry", nrow(data1)), rep("nexit", nrow(data1))),
                           y = c(as.numeric(data1$nentry)/as.numeric(data1$ntotal), - as.numeric(data1$nexit)/as.numeric(data1$ntotal)))
        ylabtxt = "Osuus"
        
      } else if (proportional == FALSE) {
        
        data <- data.frame(vuosineljännes = data1$vuosineljännes, 
                           nentry = as.numeric(data1$nentry), nexit = as.numeric(data1$nexit), total = as.numeric(data1$ntotal),
                           vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                           neljännes = as.character(substr(data1$vuosineljännes, 5, 6)),
                           group = c(rep("nentry", nrow(data1)), rep("nexit", nrow(data1))),
                           y = c(as.numeric(data1$nentry), - as.numeric(data1$nexit)))
        ylabtxt = "Lukumäärä"
      }
      
      if (entry==TRUE & double == FALSE & yearly == FALSE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = nentry, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=DHcolors)
        plot <- plot + geom_hline(yintercept=0, color="red")
        
      } else if (entry==FALSE & double == FALSE & yearly == FALSE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = nexit, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=DHcolors)
        plot <- plot + geom_hline(yintercept=0, color="red")
        
      } else if (double == TRUE & yearly == FALSE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = y, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=DHcolors)
        plot <- plot + geom_hline(yintercept=0, color="red")
        
      } else if (entry==TRUE & double == FALSE & yearly == TRUE) {
        
        fuel <- data[,c(1:5)]
        fuel <- distinct(fuel, vuosineljännes, .keep_all=TRUE)
        
        print(fuel)
        
        yearlyentry <- aggregate(nentry~vuosi, data=fuel, FUN=sum)
        yearlyexit <- aggregate(nexit~vuosi, data=fuel, FUN=sum)
        
        fuel <- data.frame(
          nexit = yearlyexit$nexit, nentry = yearlyentry$nentry,
          vuosi = yearlyentry$vuosi)
        
        print(fuel)
        
        plot <- ggplot(fuel, aes(x = vuosi, y = nentry, fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=DHcolors)
        plot <- plot + geom_hline(yintercept=0, color="red")
        
      } else if (entry==FALSE & double == FALSE & yearly == TRUE) {
        
        fuel <- data[,c(1:5)]
        fuel <- distinct(fuel, vuosineljännes, .keep_all=TRUE)
        
        yearlyentry <- aggregate(nentry~vuosi, data=fuel, FUN=sum)
        yearlyexit <- aggregate(nexit~vuosi, data=fuel, FUN=sum)
        
        data <- data.frame(
          nexit = yearlyexit$nexit, nentry = yearlyentry$nentry,
          vuosi = yearlyentry$vuosi)
        
        plot <- ggplot(data, aes(x = vuosi, y = nexit, fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=DHcolors)
        plot <- plot + geom_hline(yintercept=0, color="red")
        
      } else if (double == TRUE & yearly == TRUE) {
        
        fuel <- data[,c(1:5)]
        fuel <- distinct(fuel, vuosineljännes, .keep_all=TRUE)
        
        yearlyentry <- aggregate(nentry~vuosi, data=fuel, FUN=sum)
        yearlyexit <- aggregate(nexit~vuosi, data=fuel, FUN=sum)
        
        data <- data.frame(
          nexit = yearlyexit, nentry = yearlyentry,
          vuosi = yearlyentry$vuosi,
          y = c(as.numeric(yearlyentry$nentry), - as.numeric(yearlyexit$nexit)))
        
        plot <- ggplot(data, aes(x = vuosi, y = y, fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=DHcolors)
        plot <- plot + geom_hline(yintercept=0, color="red")
        
      }
      
    }
    
    stockplot <- function(data, argument) {
        
      data1 <- data
      
      fuel <- data.frame(totstock <- data1$ntotal, vuosineljännes = data1$vuosineljännes,
                         vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                         neljännes = as.character(substr(data1$vuosineljännes, 5, 6)))
      
      plot <- ggplot(fuel, aes(x = vuosi, y = as.numeric(totstock), fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab("yristyskanta") + xlab("Vuosi")
      plot <- plot + scale_fill_manual(values=rep("black", 10))
      plot <- plot + geom_hline(yintercept=0, color="red")
      
    }
    
    map <- function(data, varname, main, text, id, decimals) {
      
      df <- join(mapdata, mydata, by="id")
      df <- join(df, coordinates, by="id")
      df <- join(df, data, by=id)
      
      for (i in 1:nrow(df)) {
        if (is.na(df$NUTS3[i]) == TRUE) {
          df$nobs[i] <- NA
        }
      }
      
      print(df)
      
      df <- df[, !duplicated(colnames(df))]
      
      gg <- ggplot() + geom_polygon(data = df, aes(x = long, y = lat, group = group, fill=round(df[,which(colnames(df)==varname)], decimals)), color = "darkgrey", size = 0.5)
      gg <- gg + scale_fill_continuous(low = "white", high = DHcolors[8], name = "Productivity estimates", limits=c(min(round(df[,which(colnames(df)==varname)], decimals)),max(round(df[,which(colnames(df)==varname)], decimals))), na.value="transparent",
                                       label = scales::comma)
      gg <- gg + geom_text(data=df, aes(meanlong, meanlat, label=round(df[,which(colnames(df)==varname)], decimals)), size =8)
      gg <- gg + coord_fixed(1)
      gg <- gg + theme_dark()
      gg <- gg + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')
      gg <- gg + theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      gg <- gg + labs(title = main,
                      subtitle = text)
      gg <- gg + theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
      gg <- gg + theme(panel.background = element_rect(color = "black", fill = "lightblue"))
      
      return(gg)
    }
    
    timeseries <- function(data, texts, argument, baseyear, colours) {
      
      timeseries <- TSstudio::ts_plot(data, title=texts, slider=TRUE) 
      
      if (argument & ncol(data)==2) {
        
        timeseries %>% add_segments(y=min(data[,-1]), x= as.POSIXct(baseyear), yend=max(data[,-1]), xend=as.POSIXct(baseyear), color = "black", showlegend=FALSE) |> layout(showlegend=FALSE, colorway=colours[runif(1, 0, length(colours))])
        
      } else if (argument & ncol(data)>2) {
        
        timeseries %>% add_segments(y=min(data[,-1]), x= as.POSIXct(baseyear), yend=max(data[,-1]), xend=as.POSIXct(baseyear), color = "black", showlegend=TRUE) |> layout(showlegend=TRUE, colorway=colours)
        
      } else {
        
        timeseries |> layout(showlegend=TRUE, colorway=colours)
      }
      
    }
    
}


##### RUN THE APP #####

shinyApp(ui = ui, server = server)
