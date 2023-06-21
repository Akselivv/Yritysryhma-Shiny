library(shiny)
library(plotly)
library(shinyjs)
library(ECharts2Shiny)
library(tidyverse)
library(highcharter)
library(shinydashboard)
library(markdown)
library(shinyWidgets)
library(shinyscreenshot)
library(ggplot2)
library(magrittr)

##### TASKS #####

  ##### TO DO #####

  # Lisää toimialakohtaiset aggregaattituottavuudet (Data jo olemassa)
  
  ##### DONE #####

##### SOURCE AUXILIARY SCRIPTS #####

source("Shiny-demo-functions.R")

source("Shiny-demo-scraper.R")

source("Shiny-demo-auxiliary.R")

source("Shiny-demo-wrangling.R")

##### DEFINE UI #####

ui <- navbarPage(
  
  useShinyjs(),
  
    windowTitle="Yritysryhmän demo",
    id="navbarID",
    
    ##### ETUSIVU #####
    
navbarMenu(
  title="Yritysryhmä",
      
  tabPanel(
      title="Yritysryhmän etusivu",
      
          sidebarLayout(
          sidebarPanel(
            fluidRow(
              actionButton("kuvankaappaus1", "Kuvankaappaus"),
              img(id="initialtag1", src="yritystag.jpg", style="cursor:pointer;"), 
              img(id="initialtag2", src="investointitag.jpg", style="cursor:pointer;"), 
              img(id="initialtag3", src="konkurssitag.jpg", style="cursor:pointer;"), 
              img(id="initialtag4", src="karttatag.jpg", style="cursor:pointer;"), 
              img(id="initialtag5", src="tuottavuustag.jpg", style="cursor:pointer;"), 
              column(12, valueBoxOutput("vbox1")),
              column(12, valueBoxOutput("vbox2")),
              column(12, highchartOutput("firmTreeMap", height="800px")),
              tags$a(
                
                href = "https://www.prh.fi/fi/kaupparekisteri/yritystenlkm/lkm.html",
                target = "_blank",
                valueBox(
                  
                  "",
                  "Lähde: Patentti- ja rekisterihallitus, Yritysten lukumäärät kaupparekisterissä"
                
                )  
              )
            )
          ),
          
          mainPanel(
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
              column(12, includeMarkdown("Shiny-demo-markdown-frontpage.markdown")),
              column(12, tags$figure(class= "centerFigure", tags$img(
                src="whitespace.png", align="center",
                width=1200,
                alt=" "
              ),tags$figcaption(" "))),
              
              column(12, splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("bankruptcySeries"), plotlyOutput("payrollSeries")))),
            fluidRow(tags$a(
              
              href = "https://stat.fi/tilasto/kony",
              target = "_blank",
              valueBox(
                
                "",
                "Lähde: Tilastokeskus. 13fb -- Konkurssit kuukausittain vuodesta 1986, 1986M01-2023M04", width=6
              ) 
              
            ), tags$a(
              
              href = "https://www.stat.fi/tilasto/ktps",
              target = "_blank",
              valueBox(
                
                "",
                "Lähde: Tilastokeskus. 111m -- Palkkasummakuvaajat toimialoittain kuukausitasolla (2015=100), 1995M01-2023M03", width=6
              ) 
              
            )),
            fluidRow(column(12, splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("inflationSeries"), plotlyOutput("cycleSeries")))),
            fluidRow(tags$a(
              
              href = "https://www.stat.fi/tilasto/khi",
              target = "_blank",
              valueBox(
                
                "",
                "Lähde: Tilastokeskus. 11xb -- Kuluttajahintaindeksi (2015=100), kuukausitiedot, 2015M01-2023M04", width=6
              ) 
              
            ), tags$a(
              
              href = "https://www.stat.fi/tilasto/ktkk",
              target = "_blank",
              valueBox(
                
                "",
                "Lähde: Tilastokeskus. 132f -- Tuotannon suhdannekuvaaja, kuukausittain, 1995M01-2023M03", width=6
              ) 
              
            )),
            fluidRow(   
              column(12, tags$figure(class= "centerFigure", tags$img(
                src="whitespace.png", align="center",
                width=1200,
                alt=" "
              ),tags$figcaption(" "))),
              column(12, plotlyOutput("searchInterest"))),
            fluidRow(valueBoxOutput("emptyvbox6", width = 8), tags$a(
              
              href = "https://trends.google.com/trends/explore?date=2017-06-14%202023-06-14&geo=FI&q=konkurssit,lama,yritystuet,ansiosidonnainen&hl=fi",
              target = "_blank",
              valueBox(
                
                "",
                "Lähde: Google Trends"
                
              ) 
              
              )
          )
        )
      )
    ),
    
    ##### BANKRUPTCY SECTION #####
               
               ##### BANKRUPTCY GRAPHPLOT id 12 #####
  
               tabPanel(
                 title="Konkurssit",
                 #value=bankruptcy_time_series_graph_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     actionButton("kuvankaappaus2", "Kuvankaappaus"),
                     
                     img(id="tag311", src="konkurssitag.jpg", style="cursor:pointer;"), 
                     
                     dateRangeInput("dates12", "Select dates:",
                                    start= "1980-01-01",
                                    end = as.character(Sys.Date())),
                     
                     checkboxGroupInput("industries12", "Valitse toimialat:",
                                        industrychoicevec,
                                        selected = industrychoicevec),
                     
                     actionLink("selectallindustries12", label="Valitse kaikki/Poista valinnat"),
                     
                     checkboxGroupInput("regions12", "Valitse maakunnat:",
                                        regionchoicevec,
                                        selected = regionchoicevec),
                     actionLink("selectallregions12", label="Valitse kaikki/Poista valinnat"),
                     checkboxGroupInput("graphchoice12", "Muokkaa Esitystapaa:", c("Palkit"), selected=NULL),
                     plotOutput("FinMapPlot")),
                   
                   mainPanel(
                     fluidRow(
                       column(12,plotlyOutput("timeSeries")),
                       column(12,plotlyOutput("DecompTimeSeriesRegion")),
                       column(12,plotlyOutput("DecompTimeSeriesIndustry")),
                       fluidRow(valueBoxOutput("emptyvbox7", width = 8), tags$a(
                         
                         href = "https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__kony/statfin_kony_pxt_13fb.px/",
                         target = "_blank",
                         valueBox(
                           
                           "",
                           "Lähde: Tilastokeskus. 13fe -- Konkurssit kuukausittain alueittain ja toimialoittain, 2003M01-2023M05"
                           
                         ) 
                         
                       )
                       ))
                   )  
                 )
               ),
    
    
    ##### MISCALLENIOUS SECTION #####
               
               ##### INVESTMENT CHANGE BARS id 31 #####
  
               tabPanel(
                 title="Investoinnit, muutos",
                 #value=miscallenious_investment_change_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     actionButton("kuvankaappaus3", "Kuvankaappaus"),
                     
                     img(id="tag131", src="yritystag.jpg", style="cursor:pointer;"), 
                     img(id="tag231", src="investointitag.jpg", style="cursor:pointer;"), 
                     radioButtons(inputId = "choice31", label="Valitse toimiala", 
                                  choices = investmentchoices31),
                     
                     checkboxGroupInput("quartals31", "Valitse vuosineljännekset:",
                                        quartalchoicevec,
                                        selected=quartalcharvec),
                     actionLink("selectallquartals31", label="Valitse kaikki/Poista valinnat"),
                     checkboxGroupInput("graphchoice31", "Valitse esitysmuoto:",
                     "Palkit", selected=NULL)),
                   
                   
                   mainPanel(
                     
                     fluidRow(
                       column(12,plotlyOutput("InvestmentChange"), height="100%")),
                     fluidRow(
                       valueBoxOutput("emptyvbox", width = 8), tags$a(
                         
                         href = "https://www.stat.fi/tup/kokeelliset-tilastot/yritysten-investoinnit/index.html",
                         target = "_blank",
                         valueBox(
                           
                           "",
                           "Lähde: Tilastokeskus, Yritysten investoinnit"
                         ) 
                         
                       ))
                   )
                 )
               ),
               
               ##### ENTRY AND EXIT BARS id 32 #####
  
                tabPanel(
                 title="Aloittaneet ja Lopettaneet Yritykset",
                 #value=miscallenious_entry_and_exit_url,
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     actionButton("kuvankaappaus4", "Kuvankaappaus"),
                     
                     img(id="tag132", src="yritystag.jpg", style="cursor:pointer;"), 
                     img(id="tag432", src="karttatag.jpg", style="cursor:pointer;"), 
                     
                     selectInput(inputId = "regionchoice32", label="Valitse maakunta:", choices = regionchoicevec32,
                                 selected = regionchoicevec32[1], multiple = FALSE, selectize = FALSE),
                     
                     selectInput(inputId = "industrychoice32", label="Valitse toimiala:", 
                                 choices = TOLcharvec, multiple = FALSE, selectize = FALSE),
                     
                     checkboxGroupInput("years32", "Valitse vuodet:",
                                        yearchoicevec,
                                        selected=yearcharvec),
                     
                     actionLink("selectallyears32", label="Valitse kaikki/Poista valinnat"),
                     
                     checkboxGroupInput(inputId = "graphchoice32", "Muokkaa esitysmuotoa:",
                                        c("Samassa kuvaajassa", "Näytä vuosineljännekset", "Näytä normalisoitu vaihtuvuus (kartta)", "Aikasarjana",
                                          "Näytä yritysten määrän nettomuutos"), 
                                        selected = c("Näytä vuosineljännekset", "Näytä normalisoitu vaihtuvuus (kartta)")),
                     fluidRow(
                       column(12, plotlyOutput("yearlyentryexit1")),
                       column(12, plotOutput("turnoverMap"))
                     )),
                   
                   
                   mainPanel(
                     
                    fluidRow(column(12,plotlyOutput("TotalEntry_TotalDouble"))),
                    fluidRow(column(12,plotlyOutput("TotalExit_PropDouble"))),
                    fluidRow(column(12,plotlyOutput("PropEntry"))),
                    fluidRow(column(12,plotlyOutput("PropExit"))),
                    fluidRow(column(12,plotlyOutput("turnoverIndustry"))),
                    fluidRow(valueBoxOutput("emptyvbox2", width = 8), tags$a(
                      
                      href = "https://stat.fi/tilasto/aly",
                      target = "_blank",
                      valueBox(
                        
                        "",
                        "Lähde: Tilastokeskus, Aloittaneet ja lopettaneet yritykset"
                        
                      ) 
                      
                    )))
                   )
                 ),
               
               ##### SUBSIDY BARS id 33 #####
  
               tabPanel(
                 title="Yritystuet",
                 #value=miscallenious_subsidies_url,
                 
                 sidebarLayout(      
                   sidebarPanel(
                     
                     actionButton("kuvankaappaus5", "Kuvankaappaus"),
                     
                     img(id="tag133", src="yritystag.jpg", style="cursor:pointer;"), 
                     
                     selectInput(inputId = "industrychoice33", label="Valitse toimiala:", choices = TOLcharvec33,
                                 selected = TOLchoicevec33[1], multiple = FALSE, selectize = FALSE),
                     
                     selectInput(
                       inputId = "variablechoice33", label="Valitse muuttuja:", choices = variablechoicevec33,
                       selected = variablechoicevec33[1], multiple = FALSE, selectize = FALSE),
                     
                     checkboxGroupInput(inputId = "graphchoice33", "Muokkaa esitysmuotoa:",
                                        c("Näytä suhteelliset lukumäärät (yritykset)"))),
                   
                   mainPanel(
                     
                     fluidRow(column(12,plotlyOutput("subsidyPlot", height="100%"))),
                     fluidRow(valueBoxOutput("emptyvbox3", width = 8), tags$a(
                       
                       href = "https://stat.fi/tilasto/aly",
                       target = "_blank",
                       valueBox(
                         
                         "",
                         "Lähde: Tilastokeskus, Aloittaneet ja lopettaneet yritykset"
                         
                       ) 
                       
                     )))
                 )
               ), 
               
               ##### REVENUE SERIES id 34 #####
  
               tabPanel(
                 title="Liikevaihtoennakot",
                 #value=miscallenious_revenue_url,
                 sidebarLayout(      
                   sidebarPanel(
                     
                     actionButton("kuvankaappaus6", "Kuvankaappaus"),
                     
                     img(id="tag134", src="yritystag.jpg", style="cursor:pointer;"), 
                     
                     selectInput(inputId = "industrychoice34", label="Valitse toimiala:", choices = TOLchoicevec34,
                                 selected = TOLchoicevec34[1], multiple = FALSE, selectize = FALSE),
                     
                     selectInput(
                       inputId = "variablechoice34", label="Valitse sarja:", choices = variablechoicevec34,
                       selected = variablechoicevec34[1], multiple = FALSE, selectize = FALSE),
                     
                     checkboxGroupInput(inputId = "graphchoice34", "Muokkaa esitysmuotoa:",
                                        c("Näytä indeksin perusvuosi"))),
                   
                   mainPanel(
                     
                     fluidRow(
                       column(12,plotlyOutput("revenuePlot", height="100%"))),
                     fluidRow(valueBoxOutput("emptyvbox4", width = 8), valueBoxOutput("Lähde34", width = 4)))
               )
              ),
    
    ##### PRODUCTIVITY SECTION #####
    
               
               ##### PRODUCTIVITY GRAPHPLOT id 22 #####
  
                 tabPanel(
                   title="Productivity",
                   
                   sidebarLayout(
                     sidebarPanel(
                       
                       actionButton("kuvankaappaus7", "Kuvankaappaus"),
                       
                       img(id="tag521", src="tuottavuustag.jpg", style="cursor:pointer;"), 
                       
                       selectInput("years4", "Valitse vuodet:",
                                   choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                                   selected = "2013", multiple = FALSE, selectize = FALSE),
                       
                       checkboxGroupInput("industries", "Select Industries:",
                                          industrychoicevec,
                                          selected = industrycharvec),
                       
                       actionLink("selectallindustries", label="Select/Deselect all industries"),
                       
                       checkboxGroupInput("regions", "Select Regions:",
                                          regionchoicevec,
                                          selected = regioncharvec),
                       actionLink("selectallregions", label="Select/Deselect all regions"),
                     plotOutput("prodmap")),
                     
                     mainPanel(
                       fluidRow(
                         column(12, plotlyOutput("productivityTurnoverScatter")),
                         column(12, plotlyOutput("productivityBankruptcyScatter"))
                       )
                     )
                   )
                 ),
                 
  
    
    ##### INFO PAGE #####
               tabPanel(
                 title="info",
                 
                 column(12, img(id="etusivulle6", src="etusivulle.jpg", style="cursor:pointer;"), useShinyjs()),
                 includeMarkdown("Shiny-demo-markdown-info.markdown")
                        )),
  
  ##### TAGGED PAGE #####
  
  ##### YRITYKSET #####
  
  navbarMenu(
    title="Tagged",
    
    tabPanel(
      title="#Yritykset",
      
      column(12, img(id="etusivulle1", src="etusivulle.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="investoinnitmuutos1", src="investointimuutos.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="aloittaneetjalopettaneet1", src="aloittaneetjalopettaneet.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="yritystuet1", src="yritystuet.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="liikevaihtoennakot1", src="liikevaihtoennakot.jpg", style="cursor:pointer;"), useShinyjs())),
    
    ##### INVESTOINNIT #####
    
    tabPanel(
      title="#Investoinnit",
      #value=,
      
      column(12, img(id="etusivulle2", src="etusivulle.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="investoinnitmuutos2", src="investointimuutos.jpg", style="cursor:pointer;"), useShinyjs())),
    
    ##### KONKURSSIT #####
    
    tabPanel(
      title="#Konkurssit",
      #value=,
      
      column(12, img(id="etusivulle3", src="etusivulle.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="konkurssit", src="Konkurssit1.jpg", style="cursor:pointer;"), useShinyjs())),
    
    ##### KARTAT #####
    
    tabPanel(
      title="#Kartat",
      #value=,
      
      column(12, img(id="etusivulle4", src="etusivulle.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="konkurssit2", src="Konkurssit1.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="aloittaneetjalopettaneet2", src="aloittaneetjalopettaneet.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="tuottavuuskartta1", src="tuottavuuskartta.jpg", style="cursor:pointer;"), useShinyjs())),
    
    ##### TUOTTAVUUDET #####
    
    tabPanel(
      title="#Tuottavuudet",
      #value=,
      
      column(12, img(id="etusivulle5", src="etusivulle.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="tuottavuussarja1", src="tuottavuussarja.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="tuottavuuspalkit1", src="tuottavuuspalkit.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="tuottavuushistogrammi1", src="tuottavuushistogrammi.jpg", style="cursor:pointer;"), useShinyjs()),
      column(12, img(id="tuottavuuskartta2", src="tuottavuuskartta.jpg", style="cursor:pointer;"), useShinyjs())),
    
      )
)

  



##### SERVER #####

server <- function(input, output, session) {
  
  ##### URL UPDATING #####
  
  observeEvent(input$navbarID, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarID)
    if(is.null(currentHash) || currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 1)
  
  observeEvent(session$clientData$url_hash, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    if(is.null(input$navbarID) || !is.null(currentHash) && currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateNavbarPage(session, "navbarID", selected = currentHash)
    }
  }, priority = 2)
  
  ##### IMAGE LINK BACKEND #####
  
  shinyjs::onclick("image1", updateNavbarPage(session, inputId="navbarID", selected="info"))
  shinyjs::onclick("image2", updateNavbarPage(session, inputId="navbarID", selected="Konkurssit")) 
  shinyjs::onclick("image3", updateNavbarPage(session, inputId="navbarID", selected="Investoinnit, muutos"))
  shinyjs::onclick("image4", updateNavbarPage(session, inputId="navbarID", selected="Productivity Time Series (Graph)"))
  
  shinyjs::onclick("tag131", updateNavbarPage(session, inputId="navbarID", selected="#Yritykset"))
  shinyjs::onclick("tag132", updateNavbarPage(session, inputId="navbarID", selected="#Yritykset"))
  shinyjs::onclick("tag133", updateNavbarPage(session, inputId="navbarID", selected="#Yritykset"))
  shinyjs::onclick("tag134", updateNavbarPage(session, inputId="navbarID", selected="#Yritykset"))
  shinyjs::onclick("tag231", updateNavbarPage(session, inputId="navbarID", selected="#Investoinnit"))
  shinyjs::onclick("tag432", updateNavbarPage(session, inputId="navbarID", selected="#Kartat"))
  
  shinyjs::onclick("tag311", updateNavbarPage(session, inputId="navbarID", selected="#Konkurssit"))
  shinyjs::onclick("tag312", updateNavbarPage(session, inputId="navbarID", selected="#Konkurssit"))
  shinyjs::onclick("tag313", updateNavbarPage(session, inputId="navbarID", selected="#Konkurssit"))
  shinyjs::onclick("tag314", updateNavbarPage(session, inputId="navbarID", selected="#Konkurssit"))
  shinyjs::onclick("tag315", updateNavbarPage(session, inputId="navbarID", selected="#Konkurssit"))
  shinyjs::onclick("tag414", updateNavbarPage(session, inputId="navbarID", selected="#Kartat"))
  
  shinyjs::onclick("tag521", updateNavbarPage(session, inputId="navbarID", selected="#Tuottavuudet"))
  shinyjs::onclick("tag522", updateNavbarPage(session, inputId="navbarID", selected="#Tuottavuudet"))
  shinyjs::onclick("tag523", updateNavbarPage(session, inputId="navbarID", selected="#Tuottavuudet"))
  shinyjs::onclick("tag524", updateNavbarPage(session, inputId="navbarID", selected="#Tuottavuudet"))
  shinyjs::onclick("tag424", updateNavbarPage(session, inputId="navbarID", selected="#Kartat"))
  
  shinyjs::onclick("investoinnitmuutos1", updateNavbarPage(session, inputId="navbarID", selected="Investoinnit, muutos"))
  shinyjs::onclick("aloittaneetjalopettaneet1", updateNavbarPage(session, inputId="navbarID", selected="Aloittaneet ja Lopettaneet Yritykset"))
  shinyjs::onclick("yritystuet1", updateNavbarPage(session, inputId="navbarID", selected="Yritystuet"))
  shinyjs::onclick("liikevaihtoennakot1", updateNavbarPage(session, inputId="navbarID", selected="Liikevaihtoennakot"))
  
  shinyjs::onclick("investoinnitmuutos2", updateNavbarPage(session, inputId="navbarID", selected="Investoinnit, muutos"))
  
  shinyjs::onclick("konkurssikartta1", updateNavbarPage(session, inputId="navbarID", selected="Konkurssit"))
  shinyjs::onclick("aloittaneetjalopettaneet2", updateNavbarPage(session, inputId="navbarID", selected="Aloittaneet ja Lopettaneet Yritykset"))
  shinyjs::onclick("tuottavuuskartta1", updateNavbarPage(session, inputId="navbarID", selected="Productivity Map"))
  
  shinyjs::onclick("konkurssit", updateNavbarPage(session, inputId="navbarID", selected="Bankrupty Time Series (Graph)"))
  
  shinyjs::onclick("tuottavuussarja1", updateNavbarPage(session, inputId="navbarID", selected="Productivity Time Series (Graph)"))
  shinyjs::onclick("tuottavuuspalkit1", updateNavbarPage(session, inputId="navbarID", selected="Productivity Time Series (Bar)"))
  shinyjs::onclick("tuottavuushistogrammi1", updateNavbarPage(session, inputId="navbarID", selected="Productivity Histogram"))
  shinyjs::onclick("tuottavuuskartta2", updateNavbarPage(session, inputId="navbarID", selected="Productivity Map"))
    
  shinyjs::onclick("initialtag1", updateNavbarPage(session, inputId="navbarID", selected="#Yritykset"))
  shinyjs::onclick("initialtag2", updateNavbarPage(session, inputId="navbarID", selected="#Investoinnit"))
  shinyjs::onclick("initialtag3", updateNavbarPage(session, inputId="navbarID", selected="#Konkurssit"))
  shinyjs::onclick("initialtag4", updateNavbarPage(session, inputId="navbarID", selected="#Kartat"))
  shinyjs::onclick("initialtag5", updateNavbarPage(session, inputId="navbarID", selected="#Tuottavuudet"))
  
  shinyjs::onclick("etusivulle1", updateNavbarPage(session, inputId="navbarID", selected="Yritysryhmän etusivu"))
  shinyjs::onclick("etusivulle2", updateNavbarPage(session, inputId="navbarID", selected="Yritysryhmän etusivu"))
  shinyjs::onclick("etusivulle3", updateNavbarPage(session, inputId="navbarID", selected="Yritysryhmän etusivu"))
  shinyjs::onclick("etusivulle4", updateNavbarPage(session, inputId="navbarID", selected="Yritysryhmän etusivu"))
  shinyjs::onclick("etusivulle5", updateNavbarPage(session, inputId="navbarID", selected="Yritysryhmän etusivu"))
  shinyjs::onclick("etusivulle6", updateNavbarPage(session, inputId="navbarID", selected="Yritysryhmän etusivu"))
  
  ##### SCREENSHOT #####
    
  observeEvent(input$kuvankaappaus1 | input$kuvankaappaus2 | input$kuvankaappaus3 | input$kuvankaappaus4 | input$kuvankaappaus5 |
               input$kuvankaappaus6 | input$kuvankaappaus7, {
    
          if (input$kuvankaappaus1 == 0 && input$kuvankaappaus2 == 0 && input$kuvankaappaus3 == 0 && input$kuvankaappaus4 == 0 && input$kuvankaappaus5 == 0 &&
              input$kuvankaappaus6 == 0 && input$kuvankaappaus7 == 0) {
            
            return
            
          } else {
            
            screenshot(
              scale=1,
              filename="Shiny_screenshot")
            
          }   
    
  })
  

  
  ##### SELECT ALL HISTOGRAM ##### 

  
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
  
  data121 <- reactive ({
    
    BRCfin <- months_to_quartals(BRCfin)
    
    BRCfin <- BRCfin[(BRCfin$TOL != "Yhteensä") & (BRCfin$maakunta != "KOKO MAA")& (BRCfin$maakunta != "MA1 MANNER-SUOMI") & (BRCfin$maakunta != "MA2 AHVENANMAA"),]
    
    BRCfin$nobs <- as.numeric(BRCfin$nobs)
    
    BRCfin <- BRCfin[is.element(BRCfin$TOL, input$industries12) & is.element(BRCfin$maakunta, input$regions12) & (BRCfin$time > as.Date(input$dates12[1])) & (BRCfin$time < as.Date(input$dates12[2])),]
    BRCfin <- aggregate(nobs~time, data=BRCfin, FUN=sum)
    
    return(BRCfin)
    
  })
  
  data122 <- reactive ({
    
    BRCfin <- months_to_quartals(BRCfin)
    
    BRCfin <- BRCfin[(BRCfin$maakunta != "KOKO MAA") & (BRCfin$maakunta != "MA1 MANNER-SUOMI") & (BRCfin$maakunta != "MA2 AHVENANMAA"),]
    
    BRCfin$nobs <- as.numeric(BRCfin$nobs)
    
    BRCfin <- BRCfin[is.element(BRCfin$TOL, input$industries12) & is.element(BRCfin$maakunta, input$regions12) & (BRCfin$time > as.Date(input$dates12[1])) & (BRCfin$time < as.Date(input$dates12[2])),]
    
    return(BRCfin)
    
  })
  
  data123 <- reactive ({
    
    BRCfin <- months_to_quartals(BRCfin)
    
    BRCfin <- BRCfin[(BRCfin$TOL != "Yhteensä") & (BRCfin$maakunta != "MA1 MANNER-SUOMI") & (BRCfin$maakunta != "MA2 AHVENANMAA"),]
    
    BRCfin$nobs <- as.numeric(BRCfin$nobs)
    
    BRCfin <- BRCfin[is.element(BRCfin$TOL, input$industries12) & is.element(BRCfin$maakunta, input$regions12) & (BRCfin$time > as.Date(input$dates12[1])) & (BRCfin$time < as.Date(input$dates12[2])),]
    
    return(BRCfin)
    
  })
  
  data124 <- reactive ({
    
    BRCfin <- month_wrangler(BRCfin)
    
    BRCfin <- BRCfin[(BRCfin$maakunta != "KOKO MAA") & (BRCfin$maakunta != "MA1 MANNER-SUOMI") & (BRCfin$maakunta != "MA2 AHVENANMAA"),]
    
    BRCfin$nobs <- as.numeric(BRCfin$nobs)
    
    BRCfin <- BRCfin[is.element(BRCfin$TOL, input$industries12) & is.element(BRCfin$maakunta, input$regions12) & (BRCfin$time > as.Date(input$dates12[1])) & (BRCfin$time < as.Date(input$dates12[2])),]
    
    BRCfin <- aggregate(nobs~time, data=BRCfin, FUN=sum)
    
    return(BRCfin)
    
  })
  
  data125 <- reactive ({
    
    BRCfin <- month_wrangler(BRCfin)
    
    BRCfin <- BRCfin[(BRCfin$TOL != "Yhteensä") & (BRCfin$maakunta != "MA1 MANNER-SUOMI") & (BRCfin$maakunta != "MA2 AHVENANMAA"),]
    
    BRCfin$nobs <- as.numeric(BRCfin$nobs)
    
    BRCfin <- BRCfin[is.element(BRCfin$TOL, input$industries12) & is.element(BRCfin$maakunta, input$regions12) & (BRCfin$time > as.Date(input$dates12[1])) & (BRCfin$time < as.Date(input$dates12[2])),]
    
    BRCfin <- aggregate(nobs~time, data=BRCfin, FUN=sum)
    
    return(BRCfin)
    
  })
  
  data14 <- reactive ({
    
    BRCfin <- month_wrangler(BRCfin)
    
    BRCfin <- join(BRCfin, BRCfinsubs, by = "maakunta")
    
    BRCfin <- BRCfin[(BRCfin$TOL != "Yhteensä") & (BRCfin$maakunta != "KOKO MAA")& (BRCfin$maakunta != "MA1 MANNER-SUOMI") & (BRCfin$maakunta != "MA2 AHVENANMAA"),]
    
    BRCfin$nobs <- as.numeric(BRCfin$nobs)
    
    BRCfin <- BRCfin[is.element(BRCfin$TOL, input$industries12) & is.element(BRCfin$maakunta, input$regions12) & (BRCfin$time > as.Date(input$dates12[1])) & (BRCfin$time < as.Date(input$dates12[2])),]
    
    BRCfin <- aggregate(nobs~maakunta+nutsname, data=BRCfin, FUN=sum)
    
    for (i in 1:length(BRCfinzeros)) {
      
      if (is.element(BRCfinzeros$maakunta[i], BRCfin$maakunta) == FALSE) {
        
        BRCfin <- rbind(BRCfin, BRCfinzeros[i,])
        
      }
      
    }
    
    return(BRCfin)
    
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
    
    data <- data.frame(data, vuosi = as.character(substr(data$vuosineljännes, 1, 4)),
                       neljännes = as.factor(substr(data$vuosineljännes, 5, 6)))

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
    
    entryexitdata2 <- data.frame(vuosineljännes = entryexitdata2$vuosineljännes, luokitus = entryexitdata2$luokitus, aloittaneet = entryexitdata2$aloittaneet, 
                                 lopettaneet = entryexitdata2$lopettaneet, ntotal = entryexitdata2$ntotal)
    
    entryexitdata2 <- entryexitdata2[-c(1),] 
    
    ##### TOL #####
    
    luokitusnimi <- as.character(input$industrychoice32)
    
    data1 <- entryexitdata2[entryexitdata2$luokitus == luokitusnimi,]
    data1 <- data1[is.element(as.character(substr(data1$vuosineljännes, 1, 4)), input$years32),]
    
    data1$aloittaneet <- as.numeric(data1$aloittaneet)
    data1$lopettaneet <- as.numeric(data1$lopettaneet)
    data1$ntotal <- as.numeric(data1$ntotal)
    
    data1$vuosi <- factor(as.character(substr(data1$vuosineljännes, 1, 4)))
    
    return(data1)
    
  })
  
  data321 <- reactive({
    
    turnoverdata <- entryexitdata
    turnoverdata <- turnoverdata[-c(1:2),]
    turnoverdata$kategoria <- zoo::na.locf(turnoverdata$kategoria)
    turnoverdata <- turnoverdata[turnoverdata$luokitus == input$industrychoice32,]
    turnoverdata <- turnoverdata[(! grepl("MA1", turnoverdata$kategoria))&(!grepl("MA2", turnoverdata$kategoria)),]
    turnoverdata$turnoverrate <- (as.numeric(turnoverdata$aloittaneet))/(as.numeric(turnoverdata$ntotal)) + (as.numeric(turnoverdata$lopettaneet))/(as.numeric(turnoverdata$ntotal))
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
    
    handoutdata <- data.frame(num = newcolumn, vuosineljännes=handoutdata$vuosineljännes, TOL = handoutdata$TOL, 
                              kokoluokka = factor(handoutdata$kokoluokka), vuosi = as.numeric(substr(handoutdata$vuosineljännes, 1, 4)),
                              neljännes = as.character(substr(handoutdata$vuosineljännes, 5, 6)))
                              
    
    handoutdata <- handoutdata[handoutdata$TOL == as.character(input$industrychoice33),]
    
  })
  
  data34 <- reactive({
    
    data <- as.data.frame(revenuedata)
    
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
    
    colnames(data) <- gsub("Trendisarja ", "Trendisarja_", colnames(data))
    colnames(data) <- gsub("Alkuperäinen indeksisarja ", "Alkuperänen_indeksisarja_", colnames(data))
    colnames(data) <- gsub("Työpäiväkorjattu indeksisarja ", "Työpäiväkorjattu_indeksisarja_", colnames(data))
    colnames(data) <- gsub("Kausitasoitettu indeksisarja ", "Kausitasoitettu_indeksisarja_", colnames(data))
    
    colnames(data) <- gsub("(F)", "Rakentaminen", colnames(data))
    colnames(data) <- gsub("(B-E)", "Koko_teollisuus", colnames(data))
    colnames(data) <- gsub("(G)", "Koko_kauppa", colnames(data))
    colnames(data) <- gsub("(HIJLMNRS)", "Muut_palvelut", colnames(data))
    
    return(data)
    
  })
  
  data35 <- reactive({
    
    turnoverdata <- entryexitdata
    
    turnoverdata$luokitus[turnoverdata$luokitus == "T Kotitalouksien toiminta työnantajina; kotitalouksien eriyttämätön toiminta tavaroiden ja palvelujen tuottamiseksi omaan käyttöön (97-98)"] <- "T Kotitalouksien toiminta työnantajina (97-98)"
    turnoverdata <- turnoverdata[-c(1:2),]
    turnoverdata$kategoria <- zoo::na.locf(turnoverdata$kategoria)
    turnoverdata <- turnoverdata[(grepl(input$regionchoice32, turnoverdata$kategoria)),]
    turnoverdata$turnoverrate <- (as.numeric(turnoverdata$aloittaneet))/(as.numeric(turnoverdata$ntotal)) + (as.numeric(turnoverdata$lopettaneet))/(as.numeric(turnoverdata$ntotal))
    turnoverindustry <- aggregate(turnoverrate~luokitus, data=turnoverdata, FUN=mean)
    turnoverindustrygroup <- turnoverindustry[is.na(as.numeric(substring(turnoverindustry$luokitus, 1, 2))),]
    
    
  })
  
  data4 <- reactive({
    
    data <- aggregateproddf
    data <- data[as.numeric(data$vuosi) == as.numeric(input$years4),]
    data <- data[,c(1, 2)]
    data <- join(data,BRCfinsubs2,by="maakunta")
    
  })
  
  data41 <- reactive({
    
    data <- entryexitdata[-c(1,2),]
    data$vuosineljännes <- zoo::na.locf(data$vuosineljännes)
    data$kategoria <- zoo::na.locf(data$kategoria)
    data$ntotal <- as.numeric(data$ntotal)
    
    data$vuosi <- as.numeric(substr(data$vuosineljännes, 1, 4))
    data <- data[(data$vuosi==input$years4),]
    
    data <- data[data$luokitus == "Yhteensä",]
    
    data <- data[,c(2, 6, 7)]
    
    data <- aggregate(ntotal~kategoria, data=data, FUN=mean, na.rm=TRUE)
    
    colnames(data) <- c("maakunta", "yrityskanta")
    
    return(data)
    
  })
  
  data42 <- reactive({
    
    data <- BRCfin
    
    data$year <- as.numeric(substr(data$date, 1, 4))
    data <- data[data$TOL=="Yhteensä",]
    data <- data[data$year==input$years4,]
    data <- aggregate(nobs~maakunta, data=data, FUN=sum)
    #data <- data[,c(2, 3, 4)]
    
    return(data)
    
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
      
    output$timeSeries <- renderPlotly({
      
      if (length(input$graphchoice12) == 0) {
        
        h <- timeseries(data121(), c("Konkurssien määrä"), TRUE, "2003-01-01", randcolor())
        
      } else if (length(input$graphchoice12) > 0) {
        
        plot <- ggplot(data121(), aes(x = time, y = nobs)) + geom_bar(stat="identity") + ylab("ylabtxt") + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle("Konkurssien määrä")
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
        plot <- ggplotly(plot)
        
      }
        
      })
  
    output$DecompTimeSeriesRegion <- renderPlotly({
      
      if (length(input$graphchoice12) == 0) {
        
        return(bankruptcygraphs(c("Konkurssit, aikasarja, toimialoittain"), FALSE, "maakunta"))
        
      } else if (length(input$graphchoice12) > 0) {
        
        return(bankruptcygraphs(c("Konkurssit, aikasarja, maakunnittain"), TRUE, "maakunta"))
        
      }
      
    })
    
    output$DecompTimeSeriesIndustry <- renderPlotly ({
      
      
      if (length(input$graphchoice12) == 0) {
        
        return(bankruptcygraphs(c("Konkurssit, aikasarja, maakunnittain"), FALSE, "TOL"))
        
      } else if (length(input$graphchoice12) > 0) {
        
        return(bankruptcygraphs(c("Konkurssit, aikasarja, toimialoittain"), TRUE, "TOL"))
        
      }
      
    })
    
    ##### MAPS #####
      
    output$FinMapPlot <-renderPlot({
      
      finrelevant <- data14()
      
      mapdf <- join(mydata, finrelevant, by = "nutsname")
      
      mapplot <- map(mapdf, "nobs", "Konkurssit maakuntatasolla", "Valitut toimialat, valittu aikaväli", "id", 2, 8)
      
      plot(mapplot)
      
      
    }, height=850, width=650)
    
    output$turnoverMap <- renderPlot({
      
      turnoverregional <- data321()
      
      colnames(turnoverregional) <- c("name", "turnoverrate", "normalized", "nutsname")
      
      #print(mydata)
      
      #print(turnoverregional)
      
      mapdf <- join(mydata, turnoverregional, by = "nutsname")
      
      #print(mapdf)
      
      if ("Näytä normalisoitu vaihtuvuus (kartta)" %in% input$graphchoice32) {
        mapplot <- map(mapdf, "normalized", "Yrityskannan Vaihtuvuus", "Normalisoitu koko maan keskiarvon suhteen", "id", 4, 6)
      } else {
        mapplot <- map(mapdf, "normalized", "Yrityskannan Vaihtuvuus", "", "id", 4, 6)
  
      }
      
      plot(mapplot)
      
    }, height=850, width = 650)
    
    ##### INVESTMENT CHANGE BARS #####
    
    output$InvestmentChange <- renderPlotly({
      
      data <- data31()
      
      colnames(data) <- c("vuosineljännes","vuosimuutos", "vuosi", "neljännes")
      
      data[,2] <- as.numeric(data[,2])
      
      if (length(input$graphchoice31)==0) {
        data <- quartal_wrangler(data)
        
        plot <- timeseries(data[,c(1,2)], "Investointien muutos (vrt. edellisen vuoden vastaavaan kvartaaliin)", FALSE, "2018-01-01", DHcolors)
      } else if (is.element("Aikasarjana", input$graphchoice31)) {
        plot <- ggplot(data, aes(x = neljännes, y = vuosimuutos, fill = vuosi))
        plot <- plot + geom_bar(position = "dodge", stat="identity")
        plot <- plot + ylab("Vuosimuutos, prosenttia") + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=colorsample(5))
        
      }
      
    })
    
    ##### FIRM ENTRY AND EXIT #####
    
    output$TotalEntry_TotalDouble <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32)) 
          & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, FALSE, FALSE, "Aloittaneet yritykset, lkm, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if (is.element("Samassa kuvaajassa", input$graphchoice32)& (is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, FALSE, FALSE, "Aloittaneet ja lopettaneet yritykset, lkm, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, TRUE, FALSE, "Aloittaneet yritykset, lkm, vuosittain", FALSE)
        ggplotly()
      } else if ((is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, TRUE, FALSE, "Aloittaneet ja lopettaneet yritykset, lkm, vuosittain", FALSE)
        ggplotly()
      } else if (is.element("Aikasarjana", input$graphchoice32) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, data32()$aloittaneet, data32()$lopettaneet)), "Aloittaneet ja lopettaneet yritykset, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors)
      } else if (is.element("Aikasarjana", input$graphchoice32) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        yearlyentry <- aggregate(aloittaneet~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, aloittaneet = yearlyentry, lopettaneet = yearlyexit)), "Aloittaneet ja lopettaneet yritykset, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors)
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Aikasarjana", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, FALSE, "Yritysten määrän nettomuutos, lukumäärä", TRUE)
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, (data32()$aloittaneet - data32()$lopettaneet))), "Yritysten määrän nettomuutos, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors)
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        yearlyentry <- aggregate(aloittaneet~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, nettomuutos = (yearlyentry - yearlyexit))), "Yritysten määrän nettomuutos, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors)
      }
    })
    
    output$TotalExit_PropDouble <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
          & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, FALSE, TRUE, "Aloittaneet yritykset, osuus yrityskannasta, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if (is.element("Samassa kuvaajassa", input$graphchoice32)& (is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, FALSE, TRUE, "Aloittaneet ja lopettaneet yritykset, osuus yrityskannasta, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, FALSE, TRUE, TRUE, "Aloittaneet yritykset, osuus yrityskannasta, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if ((is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), TRUE, TRUE, TRUE, TRUE, "Aloittaneet ja lopettaneet yritykset, osuus yrityskannasta, vuosittain", FALSE)
        ggplotly()
      } else if (is.element("Aikasarjana", input$graphchoice32) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, (data32()$aloittaneet/data32()$ntotal), (data32()$lopettaneet/data32()$ntotal))), "Aloittaneet ja lopettaneet yritykset, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors)
      } else if (is.element("Aikasarjana", input$graphchoice32) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        yearlyentry <- aggregate((aloittaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate((lopettaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, aloittaneet = yearlyentry, lopettaneet = yearlyexit)), "Aloittaneet ja lopettaneet yritykset, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors)
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Aikasarjana", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, TRUE, "Yritysten määrän nettomuutos, osuus yrityskannasta", TRUE)
        ggplotly()
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, (data32()$aloittaneet/data32()$ntotal) - (data32()$lopettaneet/data32()$ntotal))), "Yritysten määrän nettomuutos, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors)
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        yearlyentry <- aggregate((aloittaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate((lopettaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, nettomuutos = (yearlyentry - yearlyexit))), "Yritysten määrän nettomuutos, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors)
      }
      
    })
    
    output$PropEntry <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
          & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, FALSE, "Lopettaneet yritykset, lkm, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, TRUE, FALSE, "Lopettaneet yritykset, lkm, vuosittain", FALSE)
        ggplotly()
      } else if (is.element("Samassa kuvaajassa", input$graphchoice32) | (is.element("Aikasarjana", input$graphchoice32))| (is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- industryturnover(data35())
        ggplotly(tooltip=c("x", "fill"))
      } 
      
    })
    
    output$PropExit <- renderPlotly({
      
      if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
          & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, TRUE, "Lopettaneet yritykset, osuus yrityskannasta, vuosineljänneksittäin", FALSE)
        ggplotly()
      } else if ((! is.element("Samassa kuvaajassa", input$graphchoice32)) & (! is.element("Näytä vuosineljännekset", input$graphchoice32)) & (!is.element("Aikasarjana", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, TRUE, TRUE, "Lopettaneet yritykset, osuus yrityskannasta, vuosittain", FALSE)
        ggplotly()
      }
      
    })
    
    output$yearlyentryexit1 <- renderPlotly({
      
        plot <- stockplot(data32(), input$graphchoice32)
        ggplotly()
      
    })
    
    output$turnoverIndustry <- renderPlotly({
      
      if (!is.element("Samassa kuvaajassa", input$graphchoice32) & (!is.element("Aikasarjana", input$graphchoice32))
          & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        
        bar <- industryturnover(data35())
        
        ggplotly(tooltip=c("x", "fill"))
        
      }
      
    })
  
    ##### FIRM SUBSIDY BARS #####
    
    output$subsidyPlot <- renderPlotly({
      
      handoutdata <- data33()
      
      colnames(handoutdata)[1] <- as.character(input$variablechoice33)

      if (grepl("lukumäärä", as.character(input$variablechoice33)) & length(input$graphchoice33)==1) {
       plot <- ggplot(handoutdata, aes(x=vuosineljännes, y = handoutdata[,1], fill=kokoluokka)) + xlab("Vuosineljännes") + ylab("Tukea saaneiden yritysten osuus yrityskannasta") + geom_bar(position = "dodge", stat="identity") + scale_fill_manual(values=colorsample(5))
       plot <- plot + scale_y_continuous(labels = tuhaterotin)
        ggplotly()
        
      } else if (grepl("lukumäärä", as.character(input$variablechoice33)) & length(input$graphchoice33)==0) {
        plot <- ggplot(handoutdata, aes(x=vuosineljännes, y = handoutdata[,1], fill=kokoluokka)) + xlab("Vuosineljännes") + ylab("Yritystä") + geom_bar(position = "dodge", stat="identity") + scale_fill_manual(values=colorsample(5))
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        ggplotly()
        
      }
      
      else {
        plot <- ggplot(handoutdata, aes(x=vuosineljännes, y = handoutdata[,1], fill=kokoluokka)) + xlab("Vuosineljännes") + ylab("Tuhatta euroa") + geom_bar(position = "dodge", stat="identity") + scale_fill_manual(values=colorsample(5))
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        ggplotly()
        
      }

    })
    
    output$productivityTurnoverScatter <- renderPlotly({
      
      data1 <- data4()[, c(1, 3)]
      data2 <- data321()[, c(2, 4)]
      
      colnames(data1) <- c("productivity", "NUTS3")
      colnames(data2) <- c("turnover", "NUTS3")
      
      data <- join(data1, data2, by="NUTS3")
      
      plot <- scatterplot(data, "texts", FALSE, c("turnover", "productivity"))
      
      ggplotly()
      
    })
    
    output$productivityBankruptcyScatter <- renderPlotly({
      
      data4 <- data42()
      
      data3 <- data41()
      
      #data1 <- data14()
      data2 <- data4()
      
      colnames(data4) <- c("maakunta", "konkursseja")
      colnames(data2) <- c("tuottavuus", "maakunta","nutsname")
      
      print(data3)
      print(data4)
      
      print(data2)
      
      data1 <- join(data4, data3, by="maakunta")
      
      data1 <- join(BRCfinsubs, data1, by="maakunta")
      
      data <- join(data1, data2, by="nutsname")
      data$konkurssiosuus <- data$konkursseja/data$yrityskanta
      
      print(data)

      
      data <- data[!duplicated(colnames(data))]
      
      plot <- scatterplot(data, "texts", FALSE, c("konkurssiosuus", "tuottavuus"))
      
      ggplotly()
      
    })
    
    ##### REVENUE SERIES #####
    
    output$revenuePlot <- renderPlotly({
      
      revenuedata <- data34()
      
      if (length(input$graphchoice34) == 1) {
        
        revenueseries <- timeseries(revenuedata, "Yritysten liikevaihtoennakot, sarja", TRUE, "2015-08-01", DHcolors)
        
      } else if (length(input$graphchoice34) == 0) {
        
        revenueseries <- timeseries(revenuedata, "Yritysten liikevaihtoennakot, sarja", FALSE, "2015-08-01", DHcolors)
        
      }
      
      return(revenueseries)
      
    })
    
    ##### FIRM TYPE TREEMAP #####
    
    output$firmTreeMap <- renderHighchart({
      
        hchart(scrapedtable[-c(length(scrapedtable$Yritysmuoto)),],type="treemap",
        hcaes(x=Yritysmuoto, value=X2.1.2023, color=X2.1.2023), name = "Yritykset") %>%
        hc_colorAxis(stops = color_stops(colors = DHcolors))
      
    })
    
    ##### FRONT PAGE: TREEMAP, VALUE BOXES, GOOGLE TRENDS, BANKRUPTCY, INFLATION, PAYROLL AND BUSINESS CYCLE TIME SERIES #####
    
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
      
      #plot <- plot %>% add_trace(type = 'scatter', mode='lines', x=konkurssisarja$time, y=konkursseja, text=konkursseja, hovertemplate = paste('Konkurssien määrä: $%{konkursseja:.2f}'))
      
      #return(plot)
      
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
    
    ##### EMPTY VALUE BOXES FOR FORMATTING #####
    
    output$emptyvbox <- renderValueBox({
      
      valueBox("", "")
      
    })
    
    output$emptyvbox2 <- renderValueBox({
      
      valueBox("", "")
      
    })
    
    output$emptyvbox3 <- renderValueBox({
      
      valueBox("", "")
      
    })
    
    output$emptyvbox4 <- renderValueBox({
      
      valueBox("", "")
      
    })
    
    output$emptyvbox6 <- renderValueBox({
      
      valueBox("", "")
      
    })
    
    output$emptyvbox7 <- renderValueBox({
      
      valueBox("", "")
      
    })
    
    output$prodmap <- renderPlot({
      
      data <- data4()
      
      mapdf <- join(mydata, data, by = "nutsname")
      
      mapplot <- map(mapdf, "prod", "Tuottavuus maakuntatasolla", "", "id", 2, 8)
      
      plot(mapplot)
      
    }, height=850, width=650)
    
    ##### PLOTTING FUNCTIONS #####
    
    exitentryplot <- function(data, entry, double, yearly, proportional, title, net) {
      
      data1 <- data
      
      if (proportional == TRUE) {
        
        data <- data.frame(vuosineljännes = data1$vuosineljännes,
                           aloittaneet = as.numeric(data1$aloittaneet)/as.numeric(data1$ntotal), lopettaneet = as.numeric(data1$lopettaneet)/as.numeric(data1$ntotal),
                           vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                           neljännes = as.character(substr(data1$vuosineljännes, 5, 6)),
                           group = c(rep("aloittaneet", nrow(data1)), rep("lopettaneet", nrow(data1))),
                           aloittaneet_lopettaneet = c(as.numeric(data1$aloittaneet)/as.numeric(data1$ntotal), - as.numeric(data1$lopettaneet)/as.numeric(data1$ntotal)),
                           nettomuutos = (as.numeric(data1$aloittaneet)/as.numeric(data1$ntotal) - as.numeric(data1$lopettaneet)/as.numeric(data1$ntotal)))
        ylabtxt = "Osuus yrityskannasta"
        
      } else if (proportional == FALSE) {
        
        data <- data.frame(vuosineljännes = data1$vuosineljännes, 
                           aloittaneet = as.numeric(data1$aloittaneet), lopettaneet = as.numeric(data1$lopettaneet), total = as.numeric(data1$ntotal),
                           vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                           neljännes = as.character(substr(data1$vuosineljännes, 5, 6)),
                           group = c(rep("aloittaneet", nrow(data1)), rep("lopettaneet", nrow(data1))),
                           aloittaneet_lopettaneet = c(as.numeric(data1$aloittaneet), - as.numeric(data1$lopettaneet)),
                           nettomuutos = (as.numeric(data1$aloittaneet) - as.numeric(data1$lopettaneet)))
        ylabtxt = "Lukumäärä"
      }
      
      if (entry==TRUE & double == FALSE & yearly == FALSE & net == FALSE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = aloittaneet, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (entry==FALSE & double == FALSE & yearly == FALSE & net == FALSE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = lopettaneet, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (double == TRUE & yearly == FALSE & net == FALSE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = aloittaneet_lopettaneet, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (entry==TRUE & double == FALSE & yearly == TRUE & net == FALSE) {
        
        fuel <- data[,c(1:5)]
        fuel <- distinct(fuel, vuosineljännes, .keep_all=TRUE)
        
        yearlyentry <- aggregate(aloittaneet~vuosi, data=fuel, FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=fuel, FUN=sum)
        
        fuel <- data.frame(
          lopettaneet = yearlyexit$lopettaneet, aloittaneet = yearlyentry$aloittaneet,
          vuosi = yearlyentry$vuosi)
        
        plot <- ggplot(fuel, aes(x = vuosi, y = aloittaneet, fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (entry==FALSE & double == FALSE & yearly == TRUE & net == FALSE) {
        
        fuel <- data[,c(1:5)]
        fuel <- distinct(fuel, vuosineljännes, .keep_all=TRUE)
        
        yearlyentry <- aggregate(aloittaneet~vuosi, data=fuel, FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=fuel, FUN=sum)
        
        data <- data.frame(
          lopettaneet = yearlyexit$lopettaneet, aloittaneet = yearlyentry$aloittaneet,
          vuosi = yearlyentry$vuosi)
        
        plot <- ggplot(data, aes(x = vuosi, y = lopettaneet, fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (double == TRUE & yearly == TRUE & net == FALSE) {
        
        fuel <- data[,c(1:5)]
        fuel <- distinct(fuel, vuosineljännes, .keep_all=TRUE)
        
        yearlyentry <- aggregate(aloittaneet~vuosi, data=fuel, FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=fuel, FUN=sum)
        
        data <- data.frame(
          lopettaneet = yearlyexit, aloittaneet = yearlyentry,
          vuosi = yearlyentry$vuosi,
          y = c(as.numeric(yearlyentry$aloittaneet), - as.numeric(yearlyexit$lopettaneet)))
        
        plot <- ggplot(data, aes(x = vuosi, y = aloittaneet_lopettaneet, fill = vuosi, colour = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (yearly == FALSE & net == TRUE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = nettomuutos, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      } else if (yearly == TRUE & net == TRUE) {
        
        plot <- ggplot(data, aes(x = neljännes, y = nettomuutos, fill = vuosi)) + geom_bar(position = "dodge", stat="identity") + ylab(ylabtxt) + xlab("Vuosineljännes")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(title)
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
      }
      
    }
    
    stockplot <- function(data, argument) {
        
      data1 <- data
      
      fuel <- data.frame(yrityskanta <- as.numeric(data1$ntotal), vuosineljännes = data1$vuosineljännes,
                         vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                         neljännes = as.character(substr(data1$vuosineljännes, 5, 6)))
      
      plot <- ggplot(fuel, aes(x = vuosi, y = yrityskanta)) + geom_bar(position = "dodge", stat="identity", show.legend=FALSE) + ylab("yristyskanta") + xlab("Vuosi") #aes(x = vuosi, y = yrityskanta, fill = vuosi)
      plot <- plot + scale_fill_manual(values=rep("black", 10))
      plot <- plot + scale_y_continuous(labels = tuhaterotin)
      
    }
    
    map <- function(data, varname, main, text, id, decimals, fontsize) {
      
      df <- join(mapdata, mydata, by="id")
      
      df <- join(df, coordinates, by="id")
      
      df <- join(df, data, by=id)
      
      for (i in 1:nrow(df)) {
        if (is.na(df$NUTS3[i]) == TRUE) {
          df$nobs[i] <- NA
        }
      }
      
      ##### SIRGU SOOME #####
      
      ##### 'angle' arvutatakse kraadide korrutamisel 100-ga (näiteks 100*30° = 0.3 ) #####
      
      angle <- 0.3
      
      rotmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow=2, ncol=2, byrow=TRUE)
      
      for (i in 1:length(df$lat)) {
        temp <- c(df$lat[i], df$long[i])
        temp <- rotmatrix%*%temp
        df$lat[i] <- temp[1]
        df$long[i] <- temp[2]
      }
      
      for (i in 1:length(df$meanlat)) {
        temp <- c(df$meanlat[i], df$meanlong[i])
        temp <- rotmatrix%*%temp
        df$meanlat[i] <- temp[1]
        df$meanlong[i] <- temp[2]
      }
      
      df <- df[, !duplicated(colnames(df))]
      
      gg <- ggplot() + geom_polygon(data = df, aes(x = long, y = lat, group = group, fill=round(df[,which(colnames(df)==varname)], decimals)), color = "darkgrey", size = 0.5)
      #gg <- maptools::elide(gg, rotate=45)
      gg <- gg + scale_fill_continuous(low = "white", high = DHcolors[8], name = "Productivity estimates", limits=c(min(round(df[,which(colnames(df)==varname)], decimals)),max(round(df[,which(colnames(df)==varname)], decimals))), na.value="transparent",
                                       label = scales::comma)
      gg <- gg + geom_text(data=df, aes(meanlong, meanlat, label=round(df[,which(colnames(df)==varname)], decimals)), size =fontsize)
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
      
      timeseries <- TSstudio::ts_plot(data, color= colorsample(1) ,title=texts, slider=TRUE) 
      
      if (argument & ncol(data)==2) {
        
        timeseries %>% add_segments(y=min(data[,-1]), x= as.POSIXct(baseyear), yend=max(data[,-1]), xend=as.POSIXct(baseyear), text ="Perusvuosi", showlegend=FALSE) |> layout(showlegend=FALSE, colorway=colours[runif(1, 0, length(colours))])
        
      } else if (argument & ncol(data)>2) {
        
        timeseries %>% add_segments(y=min(data[,-1]), x= as.POSIXct(baseyear), yend=max(data[,-1]), xend=as.POSIXct(baseyear), text ="Perusvuosi",  showlegend=TRUE) |> layout(showlegend=TRUE, colorway=colours) #color = "black",
        
      } else {
        
        timeseries |> layout(showlegend=TRUE, colorway=colours)
      }
      
    }
    
    industryturnover <- function(data) {
      
      turnoverindustrygroup <- data
      
      colnames(turnoverindustrygroup) <- c("Toimiala","Vaihtuvuus")
      turnoverindustrygroup$Kirjaintaso <- substr(turnoverindustrygroup$Toimiala, 1, 2)
      
      bar <- ggplot(turnoverindustrygroup, aes(x=Vaihtuvuus, y=Kirjaintaso, fill = Toimiala))+ geom_bar(position = "dodge", stat="identity")
      bar <- bar + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
      bar <- bar + ggtitle("Toimialakohtainen vaihtuvuus maakuntatasolla")
      bar <- bar + theme(legend.title = element_text(size=10),
                         legend.text = element_text(size=8),
                         legend.key.size = unit(0.5, 'cm'),
                         legend.key.height = unit(0.5, 'cm'), 
                         legend.key.width = unit(0.5, 'cm'),
                         axis.title.y = element_blank())
      
      return(bar)
      
    }
    
    bankruptcygraphs <- function(texts, bars, var) {
      
      if (bars == FALSE & var == "TOL") {
        
        decomp <- data.frame(time=unique(data124()$time))
        
        BRCfinmod <- month_wrangler(BRCfin)
        
        for (i in 1:length(input$regions12)) {
          
          decomp <- cbind(decomp, BRCfinmod$nobs[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$time > input$dates12[1] & BRCfinmod$time < input$dates12[2]]) 
          
        }
        
        colnames(decomp) <- c("time", input$regions12)
        
        d <- timeseries(decomp, texts, FALSE, as.character(sys.date()), c(DHcolors, DHcolors, DHcolors))
        
        return(d)
        
      } else if (bars==TRUE & var == "TOL") {
        
        data <- aggregate(nobs~time+TOL, data= data123(), FUN="sum")
        
        plot <- ggplot(data, aes(x = time, y = nobs, fill = TOL)) + geom_bar(stat="identity") + ylab("ylabtxt") + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(texts[1])
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
        plot <- ggplotly(plot)
        
        return(plot)
        
      } else if (bars==FALSE & var == "maakunta") {
        
        decomp <- data.frame(time=unique(data125()$time))
        
        BRCfinmod <- month_wrangler(BRCfin)
        
        for (i in 1:length(input$industries12)) {
          
          decomp <- cbind(decomp, BRCfinmod$nobs[BRCfinmod$TOL == input$industries12[i] & BRCfinmod$maakunta == "KOKO MAA" & BRCfinmod$time > input$dates12[1] & BRCfinmod$time < input$dates12[2]]) 
          
        }
        
        colnames(decomp) <- c("time", input$industries12)
        
        d <- timeseries(decomp, texts, FALSE, as.character(sys.date()), c(DHcolors, DHcolors, DHcolors))
        
      } else if (bars==TRUE & var == "maakunta") {
        
        data <- aggregate(nobs~time+maakunta, data= data122(), FUN="sum")
        
        plot <- ggplot(data, aes(x = time, y = nobs, fill = maakunta)) + geom_bar(stat="identity") + ylab("ylabtxt") + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle(texts[1])
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
        plot <- ggplotly(plot)
        
      } 
      
    }
    
    scatterplot <- function(data, texts, trend, vars) {
      
      plot <- ggplot(data, aes_string(x=vars[1], y=vars[2])) + geom_point(size=2, shape=23)
      plot <- plot + geom_smooth(method=lm,se=TRUE)
      return(plot)
      
    }
    
}


##### RUN THE APP #####

shinyApp(ui = ui, server = server)
