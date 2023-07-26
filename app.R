library(shiny)
library(pkgload)
library(plyr)
library(plotly)
library(shinyjs)
library(ECharts2Shiny)
library(readr)
library(tibble)
library(dplyr)
library(highcharter)
library(shinydashboard)
library(markdown)
library(shinyWidgets)
library(shinyscreenshot)
library(ggplot2)
library(magrittr)
library(tidyr)
library(tidyselect)
library(ggthemes)
library(lubridate)

##### TASKS #####

  ##### TO DO #####

  ##### DONE #####

# Lisää toimialakohtaiset aggregaattituottavuudet (Data jo olemassa)

##### SOURCE AUXILIARY SCRIPTS #####

  source("Shiny-demo-functions.R")

  source("Shiny-demo-auxiliary.R")

##### DEFINE UI #####

ui <- function(request) {
  
  navbarPage(

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
              #column(12, img(id="image1", src="logo1.jpg", style="cursor:pointer;"), useShinyjs()),
              #column(12, img(id="image2", src="konkurssit.jpg", style="cursor:pointer;"), useShinyjs()),
              #column(12, img(id="image3", src="kokeelliset.jpg", style="cursor:pointer;"), useShinyjs()),
              #column(12, img(id="image4", src="tuottavuus.jpg", style="cursor:pointer;"), useShinyjs()),
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
              
              fluidRow(column(12, align = "center", splitLayout(cellWidths = c("50%", "50%"), valueBoxOutput("otsikko01"), textOutput("otsikko02")))),

              fluidRow(column(12, splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("bankruptcySeries"), plotlyOutput("payrollSeries"))))),
            
            fluidRow(
              
              link("https://www.stat.fi/tilasto/kony",
                   "Lähde: Tilastokeskus. 13fb -- Konkurssit kuukausittain vuodesta 1986, 1986M01-2023M04", 6),
              
              link("https://www.stat.fi/tilasto/ktps",
                   "Lähde: Tilastokeskus. 111m -- Palkkasummakuvaajat toimialoittain kuukausitasolla (2015=100), 1995M01-2023M03", 6)
              
            ),
            
            fluidRow(column(12, align = "center", splitLayout(cellWidths = c("50%", "50%"), textOutput("otsikko03"), textOutput("otsikko04")))),
            
            fluidRow(column(12, splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("inflationSeries"), plotlyOutput("cycleSeries")))),
            fluidRow(
            
              link("https://www.stat.fi/tilasto/khi",
                   "Lähde: Tilastokeskus. 11xb -- Kuluttajahintaindeksi (2015=100), kuukausitiedot, 2015M01-2023M04", 6),
              
              link("https://www.stat.fi/tilasto/ktkk",
                   "Lähde: Tilastokeskus. 132f -- Tuotannon suhdannekuvaaja, kuukausittain, 1995M01-2023M03", 6)
            
            ),
            fluidRow(
              column(12, tags$figure(class= "centerFigure", tags$img(
                src="whitespace.png", align="center",
                width=1200,
                alt=" "
              ),tags$figcaption(" ")))#,
              #column(12, plotlyOutput("searchInterest"))),
            #fluidRow(valueBoxOutput("emptyvbox6", width = 8),
              
              #link("https://trends.google.com/trends/explore?date=2017-06-14%202023-06-14&geo=FI&q=konkurssit,lama,yritystuet,ansiosidonnainen&hl=fi",
                   #"Lähde: Google Trends", 12)
          )
        )
      )
    ),

    ##### BANKRUPTCY SECTION #####

               ##### BANKRUPTCY GRAPHPLOT id 12 #####

               tabPanel(
                 title="Konkurssit",
                 #value=bankruptcy_time_series_graph_url,
                 
                 tabsetPanel(
                   tabPanel(title="aikasarja",
                            sidebarLayout(
                              sidebarPanel(
                                
                                actionButton("kuvankaappaus2", "Kuvankaappaus"),
                                
                                img(id="tag311", src="konkurssitag.jpg", style="cursor:pointer;"),
                                
                                dateRangeInput01("dates12", "2005-01-01"),
                                
                                valueBoxOutput("IndustryChoiceTextBox11", width = 12),
                                
                                actionLink("selectallindustries12", label="Valitse kaikki/Poista valinnat"),
                                
                                checkboxGroupInput("industries12", "", # Valitse toimialat:
                                                   industrychoicevec,
                                                   selected = industrychoicevec),
                                
                                valueBoxOutput("RegionChoiceTextBox11", width = 12),
                                
                                actionLink("selectallregions12", label="Valitse kaikki/Poista valinnat"),
                                
                                checkboxGroupInput("regions12", "", # Valitse maakunnat
                                                   regionchoicevec,
                                                   selected = regionchoicevec),

                                checkboxGroupInput("graphchoice12", "Muokkaa Esitystapaa:", c("Palkit", "Liukuva keskiarvo",
                                                                                              "Indeksöity"), selected=NULL),
                                radioButtons("muuttuja12", "Valitse tarkasteltava muuttuja (palkit ja kartta):",
                                             c("työntekijöitä", "yrityksiä"), selected =  "yrityksiä"),
                                checkboxGroupInput("muuttuja121", "Valitse tarkasteltavat muuttuja (aikasarjat):",
                                                   c("työntekijöitä", "yrityksiä"), selected =  "yrityksiä"),
                                plotOutput("FinMapPlot")),
                              
                              mainPanel(
                                fluidRow(
                                  column(12,align = "center", textOutput("otsikko111")),
                                  column(12,plotlyOutput("konkurssiAikasarja")),
                                  column(12,align = "center", textOutput("otsikko112")),
                                  column(12,plotlyOutput("DecompTimeSeriesRegion")),
                                  column(12,align = "center", textOutput("otsikko113")),
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
                            )),
                 
                 tabPanel(title="vertaile vuosia", sidebarLayout(
                   
                   sidebarPanel(
                     
                     valueBoxOutput("YearChoiceTextBox12", width = 12),
                     
                     actionLink("selectallyears12", label="Valitse kaikki/Poista valinnat"),
                     
                     checkboxGroupInput(inputId = "vuodet12", label="", # Valitse vuodet
                                  choices = c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                              "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
                                              "2019", "2020", "2021", "2022", "2023"), 
                                  selected = c("2019", "2020", "2021", "2022", "2023")),
                     radioButtons("toimialat12", "Valitse toimiala:",
                                  c(industrychoicevec, "Yhteensä"),
                                  selected = "Yhteensä"),
                     checkboxGroupInput(inputId = "graphchoice122", label = "Muokkaa esitysmuotoa:",
                                        choices = c("kumulatiivinen summa", "logaritmina"),
                                        selected = c("kumulatiivinen summa", "logaritmina")),
                     
                     radioButtons(inputId = "variablechoice12", label = "Valitse muuttujat",
                                  choices = c("työntekijöitä", "yrityksiä"), selected = "työntekijöitä")
                       
                     ),
                   
                   mainPanel(fluidRow(
                     column(12,align = "center", textOutput("otsikko12")),
                     column(12, plotlyOutput("konkurssiVuodet"))
                   ))
                   
                   
                 )),
                 
                 tabPanel(title="konkurssien jakauma kokoluokittain", sidebarLayout(
                   
                   sidebarPanel(
                     
                     radioButtons("toimialat122", "Valitse toimiala:",
                                  toimialavektori122,
                                  selected = "toimialat yhteensä")
                     ),
                   
                   mainPanel(
                     
                     fluidRow(
                      column(12, splitLayout(cellWidths = c("60%", "40%"), plotOutput("konkurssijakaumaBarplot"), includeMarkdown("Shiny-demo-markdown-teksti51.md"))),
                      column(12, splitLayout(cellWidths = c("50%", "50%"), valueBoxOutput("konkurssijakaumaVbox_pienet"), valueBoxOutput("konkurssijakaumaVbox_suuret")))                      
                 )))
                   
                   
                 ))),


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

                     valueBoxOutput("QuartalChoiceTextBox2", width = 12),
                     
                     actionLink("selectallquartals31", label="Valitse kaikki/Poista valinnat"),
                     
                     checkboxGroupInput("quartals31", "", # Valitse vuosineljännekset:
                                        quartalchoicevec,
                                        selected=quartalcharvec),

                     checkboxGroupInput("graphchoice31", "Valitse esitysmuoto:",
                     "Palkit, vuosineljänneksittäin", selected=NULL)),


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
                     
                     valueBoxOutput("QuartalChoiceTextBox3", width = 12),
                     
                     actionLink("selectallyears32", label="Valitse kaikki/Poista valinnat"),

                     checkboxGroupInput("years32", " ", #"Valitse vuodet:"
                                        yearchoicevec,
                                        selected=yearcharvec),

                     checkboxGroupInput(inputId = "graphchoice32", "Muokkaa esitysmuotoa:",
                                        c("Samassa kuvaajassa", "Näytä vuosineljännekset", "Näytä normalisoitu vaihtuvuus (kartta)", "Aikasarjana",
                                          "Näytä yritysten määrän nettomuutos"),
                                        selected = c("Näytä vuosineljännekset", "Näytä normalisoitu vaihtuvuus (kartta)")),
                     fluidRow(
                       column(12, plotlyOutput("yearlyentryexit1")),
                       column(12, includeMarkdown("Shiny-demo-markdown-teksti3.md")),
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
                                        c("Näytä osuus yrityskannasta (tukea saaneiden yritysten lukumäärä)",
                                          "Näytä osuus toimialalle tietyssä vuosineljänneksessä maksetuista tuista"),
                                        selected = NULL),
                     
                     includeMarkdown("yritystuet_sidebar_teksti.md")),

                   mainPanel(
                     
                     

                     fluidRow(
                       column(12,align = "center", textOutput("otsikko4")),
                       column(12,plotlyOutput("subsidyPlot", height="100%"))),
                     
                     fluidRow(valueBoxOutput("emptyvbox3", width = 8), tags$a(

                       href = "https://stat.fi/tilasto/yrtt",
                       target = "_blank",
                       valueBox(

                         "",
                         "Lähde: Tilastokeskus, Yritystukitilasto"

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
                     
                     dateRangeInput01("dates34", "2010-01-01"),

                     selectInput(inputId = "industrychoice34", label="Valitse toimiala:", choices = TOLchoicevec34,
                                 selected = TOLchoicevec34[1], multiple = FALSE, selectize = FALSE),

                     selectInput(
                       inputId = "variablechoice34", label="Valitse sarja:", choices = variablechoicevec34,
                       selected = variablechoicevec34[1], multiple = FALSE, selectize = FALSE),

                     checkboxGroupInput(inputId = "graphchoice34", "Muokkaa esitysmuotoa:",
                                        c("Näytä indeksin perusvuosi"))),

                   mainPanel(

                     fluidRow(
                       column(12,align = "center", textOutput("otsikko5")),
                       column(12,plotlyOutput("revenuePlot", height="100%"))),
                     fluidRow(valueBoxOutput("emptyvbox4", width = 8), valueBoxOutput("Lähde34", width = 4)))
               )
              ),

    ##### PRODUCTIVITY SECTION #####


               ##### PRODUCTIVITY GRAPHPLOT id 22 #####

                 tabPanel(
                   title="Tuottavuus",

                   sidebarLayout(
                     sidebarPanel(

                       actionButton("kuvankaappaus7", "Kuvankaappaus"),

                       img(id="tag521", src="tuottavuustag.jpg", style="cursor:pointer;"),

                       selectInput("years4", "Valitse vuosi:",
                                   choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                                   selected = "2013", multiple = FALSE, selectize = FALSE),
                      
                      radioButtons("toimialat4", "Valitse toimiala:",
                                        toimialat4, selected = "Teollisuus"),
                      
                       checkboxGroupInput("graphchoice22", "Muokkaa esitysmuotoa:", c("Trendi", "Havaintojen lukumäärällä painotettu trendi"), selected = NULL),
                      radioButtons("graphchoice221", "Tarkastele tuottavuuksia", c("maakunnittain", "toimialoittain"), 
                                   selected = "maakunnittain"),
                      includeMarkdown("Shiny-demo-markdown-teksti6.md"),
                     plotOutput("prodmap")),

                     mainPanel(
                       fluidRow(
                         column(12,align = "center", textOutput("otsikko61")),
                         column(12, plotlyOutput("productivityTurnoverScatter")),
                         column(12,align = "center", textOutput("otsikko62")),
                         column(12, plotlyOutput("productivityBankruptcyScatter")),
                         column(12,align = "center", textOutput("otsikko63")),
                         column(12, splitLayout(cellWidths = c("50%", "50%"), 
                                                plotOutput("konkurssiBoxplotit"), includeMarkdown("Shiny-demo-markdown-teksti22.md")))
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

      ),

#tags$head(tags$style("#otsikko01{color: black; 
#                     font-size: 20px; 
#                     font-style: bold;}")),
tags$head(tags$style("#otsikko02{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko03{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko04{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko111{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),					
tags$head(tags$style("#otsikko112{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko113{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko12{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko4{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko5color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko61{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko62{color: black; 
                     font-size: 20px; 
                     font-style: bold;}")),
tags$head(tags$style("#otsikko63{color: black; 
                     font-size: 20px; 
                     font-style: bold;}"))
)
}





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

  shinyjs::onclick("konkurssit", updateNavbarPage(session, inputId="navbarID", selected="Konkurssit"))

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

  ##### SELECT ALL YEARS id 12 #####

  observe({
    
    if(input$selectallyears12 == 0) {return(NULL)}
    else if (input$selectallyears12%%2 == 0){
      updateCheckboxGroupInput(session,"vuodet12", "",choices=c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                                               "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
                                                               "2019", "2020", "2021", "2022", "2023"))
    } else {
      updateCheckboxGroupInput(session,"vuodet12", "",choices=c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                                                "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
                                                                "2019", "2020", "2021", "2022", "2023"),
                                                   selected = c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                                                "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
                                                                "2019", "2020", "2021", "2022", "2023"))
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
      updateCheckboxGroupInput(session,"years32", " ",choices=yearchoicevec) #"Valitse vuosineljännekset:"
    }
    else  {
      updateCheckboxGroupInput(session,"years32", " ",choices=yearchoicevec,selected=yearchoicevec) #"Valitse vuosineljännekset:"
    }
  })

  ##### REACTIVE DATA SELECTION #####

  data01 <- reactive({

    interestdata <- gtrendsR::gtrends(keyword = keywords, geo="FI", time="2017-05-24 2023-05-24", gprop="web")

    relevantinterestdata <- interestdata$interest_over_time[,c(1:3)]

    relevantinterestdata <- data.frame(date = as.POSIXct(relevantinterestdata$date[relevantinterestdata$keyword == keywords[1]]), lama = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "lama"]), konkurssit = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "konkurssit"]),
                                       yritystuet = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "yritystuet"]), ansiosidonnainen = as.numeric(relevantinterestdata$hits[relevantinterestdata$keyword == "ansiosidonnainen"]))

  })
  
  data120 <- reactive ({
    
    data <- BRCfin
    
    #data %<>% as.data.frame()
    
    #data %<>% month_wrangler()
    
    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()
    
    colnames(data) <- c("vuosi", "TOL", "alue", "yrityksiä", "työntekijöitä")
    
    data <- data %>% select(c("vuosi", "TOL", "alue", input$variablechoice12))
    
    data <- data[!(data$alue == "MK01 Uusimaa" & data$alue == "MK21 Ahvenanmaa"),]
    
    years <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
               "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
               "2019", "2020", "2021", "2022", "2023") #input$vuodet12
    
    TOL <- input$toimialat12
    
    df <- data[substr(as.character(data$vuosi), 1, 4) == years[1] & data$alue == "KOKO MAA" & data$TOL == TOL,]
    
    for (i in 1:length(years)) {
      
      if (nrow(data[substr(as.character(data$vuosi), 1, 4) == years[i] & data$alue == "KOKO MAA" & data$TOL == TOL,]) < 12) {
        
        temp <- data[substr(as.character(data$vuosi), 1, 4) == years[i] & data$alue == "KOKO MAA" & data$TOL == TOL,]

        temp <- temp[,4]

        #temp %<>% unlist()
        
        temp <- c(temp, c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
        
        temp <- temp[1:12]
        
        if (is.element("kumulatiivinen summa", input$graphchoice122)) {
          
          temp %<>% cumsum()
          
        }
        
        if (is.element("logaritmina", input$graphchoice122)) {
          
          temp %<>% log()
          
        }
        
      } else {
        
        temp <- data[substr(as.character(data$vuosi), 1, 4) == years[i] & data$alue == "KOKO MAA" & data$TOL == TOL,]
        
        temp <- temp[,4]
        
        #temp %<>% unlist()
        
        #temp <- data[substr(as.character(data$vuosi), 1, 4) == years[i] & data$alue == "KOKO MAA" & data$TOL == TOL,]$n
        
        if (is.element("kumulatiivinen summa", input$graphchoice122)) {
          
          temp %<>% cumsum()
          
        }
        
        if (is.element("logaritmina", input$graphchoice122)) {
          
          temp %<>% log()
          
        }
      
      
      }
      
      df <- cbind(df, temp)
      
    }
    
    colnames(df) <- c(c("vuosi", "TOL", "maakunta", input$variablechoice12), years)
    
    df <- df[, names(df)[names(df) %in% c(input$vuodet12, "vuosi")]]
    
    df$vuosi <- substr(df$vuosi, 6, 10)
    df$vuosi <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    
    #df$vuosi <- format(df$vuosi, "%m-%d")
    #df$vuosi <- c("tammikuu", "helmikuu", "maaliskuu", "huhtikuu", "toukokuu", "kesäkuu", "heinäkuu", "elokuu", "syyskuu", "lokakuu", "marraskuu", "joulukuu")
    
    print(df)
    
    return(df)
    
  })

  data121 <- reactive ({
    
    data <- BRCfin

    data <- data[(data$TOL != "Yhteensä") & (data$maakunta != "KOKO MAA")& (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]

    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()

    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.POSIXct(input$dates12[1])) & (data$date < as.POSIXct(input$dates12[2])),]
    
    data_yrityksiä <- aggregate(yrityksiä~date, data=data, FUN=sum)
    data_työntekijöitä <- aggregate(työntekijöitä~date, data=data, FUN=sum)
    
    data <- join(data_yrityksiä, data_työntekijöitä, by = "date")
    
    if (length(input$muuttuja121) == 1) {
      
      data <- aggregate(formula(paste0(input$muuttuja121, "~date")), data = data, FUN = "sum")
      
    } else if (is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
      
      data_yrityksiä <- aggregate(yrityksiä~date, data=data, FUN=sum)
      data_työntekijöitä <- aggregate(työntekijöitä~date, data=data, FUN=sum)
      
      data <- join(data_yrityksiä, data_työntekijöitä, by = "date")
      
    }

    return(data)

  })

  data122 <- reactive ({
    
    data <- BRCfin

    data <- data[(data$maakunta != "KOKO MAA") & (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]

    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()

    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]
    
    return(data)

  })

  data123 <- reactive ({

    data <- BRCfin

    data <- data[(data$TOL != "Yhteensä") & (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]

    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()

    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]
  
    return(data)

  })

  data124 <- reactive ({

    data <- BRCfin
    
    #data %<>% month_wrangler()

    data <- data[(data$maakunta != "KOKO MAA") & (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]

    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()

    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]
      
    data_yrityksiä <- aggregate(yrityksiä~date, data=data, FUN="sum")
    data_työntekijöitä <- aggregate(työntekijöitä~date, data=data, FUN="sum")
    
    data <- join(data_yrityksiä, data_työntekijöitä, by = "date")

    return(data)

  })

  data125 <- reactive ({

    data <- BRCfin

    data <- data[(data$TOL != "Yhteensä") & (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]

    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()

    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]

    data_yrityksiä <- aggregate(yrityksiä~date, data=data, FUN="sum")
    data_työntekijöitä <- aggregate(työntekijöitä~date, data=data, FUN="sum")
    
    data <- join(data_yrityksiä, data_työntekijöitä, by = "date")

    return(data)

  })
  
  data126 <- reactive({
    
    data <- BRCfin
    data <- data[(data$maakunta != "KOKO MAA") & (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]
    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]
    
    decomp <- data.frame(time=unique(data$date))
    
    BRCfinmod <- BRCfin
    
    #print("BRCfinmod from data126")
    
    #print(BRCfinmod)
    
    if (!is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
      
      for (i in 1:length(input$regions12)) {
      
        decomp <- cbind(decomp, as.numeric(BRCfinmod$yrityksiä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]))
        
      }
    
    } else if (is.element("työntekijöitä", input$muuttuja121) & !is.element("yrityksiä", input$muuttuja121)) {
      
      for (i in 1:length(input$regions12)) {
      
        decomp <- cbind(decomp, as.numeric(BRCfinmod$työntekijöitä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]))
        
      }
    
    } else if (is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
      
      for (i in 1:length(input$regions12)) {
        
        decomp <- cbind(decomp, as.numeric(BRCfinmod$yrityksiä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]),
                        as.numeric(BRCfinmod$työntekijöitä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]))
      
      }
    
    }
    
    return(decomp)
  
  })
    
    data127 <- reactive({
      
      data <- BRCfin
      data <- data[(data$maakunta != "KOKO MAA") & (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]
      data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]
      
      decomp <- data.frame(time=unique(data$date))
      
      BRCfinmod <- BRCfin
      
      #print("BRCfinmod from data126")
      
      #print(BRCfinmod)
      
      if (!is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
        
        for (i in 1:length(input$industries12)) {
          
          decomp <- cbind(decomp, as.numeric(BRCfinmod$yrityksiä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]))
          
        }
        
      } else if (is.element("työntekijöitä", input$muuttuja121) & !is.element("yrityksiä", input$muuttuja121)) {
        
        for (i in 1:length(input$industries12)) {
          
          decomp <- cbind(decomp, as.numeric(BRCfinmod$työntekijöitä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]))
          
        }
        
      } else if (is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
        
        for (i in 1:length(input$industries12)) {
          
          decomp <- cbind(decomp, as.numeric(BRCfinmod$yrityksiä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]),
                          as.numeric(BRCfinmod$työntekijöitä[BRCfinmod$maakunta == input$regions12[i] & BRCfinmod$TOL == "Yhteensä" & BRCfinmod$date > input$dates12[1] & BRCfinmod$date < input$dates12[2]]))
          
        }
        
      }
      
      return(decomp)
    
  })

  data14 <- reactive ({

    data <- BRCfin

    data <- join(data, BRCfinsubs, by = "maakunta")
    
    #print(BRCfin)
    
    #print(BRCfinsubs)

    data <- data[(data$TOL != "Yhteensä") & (data$maakunta != "KOKO MAA")& (data$maakunta != "MA1 MANNER-SUOMI") & (data$maakunta != "MA2 AHVENANMAA"),]

    data$yrityksiä %<>% as.numeric()
    data$työntekijöitä %<>% as.numeric()

    data <- data[is.element(data$TOL, input$industries12) & is.element(data$maakunta, input$regions12) & (data$date > as.Date(input$dates12[1])) & (data$date < as.Date(input$dates12[2])),]

    #data_yrityksiä <- aggregate(yrityksiä~date+maakunta, data=data, FUN="sum")
    #data_työntekijöitä <- aggregate(työntekijöitä~date+maakunta, data=data, FUN="sum")
    
    #data <- join(data_yrityksiä, data_työntekijöitä, by = "date")

    for (i in 1:length(BRCfinzeros)) {

      if (is.element(BRCfinzeros$maakunta[i], data$maakunta) == FALSE) {

        data <- rbind(data, BRCfinzeros[i,])

      }

    }
    
    data <- aggregate(formula(paste0(input$muuttuja12,"~maakunta+nutsname")), data = data, FUN = "sum")
    
    return(data)

  })

  data31 <- reactive({
    
    suppressMessages(
      
      investmentchangedata <- readxl::read_excel("data/investoinnitMuutokset.xlsx")
      
    )

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
    #turnoverdata <- turnoverdata[-c(1:2),]
    #turnoverdata$kategoria <- zoo::na.locf(turnoverdata$kategoria)
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
    
    suppressMessages(
      
      handoutdata <- readxl::read_excel('data/yritystuet.xlsx')
      
    )
    
    handoutdata <- as.data.frame(handoutdata)
    
    cnamevec <- c("vuosineljännes","TOL","kokoluokka",variablechoicevec33)
    
    handoutdata <- handoutdata[-c(1:2),]
    handoutdata <- handoutdata[-c(1021:1078),]
    
    colnames(handoutdata) <- cnamevec
    
    quartallen <- length(which(is.na(handoutdata$vuosineljännes) == FALSE))
    TOLlen <- length(which(is.na(handoutdata$TOL) == FALSE))
    
    quartalgap <- 85
    TOLgap <- 5
    
    handoutdata$vuosineljännes <- zoo::na.locf(handoutdata$vuosineljännes, option="locf")
    handoutdata$TOL <- zoo::na.locf(handoutdata$TOL, option="locf")
    
    handoutdata[handoutdata == "."] <- as.character(0)
    
    handoutdata <- handoutdata[handoutdata$kokoluokka != "Kaikki yritykset",]
    handoutdata <- handoutdata[handoutdata$TOL != "Muut toimialat" & handoutdata$TOL != "Toimiala tuntematon" &
                                 handoutdata$TOL != "Ei yritystunnusta",]
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

  data34 <- reactive({ # Revenue data / Liikevaihtoennakot
    
    suppressMessages(
      
      revenuedata <- readxl::read_excel("data/liikevaihtoennakot.xlsx")
      
    )
    
    vec34 <- c("(B-E)", "(F)", "(G)", "(HIJLMNRS)")
    
    namerow <- revenuedata[3,]
    
    for (i in 1:16) {
      namerow[1+i] <- paste(namerow[1+i], vec34[ceiling(i/4)])
    }
    
    namerow[1] <- "Kuukausi"
    
    revenuedata <- revenuedata[-c(1:3),]
    revenuedata <- revenuedata[-c(160:199),]
    colnames(revenuedata) <- namerow
    
    revenuedata$Kuukausi <- gsub('M', '', revenuedata$Kuukausi)
    
    revenuedata$Kuukausi <- as.POSIXct(paste0(as.character(revenuedata$Kuukausi), '01'), format='%Y%m%d')
    
    for (i in 2:length(revenuedata[1,])) {
      revenuedata[,i] <- as.numeric(unlist(revenuedata[,i]))
    }

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

    data <- data[data$Kuukausi > input$dates34[1] & data$Kuukausi < input$dates34[2],]

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
    #data <- data[,c(1, 2)]
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
    
    data$yrityksiä %<>% as.numeric()

    data$year <- as.numeric(substr(data$date, 1, 4))
 
    data$TOL[data$TOL == "Maa-, metsä ja kalatalous"] <- "Maatalous, metsätalous ja kalatalous"
    data$TOL[data$TOL == "Rakennustoiminta"] <- "Rakentaminen"
    data$TOL[data$TOL == "Muut palvelut"] <- "Muu palvelutoiminta"
    data$TOL[data$TOL == "Kauppa"] <- "Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus"
    data$TOL[data$TOL == "Teollisuus, kaivostoiminta sekä energia- ja vesihuolto"] <- "Teollisuus"
    
    data <- data[data$TOL == input$toimialat4 & data$year == input$years4,]

    data <- aggregate(yrityksiä~maakunta, data=data, FUN=sum)

    return(data)

  })

  data43 <- reactive({
    
    weightedmean_toimialat <- read.csv("data/12072023_weightedmean_toimialat.csv")
    data <- weightedmean_toimialat[weightedmean_toimialat$vuosi == input$years4,]
    
  })
  
  data44 <- reactive({
    
    weightedmean_maakunnat <- read.csv("data/12072023_weightedmean_maakunnat.csv")
    data <- weightedmean_maakunnat[weightedmean_maakunnat$vuosi == input$years4,]
    
  })
  
  data45 <- reactive({
    
    data <- BRCfin
    
    data$yrityksiä %<>% as.numeric()
    
    data <- data[substr(as.character(data$date), 6, 7) == "12",]
    
    data$year <- as.numeric(substr(data$date, 1, 4))
    
    data$TOL[data$TOL == "Maa-, metsä ja kalatalous"] <- "Maatalous, metsätalous ja kalatalous"
    data$TOL[data$TOL == "Rakennustoiminta"] <- "Rakentaminen"
    data$TOL[data$TOL == "Muut palvelut"] <- "Muu palvelutoiminta"
    data$TOL[data$TOL == "Kauppa"] <- "Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus"
    data$TOL[data$TOL == "Teollisuus, kaivostoiminta sekä energia- ja vesihuolto"] <- "Teollisuus"

    data <- data[data$year == input$years4 & data$maakunta == "KOKO MAA",]
    
    #print("data45")
    #print(data)
    
    return(data[,c(2, 3, 4, 6)])
    
  })
  
  data46 <- reactive({
    
    tuottavuus_toimialat <- read.csv("data/22062023_tuottavuus_toimialat.csv")
    tuottavuus_toimialat <- tuottavuus_toimialat[,-c(1)]
    tuottavuus_toimialat$toimiala <- iconv(tuottavuus_toimialat$toimiala, from = "ISO-8859-1", to = "UTF-8")
    tuottavuus_toimialat$maakunta <- iconv(tuottavuus_toimialat$maakunta, from = "ISO-8859-1", to = "UTF-8")
    tuottavuus_toimialat <- tuottavuus_toimialat[tuottavuus_toimialat$toimiala == input$toimialat4 
                                                 & tuottavuus_toimialat$vuosi == input$years4,]
    return(tuottavuus_toimialat)
    
  })
  
  data47 <- reactive({
    
    yrityskantadata <- readxl::read_excel("data/aloittaneetLopettaneet.xlsx")
    colnames(yrityskantadata) <- c("vuosi", "maakunta", "toimiala", "aloittaneet", "lopettaneet", "yrityskanta")
    yrityskantadata <- yrityskantadata[-c(1, 2),]
    yrityskantadata$vuosi %<>% zoo::na.locf()
    yrityskantadata$maakunta %<>% zoo::na.locf()
    yrityskantadata %<>% na.omit()
    yrityskantadata <- yrityskantadata[yrityskantadata$maakunta != "KOKO MAA" & yrityskantadata$maakunta != "MA1 MANNER-SUOMI" 
                                       & yrityskantadata$maakunta != "MA2 AHVENANMAA" & is.na(as.numeric(substr(yrityskantadata$toimiala, 1, 2)))
                                       & substr(yrityskantadata$vuosi, 5, 6) == "Q4",]

    yrityskantadata$maakunta <- substr(yrityskantadata$maakunta, 6, length(yrityskantadata$maakunta))
    yrityskantadata$vuosi <- substr(yrityskantadata$vuosi, 1, 4)

    for (i in 1:nrow(yrityskantadata)) {
      
      if (is.element(substr(yrityskantadata$toimiala[i], 1, 1), c("L", "O", "P", "D", "U"))) {
        yrityskantadata$toimiala[i] <- substring(yrityskantadata$toimiala[i], 1, nchar(yrityskantadata$toimiala[i])-5)
      } else {
        yrityskantadata$toimiala[i] <- substring(yrityskantadata$toimiala[i], 1, nchar(yrityskantadata$toimiala[i])-8)
      }
      
    }
    
    yrityskantadata$toimiala <- substring(yrityskantadata$toimiala, 3)
    
    yrityskantadata <- yrityskantadata[,-c(4, 5)]
    
    yrityskantadata$vuosi %<>% as.numeric()
    yrityskantadata$yrityskanta %<>% as.numeric()
    
    yrityskantadata$toimiala[yrityskantadata$toimiala == ""] <- "Yhteensä"
    
    #print("Yrityskantadata:")
    #print(yrityskantadata)
    return(yrityskantadata)
    
  })
  
  data51 <- reactive({
    
    konkurssijakauma <- read.csv("data/14072023_konkurssijakauma_toimialat.csv")
    konkurssijakauma$toimiala <- iconv(konkurssijakauma$toimiala, from = "ISO-8859-1", to = "UTF-8")
    
    #konkurssijakauma <- read.csv("data/19072023_konkurssit_pylväät_86_22.csv")
    #konkurssijakauma$toimiala <- iconv(konkurssijakauma$toimiala, from = "ISO-8859-1", to = "UTF-8")
    
    data <- konkurssijakauma[konkurssijakauma$toimiala == input$toimialat122 ,]
    
  })
  
  data6 <- reactive({
    
    tuottavuusjakauma <- read.csv("data/13072023_toimialat_boxplotit.csv")
    tuottavuusjakauma$toimiala <- iconv(tuottavuusjakauma$toimiala, from = "ISO-8859-1", to = "UTF-8")
    
    data <- tuottavuusjakauma[tuottavuusjakauma$toimiala == input$toimialat4 & tuottavuusjakauma$vuosi == input$years4,] #& data$yritysluokka == "mikroyritys",
    
    
  })

  ##### DRAW PLOTS #####

    ##### BANKRUPTCY GRAPHPLOTS #####

    output$konkurssiAikasarja <- renderPlotly({

      if (!is.element("Palkit", input$graphchoice12)) {
        
        yvar <- "lukumäärä"

        data <- data121()
        
        if (is.element("Liukuva keskiarvo", input$graphchoice12)) {
          
          if (ncol(data) > 2) {
            
            data[,-c(1)] <- sapply(data[,-c(1)], function(x) manual_rollmean(x, 12))
            
          } else {
            
            tempdata <- data[,2]
            
            appendvec <- rep(tempdata[1], 12)
            temp <- c(appendvec, tempdata)
            
            for (i in 12:length(temp)) {
              
              temp[i] <- mean(temp[(i-12):i])
              
            }
            temp <- temp[c((12+1):length(temp))]
            data[,2] <- temp
            
          }
          
        }
        
        if (is.element("Indeksöity", input$graphchoice12)) {
          
          yvar <- "indeksi"
          
          if (ncol(data) > 2) {
            
            data[,-c(1)] <- sapply(data[,-c(1)], function(x) indeksöi(x))
            
          } else {
            
            #data[2] %<>% indeksöi()
            data[2] <- (data[2]/data[1,2])*100
            
          }
          
        }

        plot <- timeseries(data, c("Konkurssien määrä"), FALSE, "2003-01-01", randcolor(), "muuttuja", "lukumäärä", "Päivämäärä")

      } else if (is.element("Palkit", input$graphchoice12)) {
        
        yvar <- "lukumäärä"
        
        data <- data121()
        
        #print(data)
        
        dfm <- reshape2::melt(data[,colnames(data)],id.vars = 1)
        
        #print(dfm)
        
        plot <- ggplot(dfm, aes_string(x = colnames(data)[1], y = "value")) + geom_bar(aes(fill = variable), stat="identity", position = "stack") + ylab("ylabtxt") + xlab("Vuosi")
        plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
        plot <- plot + geom_hline(yintercept=0, color="red")
        plot <- plot + ggtitle("Konkurssien lukumäärä, konkursseissa työnsä menettäneiden lukumäärä")
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        
        #plot <- decompBars(data121(), "Konkurssien lukumäärä, konkursseissa työnsä menettäneiden lukumäärä")

      }
      
      plot %<>% ggplotly()
      
      plot <- plot %>% layout(font = font1,
                              yaxis = list(title = yvar),
                              xaxis = list(title = "Aika"))

      })

    output$DecompTimeSeriesRegion <- renderPlotly({
      
      if (!is.element("Palkit", input$graphchoice12)) {

        return(bankruptcygraphs(c("Konkurssit, aikasarja, toimialoittain"), FALSE, "maakunta"))

      } else if (is.element("Palkit", input$graphchoice12)) {

        return(bankruptcygraphs(c("Konkurssit, aikasarja, maakunnittain"), TRUE, "maakunta"))
        
        #return(decompBars(data124(), "Konkurssit, aikasarja, maakunnittain"))

      }

    })

    output$DecompTimeSeriesIndustry <- renderPlotly ({

      if (!is.element("Palkit", input$graphchoice12)) {

        return(bankruptcygraphs(c("Konkurssit, aikasarja, maakunnittain"), FALSE, "TOL"))

      } else if (is.element("Palkit", input$graphchoice12)) {

       return(bankruptcygraphs(c("Konkurssit, aikasarja, toimialoittain"), TRUE, "TOL"))
        
      #return(decompBars(data125(), "Konkurssit, aikasarja, toimialoittain"))

      }

    })
    
    output$konkurssiVuodet <- renderPlotly ({
      
      if (is.element("kumulatiivinen summa",input$graphchoice122) & is.element("logaritmina",input$graphchoice122)) {
        yvar <- "kumulatiivinen summa, logaritmi"
      } else if (!is.element("kumulatiivinen summa",input$graphchoice122) & is.element("logaritmina",input$graphchoice122)) {
        yvar <- "logaritmi"
      } else if (is.element("kumulatiivinen summa",input$graphchoice122) & !is.element("logaritmina",input$graphchoice122)) {
        yvar <- "kumulatiivinen summa"
      } else if (!is.element("kumulatiivinen summa",input$graphchoice122) & !is.element("logaritmina",input$graphchoice122)) {
        yvar <- "lukumäärä"
      }

      plot <- timeseries(data120(), c("Konkurssien määrä: kumulatiivinen summa, logaritmi"), FALSE, "2003-01-01", 
                        randcolor(), "muuttuja", input$variablechoice12, "Kuukausi")
      
      #plot <- plot + scale_x_datetime(breaks = as.POSIXct(data120()$vuosi), labels = month.abb)
      
      plot %<>% ggplotly()
      
      plot <- plot %>% layout(font = font1,
                              yaxis = list(title = yvar),
                              xaxis = list(title = "Kuukausi",
                                           #ticktext = list("tammikuu", "helmikuu", "maaliskuu", "huhtikuu", "toukokuu", "kesäkuu", 
                                            #               "heinäkuu", "elokuu", "syyskuu", "lokakuu", "marraskuu", "joulukuu"),
                                           #tickvals = list("2003-01-01", "2003-02-01", "2003-03-01", "2003-04-01", "2003-05-01", "2003-06-01",
                                            #               "2003-07-01", "2003-08-01", "2003-09-01", "2003-10-01", "2003-11-01", "2003-12-01")))
                                           #ticktext = list("huhtikuu", "heinäkuu", "lokakuu", "joulukuu", ),
                                           #tickvals = list(data120()$vuosi[4], data120()$vuosi[7], data120()$vuosi[10], data120()$vuosi[12])))
                                            #ticktext = list("huhtikuu", "heinäkuu", "lokakuu", "joulukuu"),
                                            #tickvals = list("Jan 2003", "Apr 2003", "Jul 2003", "Oct 2003")))
                                           ticktext = list("tammikuu","huhtikuu", "heinäkuu", "lokakuu"),
                                           tickvals = list(1, 4, 7, 10)))
      
    })

    ##### MAPS #####

    output$FinMapPlot <-renderPlot({

      finrelevant <- data14()
      
      mapdf <- join(mydata, finrelevant, by = "nutsname")
      
      #print(mapdf)
      
      mapdf %<>% na.omit()

      mapplot <- map(mapdf, input$muuttuja12, paste0("konkurssien piirissä olevia ",input$muuttuja12), "Valitut toimialat, valittu aikaväli", "id", 2, 8)

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
        plot <- timeseries(data[,c(1,2)], "Investointien muutos (vrt. edellisen vuoden vastaavaan kvartaaliin)", FALSE, 
                           "2018-01-01", DHcolors, "muuttuja", "lukumäärä", "Päivämäärä")

      } else if (is.element("Palkit, vuosineljänneksittäin", input$graphchoice31)) {

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
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, data32()$aloittaneet, data32()$lopettaneet)), 
                           "Aloittaneet ja lopettaneet yritykset, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors, 
                           "muuttuja", "lukumäärä", "Päivämäärä")
      } else if (is.element("Aikasarjana", input$graphchoice32) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        yearlyentry <- aggregate(aloittaneet~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, aloittaneet = yearlyentry, lopettaneet = yearlyexit)), 
                           "Aloittaneet ja lopettaneet yritykset, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors, 
                           "muuttuja", "lukumäärä", "Päivämäärä")
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Aikasarjana", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, FALSE, "Yritysten määrän nettomuutos, lukumäärä", TRUE)
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, (data32()$aloittaneet - data32()$lopettaneet))), 
                           "Yritysten määrän nettomuutos, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors, 
                           "muuttuja", "lukumäärä", "Päivämäärä")
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        yearlyentry <- aggregate(aloittaneet~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, nettomuutos = (yearlyentry - yearlyexit))), 
                           "Yritysten määrän nettomuutos, lukumäärä, aikasarja", FALSE, "2005-01-01", DHcolors, 
                           "muuttuja", "lukumäärä", "Päivämäärä")
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Aikasarjana", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, TRUE, FALSE, "Yritysten määrän nettomuutos, lukumäärä", TRUE)
        ggplotly()
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
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, (data32()$aloittaneet/data32()$ntotal), (data32()$lopettaneet/data32()$ntotal))), 
                           "Aloittaneet ja lopettaneet yritykset, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors, "muuttuja", "lukumäärä", "Päivämäärä")
      } else if (is.element("Aikasarjana", input$graphchoice32) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32))) {
        yearlyentry <- aggregate((aloittaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate((lopettaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, aloittaneet = yearlyentry, lopettaneet = yearlyexit)), "Aloittaneet ja lopettaneet yritykset, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors, "muuttuja", "lukumäärä", "Päivämäärä")
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Aikasarjana", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, FALSE, FALSE, TRUE, "Yritysten määrän nettomuutos, osuus yrityskannasta", TRUE)
        ggplotly()
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        plot <- timeseries(quartal_wrangler(data.frame(data32()$vuosineljännes, (data32()$aloittaneet/data32()$ntotal) - (data32()$lopettaneet/data32()$ntotal))), "Yritysten määrän nettomuutos, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors, "muuttuja", "lukumäärä", "Päivämäärä")
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (is.element("Aikasarjana", input$graphchoice32))) {
        yearlyentry <- aggregate((aloittaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        yearlyexit <- aggregate((lopettaneet/ntotal)~vuosi, data=data32(), FUN=sum)
        years <- c("2013Q1", "2014Q1", "2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1")
        plot <- timeseries(quartal_wrangler(data.frame(years, nettomuutos = (yearlyentry - yearlyexit))), "Yritysten määrän nettomuutos, osuus yrityskannasta, aikasarja", FALSE, "2005-01-01", DHcolors, "muuttuja", "lukumäärä", "Päivämäärä")
      } else if ((is.element("Näytä yritysten määrän nettomuutos", input$graphchoice32)) & (!is.element("Näytä vuosineljännekset", input$graphchoice32))
                 & (!is.element("Aikasarjana", input$graphchoice32))) {
        plot <- exitentryplot(data32(), FALSE, TRUE, TRUE, TRUE, "Yritysten määrän nettomuutos, osuuus yrityskannasta", TRUE)
        ggplotly()
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
        plot <- industryturnover(data35(), input$regionchoice32)
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

        bar <- industryturnover(data35(), input$regionchoice32)

        ggplotly(tooltip=c("x", "fill"))

      }

    })

    ##### FIRM SUBSIDY BARS #####

    output$subsidyPlot <- renderPlotly({

      handoutdata <- data33()
      
      if (is.element("Näytä osuus toimialalle tietyssä vuosineljänneksessä maksetuista tuista", input$graphchoice33)) {
        
        aggr <- aggregate(num~vuosineljännes, data = handoutdata, FUN = "sum")
        colnames(aggr) <- c("vuosineljännes", "sum") 
        handoutdata <- plyr::join(aggr, handoutdata, by = "vuosineljännes")
        handoutdata$num <- handoutdata$num/handoutdata$sum
        
      }
      
      #print(handoutdata)

      if (is.element(input$variablechoice33, c("Tukea saaneiden yritysten lukumäärä", "Suhdanneheikentymän vuoksi tukea saaneiden yritysten lukumäärä"))) {

        #colnames(handoutdata)
        colnames(handoutdata)[which(names(handoutdata) == "num")] <- "lukumäärä"
        varname <- "lukumäärä"

      } else if (is.element(input$variablechoice33, c("Maksetut suorat tuet", "Suhdanneheikentymän vuoksi maksetut suorat tuet"))) {

        #colnames(handoutdata)[1] <-"tukea"
        colnames(handoutdata)[which(names(handoutdata) == "num")] <- "tukea"
        varname <- "tukea"

      } else if (is.element(input$variablechoice33, c("Myönnetyt takaukset", "Suhdanneheikentymän vuoksi myönnetyt takaukset"))) {

        #colnames(handoutdata)[1] <-"takauksia"
        colnames(handoutdata)[which(names(handoutdata) == "num")] <- "takauksia"
        varname <- "takauksia"

      } else if (is.element(input$variablechoice33, c("Maksetut lainat", "Suhdanneheikentymän vuoksi maksetut lainat"))) {

        #colnames(handoutdata)[1] <-"lainaa"
        colnames(handoutdata)[which(names(handoutdata) == "num")] <- "lainaa"
        varname <- "lainaa"

      }
      

      if (grepl("lukumäärä", as.character(input$variablechoice33)) & is.element("Näytä osuus yrityskannasta (tukea saaneiden yritysten lukumäärä)", input$graphchoice33)
          & !is.element("Näytä osuus toimialalle tietyssä vuosineljänneksessä maksetuista tuista", input$graphchoice33)) {
        
        ylabtext <- "Tukea saaneiden yritysten osuus yrityskannasta"
        arg_position <- "dodge"

      } else if (grepl("lukumäärä", as.character(input$variablechoice33)) & !is.element("Näytä osuus yrityskannasta (tukea saaneiden yritysten lukumäärä)", input$graphchoice33)
                 & !is.element("Näytä osuus toimialalle tietyssä vuosineljänneksessä maksetuista tuista", input$graphchoice33)) {
        
        ylabtext <- "Yritystä"
        arg_position <- "dodge"

      }

      else if (!is.element("Näytä osuus yrityskannasta (tukea saaneiden yritysten lukumäärä)", input$graphchoice33)
               & !is.element("Näytä osuus toimialalle tietyssä vuosineljänneksessä maksetuista tuista", input$graphchoice33)) {
        
        ylabtext <- "Tuhatta euroa"
        arg_position <- "dodge"

      } else if (!grepl("lukumäärä", as.character(input$variablechoice33)) & !is.element("Näytä osuus yrityskannasta (tukea saaneiden yritysten lukumäärä)", input$graphchoice33)
                  & is.element("Näytä osuus toimialalle tietyssä vuosineljänneksessä maksetuista tuista", input$graphchoice33)) {
        
        ylabtext <- "Osuus toimialalle maksetuista tuista"
        arg_position <- "stack"
        colnames(handoutdata)[which(names(handoutdata) == "num")] <- "osuus"
        colnames(handoutdata)[which(names(handoutdata) == "lukumäärä")] <- "osuus"
        colnames(handoutdata)[which(names(handoutdata) == "tukea")] <- "osuus"
        colnames(handoutdata)[which(names(handoutdata) == "takauksia")] <- "osuus"
        colnames(handoutdata)[which(names(handoutdata) == "lainaa")] <- "osuus"
        varname <- "osuus"
        
      }
      
      #print(handoutdata)
      
      if (exists("ylabtext")) {
        
        plot <- ggplot(handoutdata, aes_string(x="vuosineljännes", y = varname, fill="kokoluokka")) + xlab("Vuosineljännes") + ylab(ylabtext) + geom_bar(position = arg_position, stat="identity") + scale_fill_manual(values=colorsample(5))
        plot <- plot + scale_y_continuous(labels = tuhaterotin)
        ggplotly()
        
      } else {
        
        return(NULL)
        
      }

    })

    output$productivityTurnoverScatter <- renderPlotly({
      
      if (input$graphchoice221 == "maakunnittain") {
        
        if (input$toimialat4 == "Yhteensä") {
          data1 <- data44()
          data1$toimiala <- rep("Yhteensä", rep = nrow(data1))
          colnames(data1) <- c("maakunta", "vuosi", "tuottavuus", "yrityskanta", "toimiala")
        } else {
          data1 <- data46()[,-c(5)]
          colnames(data1) <- c("maakunta", "vuosi", "toimiala", "tuottavuus")
          data1 <- merge(x = data1, y = data47(), by.x = c("vuosi", "toimiala", "maakunta"), 
                        by.y = c("vuosi", "toimiala", "maakunta"))
          data1 <- data1[data1$toimiala == input$toimialat4 & data1$vuosi == input$years4,]
          #data1 <- data1[,-c(3)]
        }
        
        data2 <- data321()[, c(1, 2)]
        data2$kategoria <- substring(data2$kategoria, 6)
        colnames(data2) <- c("maakunta", "vaihtuvuus")
        
        data <- join(data1, data2, by= "maakunta")
        
        var <- "maakunta"
        
      } else if (input$graphchoice221 == "toimialoittain") {
        
        data1 <- data43()
        data2 <- data35()
        
        colnames(data1) <- c("toimiala", "vuosi", "tuottavuus", "yrityskanta")
        colnames(data2) <- c("toimiala", "vaihtuvuus")
        
        for (i in 1:nrow(data2)) {
          
          if (is.element(substr(data2$toimiala[i], 1, 1), c("L", "O", "P", "D", "U"))) {
            data2$toimiala[i] <- substring(data2$toimiala[i], 1, nchar(data2$toimiala[i])-5)
          } else {
            data2$toimiala[i] <- substring(data2$toimiala[i], 1, nchar(data2$toimiala[i])-8)
          }
          
        }
        
        data2$toimiala <- substring(data2$toimiala, 3)
        
        #data2$toimiala <- gsub("[[:punct:]]", "", data2$toimiala)
        #data2$toimiala <- substring(data2$toimiala, 1, nchar(data2$toimiala)-1) 
        
        data <- join(data1, data2, by= "toimiala")
        
        #print(data)
        
        var <- "toimiala"
      }

      if (is.element("Trendi", input$graphchoice22) & !is.element("Havaintojen lukumäärällä painotettu trendi", input$graphchoice22)) {

        plot <- scatterplot(data, c("Yrityskannan vaihtuvuus"), c("vaihtuvuus", "tuottavuus", var, "yrityskanta"), TRUE, FALSE)

      } else if (!is.element("Trendi", input$graphchoice22)) {

        plot <- scatterplot(data, c("Yrityskannan vaihtuvuus"), c("vaihtuvuus", "tuottavuus", var, "yrityskanta"), FALSE, FALSE)
    
      } else if (is.element("Havaintojen lukumäärällä painotettu trendi", input$graphchoice22)) {
        
        plot <- scatterplot(data, c("Yrityskannan vaihtuvuus"), c("vaihtuvuus", "tuottavuus", var, "yrityskanta"), FALSE, TRUE)
        
      }

      ggplotly(plot)

    })

    output$productivityBankruptcyScatter <- renderPlotly({
      
      if (input$graphchoice221 == "maakunnittain") {
        
        data42 <- data42()
        
        if (input$toimialat4 == "Yhteensä") {
          data1 <- data44()
          data1$toimiala <- rep("Yhteensä", rep = nrow(data1))
          colnames(data1) <- c("maakunta", "vuosi", "tuottavuus", "yrityskanta", "toimiala")
        } else {
          data1 <- data46()[,-c(5)]
          colnames(data1) <- c("maakunta", "vuosi", "toimiala", "tuottavuus")
          data1 <- merge(x = data1, y = data47(), by.x = c("vuosi", "toimiala", "maakunta"), 
                         by.y = c("vuosi", "toimiala", "maakunta"))
          colnames(data1) <- c("vuosi", "toimiala", "maakunta", "tuottavuus", "yrityskanta")
          #data1 <- data1[data1$toimiala == input$toimialat4 & data1$vuosi == input$years4,]
          #data1 <- data1[,-c(3)]
        }
        colnames(data42) <- c("maakunta", "konkursseja")
        
        data42 <- data42[data42$maakunta != "KOKO MAA" & data42$maakunta != "MA1 MANNER-SUOMI" & data42$maakunta != "MA2 AHVENANMAA",]
        data42$maakunta <- substring(data42$maakunta, 6)
        
        #print("data42:")
        #print(data42)
        #print(colnames(data42))
        #print("data1:")
        #print(data1)
        #print(colnames(data1))
        
        data <- plyr::join(data1, data42, by = "maakunta")
        data$konkurssitn <- as.numeric(data$konkursseja)/as.numeric(data$yrityskanta)
        
        data <- data[!duplicated(colnames(data))]
        
        var <- "maakunta"
        
      } else if (input$graphchoice221 == "toimialoittain") {
        
        data1 <- data43()
        #print(data45())
        data2 <- data45()
        var <- "toimiala"
        
        colnames(data1) <- c("toimiala", "vuosi", "tuottavuus", "yrityskanta")
        colnames(data2) <- c("toimiala", "maakunta", "konkursseja", "vuosi")
        
        print(data1)
        print(data2)
        
        #data2$toimiala[data2$toimiala == "Maa-, metsä ja kalatalous"] <- "Maatalous, metsätalous ja kalatalous"
        #data2$toimiala[data2$toimiala == "Rakennustoiminta"] <- "Rakentaminen"
        #data2$toimiala[data2$toimiala == "Muut palvelut"] <- "Muu palvelutoiminta"
        #data2$toimiala[data2$toimiala == "Kauppa"] <- "Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus"
        #data2$toimiala[data2$toimiala == "Teollisuus, kaivostoiminta sekä energia- ja vesihuolto"] <- "Teollisuus"
        
        data <- join(data1, data2, by = "toimiala")
        
        data <- data[, c(1, 2, 3, 4, 6)]
        
        data$konkurssitn <- as.numeric(data$konkursseja)/as.numeric(data$yrityskanta)
        
        print(data)
        
      }

      if (is.element("Trendi", input$graphchoice22) & !is.element("Havaintojen lukumäärällä painotettu trendi", input$graphchoice22)) {

        plot <- scatterplot(data, "konkurssitodennäköisyys (konkurssien lukumäärä/yrityskanta)", c("konkurssitn", "tuottavuus", var, "yrityskanta"), TRUE, FALSE)

      } else if (!is.element("Trendi", input$graphchoice22)) {
        
        plot <- scatterplot(data, "konkurssitodennäköisyys (konkurssien lukumäärä/yrityskanta)", c("konkurssitn", "tuottavuus", var ,"yrityskanta"), FALSE, FALSE)
        
      } else if (is.element("Havaintojen lukumäärällä painotettu trendi", input$graphchoice22)) {
        
        plot <- scatterplot(data, c("konkurssitodennäköisyys (konkurssien lukumäärä/yrityskanta)"), c("konkurssitn", "tuottavuus", var, "yrityskanta"), FALSE, TRUE)
        
      }                    

      ggplotly(plot)
      


    })
    
    #output$productivityHistogram <- renderPlotly({
      
    #})

    ##### REVENUE SERIES #####

    output$revenuePlot <- renderPlotly({

      revenuedata <- data34()
      
      if (is.element("Näytä indeksin perusvuosi", input$graphchoice34)) {

        revenueseries <- timeseries(revenuedata, "Yritysten liikevaihtoennakot, sarja", TRUE, "2015-08-01", c(DHcolors, DHcolors, DHcolors), "muuttuja", "liikevaihto", "Päivämäärä")

      } else if (!is.element("Näytä indeksin perusvuosi", input$graphchoice34)) {

        revenueseries <- timeseries(revenuedata, "Yritysten liikevaihtoennakot, sarja", FALSE, "2015-08-01", c(DHcolors, DHcolors, DHcolors), "muuttuja", "liikevaihto", "Päivämäärä")

      } 

      return(revenueseries)

    })
    
    ##### BANKRUPTCY DISTRIBUTIONS #####
    
    data52 <- reactive({
      
      konkurssijakauma <- read.csv("data/14072023_konkurssijakauma_toimialat.csv")
      konkurssijakauma$toimiala <- iconv(konkurssijakauma$toimiala, from = "ISO-8859-1", to = "UTF-8")
      
      #konkurssijakauma <- read.csv("data/19072023_konkurssit_pylväät_86_22.csv")
      #konkurssijakauma$toimiala <- iconv(konkurssijakauma$toimiala, from = "ISO-8859-1", to = "UTF-8")
      
      data <- konkurssijakauma[konkurssijakauma$toimiala == input$toimialat122,]
      
      breaks_diff <- (data$breaks[2] - data$breaks[1])/2
      
      data %<>% na.omit()
      
      breaks_pituus <- length(data$breaks)
      
      data$breaks <- data$breaks + (data$breaks[2] - data$breaks[1])/2
      
      data <- data[,-c(1)]
      
      breaks <- data$breaks
      
      suurin_N <- data$y[breaks_pituus]/sum(data$y, na.rm=TRUE)

      suurin_n <- data$x[breaks_pituus]/sum(data$x, na.rm=TRUE)

      pienin_N <- data$y[1]/sum(data$y, na.rm=TRUE)

      pienin_n <- data$x[1]/sum(data$x, na.rm=TRUE)
      
      returnvec <- c(breaks[1], breaks[2], breaks[breaks_pituus-1], breaks[breaks_pituus],
                     suurin_N, suurin_n, pienin_N, pienin_n, breaks_diff)
      
    })
    
    output$konkurssijakaumaBarplot <- renderPlot({
      
      data <- data51()
      
      data %<>% na.omit()
      
      data$breaks <- data$breaks + (data$breaks[2] - data$breaks[1])/2 
      data <- data %>% dplyr::rename("työntekijöitä" = "breaks")
      
      plot <- ggplot(data = data, aes(x = työntekijöitä)) + #, y = 0.2*y 
        geom_bar(aes(y = y/sum(y)), stat= "identity", fill = "red", colour = "red", alpha = 0.2) +

        geom_bar(aes(y = x/sum(x)), stat= "identity", fill = "blue", colour = "blue", alpha = 0.2) + 
        #ggrepel::geom_text_repel(aes(y = y/sum(y), label = round(y/sum(y), digits = 3)), colour = "red") + #, vjust = -0.2 
        #ggrepel::geom_text_repel(aes(y = y/sum(y), label = round(x/sum(x), digits = 3)), colour = "blue") + #, vjust = -1.2
        
        geom_text(aes(y = x/sum(x), label = round(y/sum(y), digits = 3)), colour = "red", vjust = -2.5) + # 
        geom_text(aes(y = x/sum(x), label = round(x/sum(x), digits = 3)), colour = "blue", vjust = -1) + # 
        
        scale_y_continuous(
          #name= "yritysten lkm",
          name= "",
          limits = c(0, 1.1),
          #sec.axis = sec_axis(~., name = "työntekijöiden lkm"),
        ) + 
        
        scale_x_continuous(
          breaks = pad_breaks(data$työntekijöitä + (data$työntekijöitä[2] - data$työntekijöitä[1])/2),
        )
      
      print("Not padded:")
      print(data$työntekijöitä + (data$työntekijöitä[2] - data$työntekijöitä[1])/2)
      
      print("Padded:")
      print(pad_breaks(data$työntekijöitä + (data$työntekijöitä[2] - data$työntekijöitä[1])/2))
      
      return(plot)
      
    })
    
    output$konkurssijakaumaVbox_pienet <- renderValueBox({
      
      data <- data52()
      
      valueBox(
        
        paste0("Pienimmässä kokoluokassa (", as.character(data[1]-data[9]), "-", as.character(data[1]+data[9]),
               " työntekijää):"), 
        
        paste0(as.character(signif(100*data[8], 3)), "% konkursseista, ", as.character(signif(100*data[7], 3)),
               "% konkursseissa työnsä menettänyttä")
        
      )
      
    })
    
    output$konkurssijakaumaVbox_suuret <- renderValueBox({
      
      data <- data52()
      
      valueBox(
        
        paste0("Suurimmassa kokoluokassa (", as.character(data[4]-data[9]), "-", as.character(data[4]+data[9]),
               " työntekijää):"), 
        
        paste0(as.character(signif(100*data[6], 3)), "% konkursseista, ", as.character(signif(100*data[5], 3)),
               "% konkursseissa työnsä menettänyttä")
        
      )
      
    })
    
    output$IndustryChoiceTextBox11 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse toimialat:", style = "font-weight:bold")
        
      )
      
    })
    
    output$RegionChoiceTextBox11 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse maakunnat:", style = "font-weight:bold")
        
      )
      
    })
    
    output$YearChoiceTextBox12 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse vuodet:", style = "font-weight:bold")
        
      )
      
    })
    
    output$YearChoiceTextBox12 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse vuodet:", style = "font-weight:bold")
        
      )
      
    })
    
    output$QuartalChoiceTextBox2 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse vuosineljännekset:", style = "font-weight:bold")
        
      )
      
    })
    
    output$QuartalChoiceTextBox3 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse vuosineljännekset:", style = "font-weight:bold")
        
      )
      
    })
    
    output$YearChoiceTextBox12 <- renderValueBox({
      
      valueBox(
        
        "",
        
        tags$p("Valitse vuodet:", style = "font-weight:bold")
        
      )
      
    })
    
    output$otsikko01 <- renderValueBox({
      
      valueBox(
        "Konkurssien määrä; yritykset (punainen) ja työntekijät (sininen)",
        "Indeksoitu, liukuva keskiarvo"
      )
      
    })
    
    #output$otsikko01 <- renderText({"Konkurssien määrä; yritykset (punainen) ja työntekijät (sininen)"})
    
    output$otsikko02 <- renderText({"Palkkasummaindeksi. Koko maa, kaikki toimialat"})
    
    output$otsikko03 <- renderText({"Kuluttajahintaindeksi"})
    
    output$otsikko04 <- renderText({"Teollisuuden tuotantosuhdanneindeksi"})
    
    #output$otsikko111 <- renderText({paste0("Konkurssien piirissä olevia ", input$muuttuja12)})
    
    #output$otsikko112 <- renderText({paste0("Konkurssien piirissä olevia ", input$muuttuja12, ": toimialoittain")})
    
    #output$otsikko113 <- renderText({paste0("Konkurssien piirissä olevia ", input$muuttuja12, ": maakunnittain")})
    
    output$otsikko111 <- renderText({paste0("Konkursseja (", input$muuttuja121, "), toimialoittain")})
    
    output$otsikko112 <- renderText({paste0("Konkursseja ( ", input$muuttuja121, ") toimialoittain")})
    
    output$otsikko113 <- renderText({paste0("Konkursseja (", input$muuttuja121, ") maakunnittain")})
    
    output$otsikko12 <- renderText({"Konkurssien vuosittainen kehitys"})
    
    output$otsikko4 <- renderText({paste0(input$variablechoice33, ": ", input$industrychoice33)})
    
    output$otsikko5 <- renderText({"Yritysten liikevaihtoennakot: aikasarjoja"})

    output$otsikko61 <- renderText({paste0("Sirontakuvio: tuottavuus ja yrityskannan vaihtuvuus ", input$graphchoice221)})
    
    output$otsikko62 <- renderText({paste0("Sirontakuvio: tuottavuus ja konkurssin todennäköisyys ", input$graphchoice221)})
    
    output$otsikko63 <- renderText({"Yritysten tuottavuusjakauma kokoluokittain"})
    
    output$konkurssiBoxplotit <- renderPlot({
      
      
      tags$p("Valitse toimialat:", style = "font-weight:bold")
      data <- data6()
      
      #print(data6())
      
      data$kokoluokka <- factor(data$kokoluokka, levels = c("mikroyritys","pienyritys", "keskisuuri yritys", "suuryritys"))
      
      plot <- ggplot(data,
       aes(kokoluokka)
      )
      
      plot <- plot + 
        geom_boxplot(
          aes(
            min = y_min,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_max
          ), colour = "black", width = 0.75, stat = "identity"
        )
      
      return(plot)
      
      #ggplotly()
      
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

      plot <- timeseries(data, "Tiettyihin asiasanoihin kohdistuvat Google-haut", FALSE, "2005-01-01", DHcolors2, "muuttuja", "kiinnostus", "Päivämäärä")

    })

    output$bankruptcySeries <- renderPlotly({

      konkurssisarja <- time_series_wrangler(konkurssisarja)

      plot <- timeseries(konkurssisarja, "Konkurssit", TRUE, "2005-01-01", DHcolors, "muuttuja", "lukumäärä", "Päivämäärä")
      
      plot %<>% ggplotly()
      
      plot <- plot %>% layout(showlegend = FALSE,
                              yaxis = list(title = "indeksi"),
                              xaxis = list(title = "aika"))

    })

    output$inflationSeries <- renderPlotly({

      kuluttajahintaindeksi <- time_series_wrangler(kuluttajahintaindeksi)

      colnames(kuluttajahintaindeksi)[2] <- "indeksi"

      plot <- timeseries(kuluttajahintaindeksi, "Kuluttajahintaindeksi", TRUE, "2005-01-01", DHcolors, "muuttuja", "indeksi", "Päivämäärä")
      
      plot %<>% ggplotly()
      
      plot <- plot %>% layout(showlegend = FALSE,
                              yaxis = list(title = "indeksi"),
                              xaxis = list(title = "aika"))

    })

    output$payrollSeries <- renderPlotly({

      palkkasummaindeksi <- time_series_wrangler(palkkasummaindeksi)

      colnames(palkkasummaindeksi)[2] <- "indeksi"

      plot <- timeseries(palkkasummaindeksi, "Palkkasumma, indeksisarja", TRUE, "2015-01-01", DHcolors, "muuttuja", "indeksi", "Päivämäärä")
      
      plot %<>% ggplotly()
      
      plot <- plot %>% layout(showlegend = FALSE,
                              yaxis = list(title = "indeksi"),
                              xaxis = list(title = "aika"))

    })

    output$cycleSeries <- renderPlotly({

      tuotantosuhdanneindeksi <- time_series_wrangler(tuotantosuhdanneindeksi)

      colnames(tuotantosuhdanneindeksi)[2] <- "indeksi"

      plot <- timeseries(tuotantosuhdanneindeksi, "tuotantosuhdanne, indeksisarja", TRUE, "2015-01-01", DHcolors, "muuttuja", "indeksi", "Päivämäärä")
      
      plot %<>% ggplotly()
      
      plot <- plot %>% layout(showlegend = FALSE,
                              yaxis = list(title = "indeksi"),
                              xaxis = list(title = "aika"))

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

      # Tekee kuvaajat Aloittaneet ja Lopettaneet Yritykset -sivulle. Tätä tuskin kannattaa käyttää mihinkään muuhun tarkoituksee.

      data1 <- data

        data <- data.frame(vuosineljännes = data1$vuosineljännes,
                           aloittaneet = as.numeric(data1$aloittaneet), lopettaneet = as.numeric(data1$lopettaneet), total = as.numeric(data1$ntotal),
                           vuosi = factor(as.character(substr(data1$vuosineljännes, 1, 4))),
                           neljännes = as.character(substr(data1$vuosineljännes, 5, 6)),
                           group = c(rep("aloittaneet", nrow(data1)), rep("lopettaneet", nrow(data1))),
                           aloittaneet_lopettaneet = c(as.numeric(data1$aloittaneet), - as.numeric(data1$lopettaneet)),
                           nettomuutos = (as.numeric(data1$aloittaneet) - as.numeric(data1$lopettaneet)))

        if (proportional) {

          data$aloittaneet <- as.numeric(data1$aloittaneet)/as.numeric(data1$ntotal)
          data$lopettaneet <- as.numeric(data1$lopettaneet)/as.numeric(data1$ntotal)
          data$aloittaneet_lopettaneet <- c(as.numeric(data1$aloittaneet)/as.numeric(data1$ntotal), - as.numeric(data1$lopettaneet)/as.numeric(data1$ntotal))
          data$nettomuutos <- as.numeric(data1$aloittaneet)/as.numeric(data1$ntotal) - as.numeric(data1$lopettaneet)/as.numeric(data1$ntotal)

        }

        ydata <- data[,c(1:5)]
        ydata %>% distinct(vuosineljännes, .keep_all=TRUE)

        yearlyentry <- aggregate(aloittaneet~vuosi, data=ydata, FUN=sum)
        yearlyexit <- aggregate(lopettaneet~vuosi, data=ydata, FUN=sum)

        ydata <- data.frame(
          lopettaneet = yearlyexit$lopettaneet,
          aloittaneet = yearlyentry$aloittaneet,
          nettomuutos = yearlyentry$aloittaneet - yearlyexit$lopettaneet,
          aloittaneet_lopettaneet = c(as.numeric(yearlyentry$aloittaneet), - as.numeric(yearlyexit$lopettaneet)),
          vuosi = yearlyentry$vuosi)

        ylabtxt = "Lukumäärä"

        if (proportional) {
          ylabtxt = "Osuus yrityskannasta"
        }


      if (entry==TRUE & double == FALSE & yearly == FALSE & net == FALSE) {

        plot <- generalGgroupedBarPlot(data, "vuosi", "aloittaneet", "neljännes", ylabtxt, "Vuosi", title, TRUE)

      } else if (entry==FALSE & double == FALSE & yearly == FALSE & net == FALSE) {

        plot <- generalGgroupedBarPlot(data, "vuosi", "lopettaneet", "neljännes", ylabtxt, "Vuosi", title, TRUE)

      } else if (double == TRUE & yearly == FALSE & net == FALSE) {

        plot <- generalGgroupedBarPlot(data, "vuosi", "aloittaneet_lopettaneet", "neljännes", ylabtxt, "Vuosi", title, TRUE)

      } else if (entry==TRUE & double == FALSE & yearly == TRUE & net == FALSE) {

        plot <- generalGgroupedBarPlot(ydata, "vuosi", "aloittaneet", "vuosi", ylabtxt, "Vuosi", title, TRUE)

      } else if (entry==FALSE & double == FALSE & yearly == TRUE & net == FALSE) {

        plot <- generalGgroupedBarPlot(ydata, "vuosi", "lopettaneet", "vuosi", ylabtxt, "Vuosi", title, TRUE)

      } else if (double == TRUE & yearly == TRUE & net == FALSE) {

        plot <- generalGgroupedBarPlot(ydata, "vuosi", "aloittaneet_lopettaneet", "vuosi", ylabtxt, "Vuosi", title, TRUE)

      } else if (yearly == FALSE & net == TRUE) {

		    plot <- generalGgroupedBarPlot(data, "vuosi", "nettomuutos", "neljännes", "Nettomuutos, lukumäärä", "Vuosi", title, TRUE)

      } else if (yearly == TRUE & net == TRUE) {

		    plot <- generalGgroupedBarPlot(ydata, "vuosi", "nettomuutos", "Vuosi", "Nettomuutos, osuus yrityskannasta", "Vuosi", title, TRUE)

      }

    }

	generalGgroupedBarPlot <- function(data, xax, yax, fill,  ylabtxt, xlabtxt, title, dodge) {

	  # Tekee palkkikuvaajan, jossa eri vuosien data on ryhmitelty vuosineljänneksittäin, kuukausittain tai vuosittain.
	  # argumentti 'data' on data.frame, jossa sarakkeina numeerista dataa, vuosi, ja mahdollisesti vuosineljännes/kuukausi
	  # xax on muuttuja, jonka mukaan data ryhmitellään (vuosi/vuosineljännes/kuukausi)
	  # yax on numeerinen muuttuja, jota kuvataan
	  # fill on vuosimuuttujan nimi; joustavuuden takia en ole kovakoodannut tätä.
	  # xax, yax ja fill merkkijonoina
	  
	plot <- ggplot(data, aes_string(x= xax, y= yax, fill = fill)) + ylab(ylabtxt) + xlab(xlabtxt) +
        scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors, DHcolors)) +
        geom_hline(yintercept=0, color="red") +
        ggtitle(title) +
        scale_y_continuous(labels = tuhaterotin)

	if (dodge) {
	  plot <- plot + geom_bar(position="dodge", stat="identity")
	} else {
	  plot <- plot + geom_bar(stat="identity")
	}

	return(plot)

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

      #Tekee suomen kartan

      df <- join(mapdata, mydata, by="id")

      df <- join(df, coordinates, by="id")

      df <- join(df, data, by=id)

      for (i in 1:nrow(df)) {
        if (is.na(df$NUTS3[i]) == TRUE) {
          df$nobs[i] <- NA
        }
      }

      df <-  rotate_map(df, 30)

      df <- df[, !duplicated(colnames(df))]

      gg <- ggplot() + geom_polygon(data = df, aes(x = long, y = lat, group = group, fill=round(df[,which(colnames(df)==varname)], decimals)), color = "darkgrey", size = 0.5)
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

    timeseries <- function(data, texts, argument, baseyear, colours, var, val, aikamääre) {

      #Tekee aikasarjan

      colnames(data)[1] <- aikamääre

      data <- data %>%
        select(everything()) %>%
        gather(key = "Muuttuja", value = "arvo", -aikamääre)
      
      timeseries <- ggplot(data, aes_string(x=aikamääre, y="arvo")) + geom_line(aes(color=Muuttuja))# + scale_color_manual(colours)

      if (argument) {

        timeseries <- timeseries + geom_vline(xintercept = as.numeric(as.POSIXct(baseyear)), col = "red")

      } else {

        timeseries

      }

    }

    industryturnover <- function(data, maakunta) {

      turnoverindustrygroup <- data

      colnames(turnoverindustrygroup) <- c("Toimiala","Vaihtuvuus")
      turnoverindustrygroup$Kirjaintaso <- substr(turnoverindustrygroup$Toimiala, 1, 2)

      bar <- ggplot(turnoverindustrygroup, aes(x=Vaihtuvuus, y=Kirjaintaso, fill = Toimiala))+ geom_bar(position = "dodge", stat="identity")
      bar <- bar + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
      bar <- bar + labs(title = paste0("Toimialakohtainen vaihtuvuus maakuntatasolla, valinta: ", maakunta))#, subtitle = as.character(paste0("Maakunta: ", maakunta)))
      #bar <- bar + expression(atop("Toimialakohtainen vaihtuvuus maakuntatasolla", atop(paste0("Maakunta: ", maakunta), "")))
      bar <- bar + theme(legend.title = element_text(size=10),
                         legend.text = element_text(size=8),
                         legend.key.size = unit(0.5, 'cm'),
                         legend.key.height = unit(0.5, 'cm'),
                         legend.key.width = unit(0.5, 'cm'),
                         axis.title.y = element_blank())

      return(bar)

    }

    bankruptcygraphs <- function(texts, bars, var) {

      # Tekee kuvaajat Konkurssit-sivulle

      #BRCfin <- BRCfin %>% dplyr::rename("konkursseja" = "nobs")
      
      yvar <- "lukumäärä"

      if (bars == FALSE & var == "TOL") {
        
        BRCfinmod <- data126()
        
        if (is.element("Liukuva keskiarvo", input$graphchoice12)) {
          
          BRCfinmod[,-c(1)] <- sapply(BRCfinmod[,-c(1)], function(x) manual_rollmean(x, 12))
          
        }
        
        if (is.element("Indeksöity", input$graphchoice12)) {
          
          BRCfinmod[,-c(1)] <- sapply(BRCfinmod[,-c(1)], function(x) indeksöi(x))
          
          yvar <- "indeksi"
          
        }
        
      #print("Decomp from bankruptcygraphs:")
      #print(BRCfinmod)
        
        if (!is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
            
            names <- c("time", paste0(rep(c("yrityksiä/"), times = length(input$regions12)), 
                                      rep(input$regions12)))
            
        } else if (is.element("työntekijöitä", input$muuttuja121) & !is.element("yrityksiä", input$muuttuja121)) {
            
            names <- c("time", paste0(rep(c("työntekijöitä/"), times = length(input$regions12)), 
                                      rep(input$regions12)))
            
        } else if (is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
          
            names <- c("time", paste0(rep(c("yrityksiä/", "työntekijöitä/"), times = length(input$regions12)), 
                                      rep(input$regions12, each =2)))
          
        }
        
        colnames(BRCfinmod) <- names
        

        plot <- timeseries(BRCfinmod, texts, FALSE, as.character(sys.date()), c(DHcolors, DHcolors, DHcolors), 
                           "muuttuja", "lukumäärä", "Päivämäärä")
        
        plot %<>% ggplotly()
        
        plot <- plot %>% layout(font = font1,
                                yaxis = list(title = yvar),
                                xaxis = list(title = "Aika"))
        
        return(plot)

      } else if (bars==TRUE & var == "TOL") {

        data <- data123()

        #data <- aggregate("yrityksiä"~"date"+"TOL", data= data, FUN="sum")
        
        data <- aggregate(formula(paste0(input$muuttuja12, "~date+TOL")), data = data, FUN = "sum")

        plot <- generalGgroupedBarPlot(data, "date", input$muuttuja12, "TOL", "ylabtxt", "vuosi", texts[1], FALSE)

        plot %<>% ggplotly()
        
        plot <- plot %>% layout(font = font1,
                                yaxis = list(title = yvar),
                                xaxis = list(title = "Aika"))

        return(plot)

      } else if (bars==FALSE & var == "maakunta") {

        #decomp <- data.frame(time=unique(data125()$date))

        BRCfinmod <- data127()
        
        if (is.element("Liukuva keskiarvo", input$graphchoice12)) {
          
          BRCfinmod[,-c(1)] <- sapply(BRCfinmod[,-c(1)], function(x) manual_rollmean(x, 12))
          
        }
        
        if (is.element("Indeksöity", input$graphchoice12)) {
          
          BRCfinmod[,-c(1)] <- sapply(BRCfinmod[,-c(1)], function(x) indeksöi(x))
          
          yvar <- "indeksi"
          
        }
        
        #print("Decomp from bankruptcygraphs:")
        #print(BRCfinmod)

        if (!is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
          
          names <- c("time", paste0(rep(c("yrityksiä/"), times = length(input$industries12)), 
                                    input$industries12))
          
        } else if (is.element("työntekijöitä", input$muuttuja121) & !is.element("yrityksiä", input$muuttuja121)) {
          
          names <- c("time", paste0(rep(c("työntekijöitä/"), times = length(input$industries12)), 
                                    input$industries12))
          
        } else if (is.element("työntekijöitä", input$muuttuja121) & is.element("yrityksiä", input$muuttuja121)) {
          
          names <- c("time", paste0(rep(c("yrityksiä/", "työntekijöitä/"), times = length(input$industries12)), 
                                    rep(input$industries12, each =2)))
          
        }
        
        colnames(BRCfinmod) <- names

        plot <- timeseries(BRCfinmod, texts, FALSE, as.character(sys.date()), c(DHcolors, DHcolors, DHcolors), 
                           "muuttuja", "lukumäärä", "Päivämäärä")

        plot %<>% ggplotly()
        
        plot <- plot %>% layout(font = font1,
                                yaxis = list(title = yvar),
                                xaxis = list(title = "Aika"))
        
      } else if (bars==TRUE & var == "maakunta") {

        data <- data122()
        
        #data <- aggregate("yrityksiä"~"date"+"maakunta", data = data, FUN="sum")
        
        data <- aggregate(formula(paste0(input$muuttuja12, "~date+maakunta")), data = data, FUN = "sum")

        plot <- generalGgroupedBarPlot(data, "date", input$muuttuja12, "maakunta", "ylabtxt", "vuosi", texts[1], FALSE)
        
        plot %<>% ggplotly()
        
        plot <- plot %>% layout(font = font1,
                                yaxis = list(title = yvar),
                                xaxis = list(title = "Aika"))

      }

    }

    scatterplot <- function(data, texts, vars, includeTrendline, weighted) {

      # Tekee scatterplotin
      
      plot <- ggplot(data, aes_string(x=vars[1], y=vars[2])) 

      if (includeTrendline) {
        
        plot <- plot + geom_smooth(mapping = aes_string(x=vars[1], y=vars[2]), method="lm", 
                                   se=FALSE)

      }
      
      if (weighted) {
      
        plot <- plot + geom_smooth(mapping = aes_string(x=vars[1], y=vars[2], weight = vars[4]), method="lm", 
                                   se=FALSE) 
      
      }
      
      plot <- plot + geom_point(aes_string(fill=vars[3], size=vars[4]))
      plot <- plot + scale_x_continuous(name = texts[1])
      
      return(plot)

    }
    
    decompBars <- function(data, title) {
      
      dfm <- reshape2::melt(data[,colnames(data)],id.vars = 1)
      
      plot <- ggplot(dfm, aes_string(x = colnames(data)[1], y = "value")) + geom_bar(aes(fill = variable), stat="identity", position = "stack") + ylab("ylabtxt") + xlab("Vuosi")
      plot <- plot + scale_fill_manual(values=c(DHcolors, DHcolors, DHcolors))
      plot <- plot + geom_hline(yintercept=0, color="red")
      plot <- plot + ggtitle("Konkurssien lukumäärä, konkursseissa työnsä menettäneiden lukumäärä")
      plot <- plot + scale_y_continuous(labels = tuhaterotin)
      
     
      return(plot) 
    }
    


}

##### RUN THE APP #####

shinyApp(ui = ui, server = server)




