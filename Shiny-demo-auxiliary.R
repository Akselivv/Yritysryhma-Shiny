##### INSTALL AND REQUIRE PACKAGES #####

##### DISABLE SCIENTIFIC NOTATION OF VALUES IN PLOT #####

options(scipen=999)

##### READ DATA #####

suppressMessages(
  
  investmentchangedata <- readxl::read_excel("~/R/Shiny_demo/data/001_12r6_2022q4_20230511-092217.xlsx")

)

suppressMessages(
  
  entryexitdata <- readxl::read_excel("~/R/Shiny_demo/data/001_11yq_2022q4_20230511-122745.xlsx")
  
)

suppressMessages(
  
  handoutdata <- readxl::read_excel('~/R/Shiny_demo/data/001_12s5_2021q4_20230515-084538.xlsx')
  
)

suppressMessages(
  
  revenuedata <- readxl::read_excel("~/R/Shiny_demo/data/001_123y_2023m03_20230516-115325.xlsx")
  
)

suppressMessages(
  
  konkurssisarja <- readxl::read_excel("~/R/Shiny_demo/data/konkurssit.xlsx")
  
)

konkurssisarja <- data_cropper(konkurssisarja, c(1:2), c(1:2), c(449:480))

suppressMessages(
  
  palkkasummaindeksi <- readxl::read_excel("~/R/Shiny_demo/data/palkkasummaindeksi.xlsx")
  
)

palkkasummaindeksi <- data_cropper(palkkasummaindeksi, c(1,4), c(1:3), c(340:382))

suppressMessages(
  
  tuotantosuhdanneindeksi <- readxl::read_excel("~/R/Shiny_demo/data/tuotantosuhdanne.xlsx")
  
)

tuotantosuhdanneindeksi <- data_cropper(tuotantosuhdanneindeksi, c(1,3), c(1:2), c(340:381))

suppressMessages(
  
  kuluttajahintaindeksi <- readxl::read_excel("~/R/Shiny_demo/data/kuluttajahintaindeksi.xlsx")
  
)

kuluttajahintaindeksi <- data_cropper(kuluttajahintaindeksi, c(1,3), c(1:2), c(221:254))

handoutdata <- as.data.frame(handoutdata)

BRC <- read.csv(file='~/R/Shiny_demo/data/BRC.csv')

shapefile <- rgdal::readOGR(dsn="~/R/NUTS_EURO_data", layer="NUTS_RG_20M_2021_3035")

##### DEFINE CHOICE, VALUE AND CONVERSION VECTORS #####

colnames(entryexitdata) <- c("vuosineljännes", "kategoria", "luokitus", "nentry", "nexit", "ntotal")

variableselection <- c("bankruptcies", "productivity estimates", "firm subsidies")

industrycharvec <- c('AFF', 'MIN', 'CON', 'MAN', 'TCE', 'WHO', 'RET','FIN', 'SER', 'PUB')

regioncharvec <- c("FI193", "FI194", "FI195", "FI196", "FI197", 
                   "FI1B1", 
                   "FI1D1", "FI1D2", "FI1D3", "FI1D5", "FI1D7", "FI1D8", "FI1D9", 
                   "FI1C1", "FI1C2", "FI1C3", "FI1C4", "FI1C5",
                   "FI200")

industrychoicevec <- c("Agriculture, Forestry, And Fishing" = "AFF",
                       "Mining" = "MIN",
                       "Construction" = "CON",
                       "Manufacturing"="MAN",
                       "Transportation, Communications, Electric, Gas, And Sanitary Services"="TCE",
                       "Wholesale Trade"="WHO",
                       "Retail Trade"="RET",
                       "Finance, Insurance, And Real Estate"="FIN",
                       "Services"="SER",
                       "Public Administration"="PUB")

regionchoicevec <- c("Ahvenanmaa" = "FI200",
                     "Keski-Suomi" = "FI193",
                     "Etelä-Pohjanmaa" ="FI194",
                     "Pohjanmaa" = "FI195",
                     "Satakunta" = "FI196",
                     "Pirkanmaa" = "FI197",
                     "Uusimaa" = "FI1B1",
                     "Lappi" = "FI1D7",
                     "Kainuu" = "FI1D8",
                     "Pohjois-Pohjanmaa" = "FI1D9",
                     "Varsinais-Suomi"= "FI1C1", 
                     "Kanta-Häme" = "FI1C2", 
                     "Päijät-Häme" = "FI1C3",
                     "Kymenlaakso" = "FI1C4",
                     "Etelä-Karjala" = "FI1C5", 
                     "Etelä-Savo" = "FI1D1",
                     "Pohjois-Savo" = "FI1D2",
                     "Pohjois-Karjala" = "FI1D3",
                     "Keski-Pohjanmaa" = "FI1D5") 

decomposedchoiceY <- c("decomposed" = "decY")
decomposedcharY <- c("decY")

latnamevec <- c("Keski-Suomi", "Etelä-Pohjanmaa", "Pohjanmaa", "Satakunta", "Pirkanmaa","Uusimaa","Etelä-Savo","Pohjois-Savo", "Pohjois-Karjala","Keski-Pohjanmaa", "Lappi", "Kainu", "Pohjois-Pohjanmaa", "Varsinais-Suomi","Kanta-Häme", "Päijät-Häme","Kymenlaakso","Etelä-Karjala","Ahvenanmaa")

regionchoicevec32 <- c("KOKO MAA","Keski-Suomi", "Etelä-Pohjanmaa", "Pohjanmaa", "Satakunta", "Pirkanmaa","Uusimaa","Etelä-Savo","Pohjois-Savo", "Pohjois-Karjala","Keski-Pohjanmaa", "Lappi", "Kainu", "Pohjois-Pohjanmaa", "Varsinais-Suomi","Kanta-Häme", "Päijät-Häme","Kymenlaakso","Etelä-Karjala","Ahvenanmaa")

industrynamevec <- c("Agriculture, Forestry, And Fishing","Mining","Construction","Manufacturing","Transportation, Communications, Electric, Gas, And Sanitary Services",
                     "Wholesale Trade","Retail Trade","Finance, Insurance, And Real Estate","Services","Public Administration")

regionlatnameconversion <- data.frame(name=latnamevec, char=regioncharvec)

quartalchoicevec <- c("2019/Q2"="2019Q2",
                      "2019/Q3"="2019Q3",
                      "2019/Q4"="2019Q4",
                      "2020/Q1"="2020Q1",
                      "2020/Q2"="2020Q2",
                      "2020/Q3"="2020Q3",
                      "2020/Q4"="2020Q4",
                      "2021/Q1"="2021Q1",
                      "2021/Q2"="2021Q2",
                      "2021/Q3"="2021Q3",
                      "2021/Q4"="2021Q4",
                      "2022/Q1"="2022Q1",
                      "2022/Q2"="2022Q2",
                      "2022/Q3"="2022Q3")

quartalcharvec <- c("2019Q2", "2019Q3", "2019Q4",
                    "2020Q1", "2020Q2", "2020Q3", "2020Q4",
                    "2021Q1", "2021Q2", "2021Q3", "2021Q4", 
                    "2022Q1", "2022Q2", "2022Q3")

investmentchoices31 <- c("Yhteensä","Kaivostoiminta ja louhinta", "Teollisuus", "Metsäteollisuus", 
                         "Kemianteollisuus","Metalliteollisuus", "Sähkö- ja elektroniikkateollisuus", 
                         "Sähkö-, kaasu- ja lämpöhuolto, jäähdytysliiketoiminta", 
                         "Vesihuolto, viemäri- ja jätevesihuolto, jätehuolto ja muu ympäristön puhtaanapito", 
                         "Rakentaminen", "Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus", 
                         "Palvelualat", "Kuljetus ja varastointi", "Informaatio ja viestintä")

yearchoicevec <- c("2013"="2013",
                   "2014"="2014",
                   "2015"="2015",
                   "2016"="2016",
                   "2017 (Huom! Dataa puuttuu)"="2017",
                   "2018"="2018",
                   "2019"="2019",
                   "2020"="2020",
                   "2021"="2021",
                   "2022"="2022")

yearcharvec <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

bigquartalcharvec <- c("2013Q1", "2013Q2", "2013Q3", "2013Q4",
                       "2014Q1", "2014Q2", "2014Q3", "2014Q4",
                       "2015Q1", "2015Q2", "2015Q3", "2015Q4",
                       "2016Q1", "2016Q2", "2016Q3", "2016Q4",
                       "2017Q1", "2017Q2", "2017Q3", "2017Q4",
                       "2018Q1", "2018Q2", "2018Q3", "2018Q4",
                       "2019Q1", "2019Q2", "2019Q3", "2019Q4",
                       "2020Q1", "2020Q2", "2020Q3", "2020Q4",
                       "2021Q1", "2021Q2", "2021Q3", "2021Q4", 
                       "2022Q1", "2022Q2", "2022Q3", "2022Q4")

TOLchoicevec <- c("Yhteensä" = "Yhteensä",
                  "A Maatalous, metsätalous ja kalatalous (01-03)" = "A",
                  "B Kaivostoiminta ja louhinta (05-09)" = "B",
                  "C Teollisuus (10-33)" = "C",
                  "D Sähkö-, kaasu- ja lämpöhuolto, jäähdytysliiketoiminta (35)" = "D",
                  "E Vesihuolto, viemäri- ja jätevesihuolto, jätehuolto ja muu ympäristön puhtaanapito (36-39)" = "E",
                  "F Rakentaminen (41-43)" = "F",
                  "G Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)" = "G",
                  "H Kuljetus ja varastointi (49-53)" = "H",
                  "I Majoitus- ja ravitsemistoiminta (55-56)" = "I",
                  "J Informaatio ja viestintä (58-63)" = "J",
                  "K Rahoitus- ja vakuutustoiminta (64-66)" = "K",
                  "L Kiinteistöalan toiminta (68)" = "L",
                  "M Ammatillinen, tieteellinen ja tekninen toiminta (69-75)" = "M",
                  "N Hallinto- ja tukipalvelutoiminta (77-82)" = "N",
                  "O Julkinen hallinto ja maanpuolustus; pakollinen sosiaalivakuutus (84)" = "O",
                  "P Koulutus (85)" = "P",
                  "Q Terveys- ja sosiaalipalvelut (86-88)" = "Q",
                  "R Taiteet, viihde ja virkistys (90-93)" = "R",
                  "S Muu palvelutoiminta (94-96)" = "S",
                  "T Kotitalouksien toiminta työnantajina; kotitalouksien eriyttämätön toiminta tavaroiden ja palvelujen tuottamiseksi omaan käyttöön (97-98)" = "T",
                  "U Kansainvälisten organisaatioiden ja toimielinten toiminta (99)" = "U",
                  "Tuntematon" = "X")

TOLcharvec <- c("Yhteensä",
                "A Maatalous, metsätalous ja kalatalous (01-03)",
                "B Kaivostoiminta ja louhinta (05-09)",
                "C Teollisuus (10-33)",
                "D Sähkö-, kaasu- ja lämpöhuolto, jäähdytysliiketoiminta (35)",
                "E Vesihuolto, viemäri- ja jätevesihuolto, jätehuolto ja muu ympäristön puhtaanapito (36-39)",
                "F Rakentaminen (41-43)",
                "G Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",
                "H Kuljetus ja varastointi (49-53)",
                "I Majoitus- ja ravitsemistoiminta (55-56)",
                "J Informaatio ja viestintä (58-63)",
                "K Rahoitus- ja vakuutustoiminta (64-66)",
                "L Kiinteistöalan toiminta (68)",
                "M Ammatillinen, tieteellinen ja tekninen toiminta (69-75)",
                "N Hallinto- ja tukipalvelutoiminta (77-82)",
                "O Julkinen hallinto ja maanpuolustus; pakollinen sosiaalivakuutus (84)",
                "P Koulutus (85)",
                "Q Terveys- ja sosiaalipalvelut (86-88)",
                "R Taiteet, viihde ja virkistys (90-93)",
                "S Muu palvelutoiminta (94-96)",
                "T Kotitalouksien toiminta työnantajina; kotitalouksien eriyttämätön toiminta tavaroiden ja palvelujen tuottamiseksi omaan käyttöön (97-98)",
                "U Kansainvälisten organisaatioiden ja toimielinten toiminta (99)",
                "Tuntematon")

variablechoicevec33 <- c("Tukea saaneiden yritysten lukumäärä" = "TUK",
                         "Maksetut suorat tuet" = "MST",
                         "Maksetut lainat" = "MAL",
                         "Myönnetyt takaukset" = "MYT",
                         "Suhdanneheikentymän vuoksi tukea saaneiden yritysten lukumäärä" = "STU",
                         "Suhdanneheikentymän vuoksi maksetut suorat tuet"="SST",
                         "Suhdanneheikentymän vuoksi maksetut lainat" = "SLA",
                         "Suhdanneheikentymän vuoksi myönnetyt takaukset"="STA")

variablechoicevec33 <- c("Tukea saaneiden yritysten lukumäärä",
                         "Maksetut suorat tuet" ,
                         "Maksetut lainat" ,
                         "Myönnetyt takaukset" ,
                         "Suhdanneheikentymän vuoksi tukea saaneiden yritysten lukumäärä",
                         "Suhdanneheikentymän vuoksi maksetut suorat tuet",
                         "Suhdanneheikentymän vuoksi maksetut lainat",
                         "Suhdanneheikentymän vuoksi myönnetyt takaukset")

TOLchoicevec33 <- c("Yhteensä" = "Yhteensä",
                  "Alkutuotanto (01-03)" = "A",
                  "Teollisuus (10-33)" = "B",
                  "Metsäteollisuus (16-17) = C",
                  "Metalliteollisuus (24-30, 33)" = "D",
                  "Muu teollisuus (10-15, 18-23, 31-32)" = "E",
                  "Rakentaminen (41-43)" = "F",
                  "Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)" = "G",
                  "H-N, R-S Palvelualat (49-82, 90-96)" = "H",
                  "Kuljetus ja varastointi (49-53)" = "I",
                  "Majoitus- ja ravitsemistoiminta (55-56)" = "J",
                  "Informaatio ja viestintä (58-63)" = "K",
                  "Ammatillinen, tieteellinen ja tekninen toiminta (69-75)" = "L",
                  "K, L, N, R, S Muut palvelualat (64-68, 77-82, 90-96)" = "M",
                  "Muut toimialat" = "N",
                  "Toimiala tuntematon" = "O",
                  "Ei yritystunnusta" = "P")

TOLcharvec33 <- c("Toimialat yhteensä",
                    "Alkutuotanto (01-03)",
                    "Teollisuus (10-33)",
                    "Metsäteollisuus (16-17)",
                    "Metalliteollisuus (24-30, 33)",
                    "Muu teollisuus (10-15, 18-23, 31-32)",
                    "Rakentaminen (41-43)",
                    "Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",
                    "H-N, R-S Palvelualat (49-82, 90-96)",
                    "Kuljetus ja varastointi (49-53)",
                    "Majoitus- ja ravitsemistoiminta (55-56)",
                    "Informaatio ja viestintä (58-63)",
                    "Ammatillinen, tieteellinen ja tekninen toiminta (69-75)",
                    "K, L, N, R, S Muut palvelualat (64-68, 77-82, 90-96)",
                    "Muut toimialat",
                    "Toimiala tuntematon",
                    "Ei yritystunnusta")

TOLchoicevec34 <- c("Kaikki","Koko teollisuus (B-E)" = "(B-E)", "Rakentaminen (F)" = "(F)", "Koko kauppa (G)" = "(G)", "Muut palvelut (HIJLMNRS)" = "(HIJLMNRS)")

variablechoicevec34 <- c("Kaikki","Alkuperäinen indeksisarja", "Työpäiväkorjattu indeksisarja", "Kausitasoitettu indeksisarja", "Trendisarja")

keywords = c("lama", "konkurssit", "yritystuet", "ansiosidonnainen")

plotvec <- c("FI1B1", "FI1C1", "FI196", "FI1C2", "FI197", "FI1C3", "FI1C4", "FI1C5", "FI1D1", "FI1D2", "FI1D3", "FI193", "FI194", "FI195", "FI1D5", "FI1D9", "FI1D8", "FI1D7", "FI200")

etusivu_url <- "yritysryhma"

bankruptcy_url <- paste0(etusivu_url, "/bankruptcies")

bankruptcy_histogram_url <- paste0(bankruptcy_url, "/histogram")
bankruptcy_time_series_graph_url <- paste0(bankruptcy_url, "/time_series_graph")
bankruptcy_time_series_bar_url <- paste0(bankruptcy_url, "/time_series_bar")
bankruptcy_map_url <- paste0(bankruptcy_url, "/map")
bankruptcy_pie_chart_url <- paste0(bankruptcy_url, "/pie_chart")

miscallenious_url <- paste0(etusivu_url, "/miscallenious")

miscallenious_investment_change_url <- paste0(miscallenious_url, "/investment_change")
miscallenious_entry_and_exit_url <- paste0(miscallenious_url, "/entry_and_exit")
miscallenious_subsidies_url <- paste0(miscallenious_url, "/subsidies")
miscallenious_revenue_url <- paste0(miscallenious_url, "/revenue")

productivity_url <- paste0(etusivu_url, "/productivity")

productivity_histogram_url <- paste0(productivity_url, "/histogram")
productivity_time_series_graph_url <- paste0(productivity_url, "/time_series_graph")
productivity_time_series_bar_url <- paste0(productivity_url, "/time_series_bar")
productivity_map_url <- paste0(productivity_url, "/map")
productivity_pie_chart_url <- paste0(productivity_url, "/pie_chart")

information_url <- paste0(etusivu_url, "/info")