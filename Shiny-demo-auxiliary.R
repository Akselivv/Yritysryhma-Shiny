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
library(tidyr)
library(tidyselect)
library(plyr)

##### SET OPTIONS #####

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

options(scipen=999) #Disable scientific notation

##### SCRAPE DATA #####

scrapeindicator <- FALSE
saveindicator <- FALSE
readindicator <- TRUE

if (scrapeindicator == TRUE) {

  pg1 <- rvest::read_html("https://www.prh.fi/fi/kaupparekisteri/yritystenlkm/lkm.html")

  tables <- pg1 %>% rvest::html_table(fill = TRUE)

  table <- tables[1]

  scrapedtable <- as.data.frame(table)

}

if (saveindicator == TRUE) {

  writexl::write_xlsx(scrapedtable,"data/scrapedtable.xlsx")

}

if (readindicator == TRUE) {

  suppressMessages(
    scrapedtable <- readxl::read_excel("data/scrapedtable.xlsx")
  )

  scrapedtable <- as.data.frame(scrapedtable)

}

scrapedtable[length(scrapedtable$Yritysmuoto),2]

#### WRANGLE DATA ON FIRM SIZE DISTRIBUTION #####

suppressMessages(

  sizedata <- readxl::read_excel('data/001_13w1_2021_20230522-151740.xlsx')

)

sizedata <- sizedata[-c(1:2),]
sizedata <- sizedata[-c(4201:4235),]

colnames(sizedata) <- c("vuosi", "TOL", "kokoluokka", "lkm")
sizedata$vuosi <- zoo::na.locf(sizedata$vuosi, option="locf")
sizedata$TOL <- zoo::na.locf(sizedata$TOL, option="locf")
sizedata$lkm <- as.numeric(sizedata$lkm)

sizedata$lkm[is.na(sizedata$lkm)] <- 0

finalvec <- c()
tempvec <- c(0,0,0,0,0)

for (i in 0:((length(sizedata$lkm)/10)-1)) {
  tempvec[1] <- sizedata$lkm[(i*10)+1]
  tempvec[2] <- sizedata$lkm[(i*10)+2] + sizedata$lkm[(i*10)+3]
  tempvec[3] <- sizedata$lkm[(i*10)+4] + sizedata$lkm[(i*10)+5]
  tempvec[4] <- sizedata$lkm[(i*10)+6] + sizedata$lkm[(i*10)+7]
  tempvec[5] <- sizedata$lkm[(i*10)+8] + sizedata$lkm[(i*10)+9] + sizedata$lkm[(i*10)+10]
  finalvec <- append(finalvec, tempvec)
}

uudetvuodet <- c(rep(2018, 525), rep(2019, 525), rep(2020, 525), rep(2021, 525))
koot <- c("Yhteensä","Mikro", "Pieni", "KSuuri", "Suuri")
finalkoot <- c()

for (i in 1:(2100/5)) {

  finalkoot <- append(finalkoot, koot)
  i = i+1

}

tollit <- unique(sizedata$TOL)

tollit <- rep(tollit, each=5)

finaltollit <- c()

for (i in 1:length(unique(sizedata$vuosi))) {

  finaltollit <- append(finaltollit, tollit)

}

finaldf <- data.frame(vuosi = uudetvuodet, nobs = finalvec, kokoluokka = finalkoot, TOL = finaltollit)
finaldf <- finaldf[finaldf$kokoluokka != "Yhteensä",]
finaldf <- finaldf[finaldf$vuosi != "2018",]

##################################################################################################

metsavec <- finaldf$TOL[as.numeric(substr(finaldf$TOL, 1, 2)) %in% c(16:17)]

metsavec <- unique(metsavec)

metsadf <- finaldf[finaldf$TOL %in% metsavec,]

metsadf <- aggregate(nobs~vuosi+kokoluokka, data=metsadf, FUN=sum)
metsadf
metsadf <- arrange(metsadf,vuosi, -nobs)
metsadf
newnames <- rep("Metsäteollisuus (16-17)", 4*3)
metsadf <- data.frame(vuosi=metsadf$vuosi, nobs = metsadf$nobs, kokoluokka=metsadf$kokoluokka, TOL=newnames)
colnames(metsadf) <- c("vuosi", "nobs", "kokoluokka", "TOL")

###################################################################################################

metallivec <- finaldf$TOL[as.numeric(substr(finaldf$TOL, 1, 2)) %in% c(24:30, 33)]

metallivec <- unique(metallivec)

metallidf <- finaldf[finaldf$TOL %in% metallivec,]

metallidf <- aggregate(nobs~vuosi+kokoluokka, data=metallidf, FUN=sum)
metallidf
metallidf <- arrange(metallidf,vuosi, -nobs)
metallidf
newnames <- rep("Metalliteollisuus (24-30, 33)", 4*3)
metallidf <- data.frame(vuosi=metallidf$vuosi, nobs = metallidf$nobs, kokoluokka=metallidf$kokoluokka, TOL=newnames)
colnames(metallidf) <- c("vuosi", "nobs", "kokoluokka", "TOL")

###################################################################################################

teollisuusvec <- finaldf$TOL[as.numeric(substr(finaldf$TOL, 1, 2)) %in% c(10:15, 18:23, 31, 32)]

teollisuusvec <- unique(teollisuusvec)

teollisuusdf <- finaldf[finaldf$TOL %in% teollisuusvec,]

teollisuusdf <- aggregate(nobs~vuosi+kokoluokka, data=teollisuusdf, FUN=sum)
teollisuusdf
teollisuusdf <- arrange(teollisuusdf,vuosi, -nobs)
teollisuusdf
newnames <- rep("Muu teollisuus (10-15, 18-23, 31-32)", 4*3)
teollisuusdf <- data.frame(vuosi=teollisuusdf$vuosi, nobs = teollisuusdf$nobs, kokoluokka=teollisuusdf$kokoluokka, TOL=newnames)
colnames(teollisuusdf) <- c("vuosi", "nobs", "kokoluokka", "TOL")

###################################################################################################

palveluvec <- finaldf$TOL[as.numeric(substr(finaldf$TOL, 1, 2)) %in% c(49:82, 90:96)]

palveluvec <- unique(palveluvec)

palveludf <- finaldf[finaldf$TOL %in% palveluvec,]

palveludf <- aggregate(nobs~vuosi+kokoluokka, data=palveludf, FUN=sum)
palveludf
palveludf <- arrange(palveludf,vuosi, -nobs)
palveludf
newnames <- rep("H-N, R-S Palvelualat (49-82, 90-96)", 4*3)
palveludf <- data.frame(vuosi=palveludf$vuosi, nobs = palveludf$nobs, kokoluokka=palveludf$kokoluokka, TOL=newnames)
colnames(palveludf) <- c("vuosi", "nobs", "kokoluokka", "TOL")

###################################################################################################

muupalveluvec <- finaldf$TOL[as.numeric(substr(finaldf$TOL, 1, 2)) %in% c(64:68, 77:82, 90:96)]

muupalveluvec <- unique(muupalveluvec)

muupalveludf <- finaldf[finaldf$TOL %in% muupalveluvec,]

muupalveludf <- aggregate(nobs~vuosi+kokoluokka, data=muupalveludf, FUN=sum)
muupalveludf
muupalveludf <- arrange(muupalveludf,vuosi, -nobs)
muupalveludf
newnames <- rep("K, L, N, R, S Muut palvelualat (64-68, 77-82, 90-96)", 4*3)
muupalveludf <- data.frame(vuosi=muupalveludf$vuosi, nobs = muupalveludf$nobs, kokoluokka=muupalveludf$kokoluokka, TOL=newnames)
colnames(muupalveludf) <- c("vuosi", "nobs", "kokoluokka", "TOL")

finalfinaldf <-NA

finalfinaldf <- rbind(finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="Yhteensä",], finaldf[finaldf$vuosi=="2019" &  finaldf$TOL=="A Maatalous, metsätalous ja kalatalous (01-03)",],
                      finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="C Teollisuus (10-33)",], metsadf[metsadf$vuosi=="2019",],
                      metallidf[metallidf$vuosi=="2019",], teollisuusdf[teollisuusdf$vuosi=="2019",],
                      finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="F Rakentaminen (41-43)",], finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="G Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",],
                      palveludf[palveludf$vuosi=="2019",], finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="H Kuljetus ja varastointi (49-53)",],
                      finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="I Majoitus- ja ravitsemistoiminta (55-56)",],
                      finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="J Informaatio ja viestintä (58-63)",], finaldf[finaldf$vuosi=="2019" & finaldf$TOL=="M Ammatillinen, tieteellinen ja tekninen toiminta (69-75)",],
                      muupalveludf[muupalveludf$vuosi=="2019",],finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="Yhteensä",], finaldf[finaldf$vuosi=="2020" &  finaldf$TOL=="A Maatalous, metsätalous ja kalatalous (01-03)",],
                      finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="C Teollisuus (10-33)",], metsadf[metsadf$vuosi=="2020",],
                      metallidf[metallidf$vuosi=="2020",], teollisuusdf[teollisuusdf$vuosi=="2020",],
                      finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="F Rakentaminen (41-43)",], finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="G Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",],
                      palveludf[palveludf$vuosi=="2020",],
                      finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="H Kuljetus ja varastointi (49-53)",],
                      finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="I Majoitus- ja ravitsemistoiminta (55-56)",],
                      finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="J Informaatio ja viestintä (58-63)",], finaldf[finaldf$vuosi=="2020" & finaldf$TOL=="M Ammatillinen, tieteellinen ja tekninen toiminta (69-75)",],
                      muupalveludf[muupalveludf$vuosi=="2020",],finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="Yhteensä",], finaldf[finaldf$vuosi=="2021" &  finaldf$TOL=="A Maatalous, metsätalous ja kalatalous (01-03)",],
                      finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="C Teollisuus (10-33)",], metsadf[metsadf$vuosi=="2021",],
                      metallidf[metallidf$vuosi=="2021",], teollisuusdf[teollisuusdf$vuosi=="2021",],
                      finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="F Rakentaminen (41-43)",], finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="G Tukku- ja vähittäiskauppa; moottoriajoneuvojen ja moottoripyörien korjaus (45-47)",],
                      palveludf[palveludf$vuosi=="2021",],
                      finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="H Kuljetus ja varastointi (49-53)",],
                      finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="I Majoitus- ja ravitsemistoiminta (55-56)",],
                      finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="J Informaatio ja viestintä (58-63)",], finaldf[finaldf$vuosi=="2021" & finaldf$TOL=="M Ammatillinen, tieteellinen ja tekninen toiminta (69-75)",],
                      muupalveludf[muupalveludf$vuosi=="2021",])

finalfinaldf <- data.frame(vuosi=finalfinaldf$vuosi, nobs = finalfinaldf$nobs, kokoluokka=finalfinaldf$kokoluokka, TOL=finalfinaldf$TOL)

finalfinalfinaldf <- rbind(finalfinaldf[finalfinaldf$vuosi=="2019",],finalfinaldf[finalfinaldf$vuosi=="2019",],finalfinaldf[finalfinaldf$vuosi=="2019",],finalfinaldf[finalfinaldf$vuosi=="2019",],
                           finalfinaldf[finalfinaldf$vuosi=="2020",],finalfinaldf[finalfinaldf$vuosi=="2020",],finalfinaldf[finalfinaldf$vuosi=="2020",],finalfinaldf[finalfinaldf$vuosi=="2020",],
                           finalfinaldf[finalfinaldf$vuosi=="2021",],finalfinaldf[finalfinaldf$vuosi=="2021",],finalfinaldf[finalfinaldf$vuosi=="2021",],finalfinaldf[finalfinaldf$vuosi=="2021",])

vuosineljännesvec <- rep(c("2019Q1", "2019Q2", "2019Q3", "2019Q4", "2020Q1", "2020Q2",
                        "2020Q3", "2020Q4", "2021Q1", "2021Q2", "2021Q3", "2021Q4"), each=56)

finalfinalfinaldf <- data.frame(vuosi=finalfinalfinaldf$vuosi, vuosineljännes = vuosineljännesvec, nobs = finalfinalfinaldf$nobs, kokoluokka=finalfinalfinaldf$kokoluokka, TOL=finalfinalfinaldf$TOL)

##### READ DATA #####

#suppressMessages(

#  konkurssisarja <- readxl::read_excel("data/konkurssit.xlsx")

#)

#konkurssisarja <- konkurssisarja[-c(1:2),]
#konkurssisarja <- konkurssisarja[-c(340:382),]

#colnames(konkurssisarja) <- c("time", "nobs")

#konkurssisarja <- as.data.frame(konkurssisarja)

#konkurssisarja$time <- gsub('M', '', konkurssisarja$time)

#konkurssisarja$time <- as.POSIXct(paste0(as.character(konkurssisarja$time), '01'), format='%Y%m%d')

#for (i in 2:length(konkurssisarja[1,])) {
#  konkurssisarja[,i] <- as.numeric(unlist(konkurssisarja[,i]))
#}

#suppressMessages(

#  palkkasummaindeksi <- readxl::read_excel("data/palkkasummaindeksi.xlsx")

#)

#palkkasummaindeksi <- palkkasummaindeksi[-c(1:3),]
#palkkasummaindeksi <- palkkasummaindeksi[-c(449:480),]

#colnames(palkkasummaindeksi)[1] <- "time"

#palkkasummaindeksi <- as.data.frame(palkkasummaindeksi)

#palkkasummaindeksi$time <- gsub('M', '', palkkasummaindeksi$time)

#palkkasummaindeksi$time <- as.POSIXct(paste0(as.character(palkkasummaindeksi$time), '01'), format='%Y%m%d')

#for (i in 2:length(palkkasummaindeksi[1,])) {
#  palkkasummaindeksi[,i] <- as.numeric(unlist(palkkasummaindeksi[,i]))
#}

suppressMessages(

  investmentchangedata <- readxl::read_excel("data/001_12r6_2022q4_20230511-092217.xlsx")

)

suppressMessages(

  entryexitdata <- readxl::read_excel("data/001_11yq_2022q4_20230511-122745.xlsx")

)

suppressMessages(

  handoutdata <- readxl::read_excel('data/001_12s5_2021q4_20230515-084538.xlsx')

)

suppressMessages(

  revenuedata <- readxl::read_excel("data/001_123y_2023m03_20230516-115325.xlsx")

)

suppressMessages(

  konkurssisarja <- readxl::read_excel("data/konkurssit.xlsx")

)

colnames(konkurssisarja)[2] <- "konkursseja"

konkursseja <- konkurssisarja[,2]

konkurssisarja <- data_cropper(konkurssisarja, c(1:2), c(1:2), c(449:480))

suppressMessages(

  palkkasummaindeksi <- readxl::read_excel("data/palkkasummaindeksi.xlsx")

)

palkkasummaindeksi <- data_cropper(palkkasummaindeksi, c(1,4), c(1:3), c(340:382))

suppressMessages(

  tuotantosuhdanneindeksi <- readxl::read_excel("data/tuotantosuhdanne.xlsx")

)

tuotantosuhdanneindeksi <- data_cropper(tuotantosuhdanneindeksi, c(1,3), c(1:2), c(340:381))

suppressMessages(

  kuluttajahintaindeksi <- readxl::read_excel("data/kuluttajahintaindeksi.xlsx")

)

kuluttajahintaindeksi <- data_cropper(kuluttajahintaindeksi, c(1,3), c(1:2), c(221:254))

handoutdata <- as.data.frame(handoutdata)

suppressMessages(

  BRCfin <- readxl::read_excel("data/BRCsuomi.xlsx")

)

suppressMessages(

  kunnatmaakunnat <- read.csv("data/kunnat_maakunnat_avain.csv")

)

suppressMessages(

  kunnattuottavuus <- read.csv("data/kunnattuottavuus.csv")

)

BRC <- read.csv(file='data/BRC.csv')

shapefile <- rgdal::readOGR(dsn="data/NUTS_EURO_data", layer="NUTS_RG_20M_2021_3035")

##### DEFINE CHOICE, VALUE AND CONVERSION VECTORS #####

plotvec <- c("FI1B1", "FI1C1", "FI196", "FI1C2", "FI197", "FI1C3", "FI1C4", "FI1C5", "FI1D1",
             "FI1D2", "FI1D3", "FI193", "FI194", "FI195", "FI1D5", "FI1D9", "FI1D8", "FI1D7", "FI200")

plotvec2 <- c("FI200", "FI193", "FI194", "FI195", "FI196", "FI197", "FI1B1", "FI1D7", "FI1D8", "FI1D9", "FI1C1",
              "FI1C2","FI1C3", "FI1C4", "FI1C5", "FI1D3", "FI1D2", "FI1D1", "FI1D5")

colnames(entryexitdata) <- c("vuosineljännes", "kategoria", "luokitus", "aloittaneet", "lopettaneet", "ntotal")

variableselection <- c("bankruptcies", "productivity estimates", "firm subsidies")

industrycharvec <- c('AFF', 'MIN', 'CON', 'MAN', 'TCE', 'WHO', 'RET','FIN', 'SER', 'PUB')

regioncharvec <- c("FI193", "FI194", "FI195", "FI196", "FI197",
                   "FI1B1",
                   "FI1D1", "FI1D2", "FI1D3", "FI1D5", "FI1D7", "FI1D8", "FI1D9",
                   "FI1C1", "FI1C2", "FI1C3", "FI1C4", "FI1C5",
                   "FI200")

industrychoicevec <- c("Maa-, metsä ja kalatalous",
                       "Teollisuus, kaivostoiminta sekä energia- ja vesihuolto",
                       "Rakennustoiminta",
                       "Kauppa",
                       "Kuljetus ja varastointi",
                       "Majoitus- ja ravitsemistoiminta",
                       "Muut palvelut",
                       "Tuntematon")

#regionchoicevec <- c("MK21 Ahvenanmaa" = "FI200",
#                     "MK13 Keski-Suomi" = "FI193",
#                     "MK14 Etelä-Pohjanmaa" ="FI194",
#                     "MK15 Pohjanmaa" = "FI195",
#                     "MK04 Satakunta" = "FI196",
#                     "MK06 Pirkanmaa" = "FI197",
#                     "MK01 Uusimaa" = "FI1B1",
#                     "MK20 Lappi" = "FI1D7",
#                     "MK18 Kainuu" = "FI1D8",
#                     "MK17 Pohjois-Pohjanmaa" = "FI1D9",
#                     "MK02 Varsinais-Suomi"= "FI1C1",
#                     "MK05 Kanta-Häme" = "FI1C2",
#                     "MK07 Päijät-Häme" = "FI1C3",
#                     "MK08 Kymenlaakso" = "FI1C4",
#                     "MK09 Etelä-Karjala" = "FI1C5",
#                     "MK10 Etelä-Savo" = "FI1D1",
#                     "MK11 Pohjois-Savo" = "FI1D2",
#                     "MK12 Pohjois-Karjala" = "FI1D3",
#                     "MK16 Keski-Pohjanmaa" = "FI1D5")

regionchoicevec <- c("Ahvenanmaa" = "MK21 Ahvenanmaa",
                     "Keski-Suomi" = "MK13 Keski-Suomi",
                     "Etelä-Pohjanmaa" = "MK14 Etelä-Pohjanmaa",
                     "Pohjanmaa" = "MK15 Pohjanmaa",
                     "Satakunta" = "MK04 Satakunta",
                     "Pirkanmaa" = "MK06 Pirkanmaa",
                     "Uusimaa" = "MK01 Uusimaa",
                     "Lappi" = "MK19 Lappi",
                     "Kainuu" = "MK18 Kainuu",
                     "Pohjois-Pohjanmaa" = "MK17 Pohjois-Pohjanmaa",
                     "Varsinais-Suomi" = "MK02 Varsinais-Suomi",
                     "Kanta-Häme" = "MK05 Kanta-Häme",
                     "Päijät-Häme" = "MK07 Päijät-Häme",
                     "Kymenlaakso" = "MK08 Kymenlaakso",
                     "Etelä-Karjala" = "MK09 Etelä-Karjala",
                     "Etelä-Savo" = "MK10 Etelä-Savo",
                     "Pohjois-Savo" = "MK11 Pohjois-Savo",
                     "Pohjois-Karjala" = "MK12 Pohjois-Karjala",
                     "Keski-Pohjanmaa" = "MK16 Keski-Pohjanmaa")

regionchoicevectemp <- c("MK21 Ahvenanmaa",
                         "MK13 Keski-Suomi",
                         "MK14 Etelä-Pohjanmaa",
                         "MK15 Pohjanmaa",
                         "MK04 Satakunta",
                         "MK06 Pirkanmaa",
                         "MK01 Uusimaa",
                         "MK19 Lappi",
                         "MK18 Kainuu",
                         "MK17 Pohjois-Pohjanmaa",
                         "MK02 Varsinais-Suomi",
                         "MK05 Kanta-Häme",
                         "MK07 Päijät-Häme",
                         "MK08 Kymenlaakso",
                         "MK09 Etelä-Karjala",
                         "MK10 Etelä-Savo",
                         "MK11 Pohjois-Savo",
                         "MK12 Pohjois-Karjala",
                         "MK16 Keski-Pohjanmaa")

regionchoicevectemp2 <- c("Ahvenanmaa",
                          "Keski-Suomi",
                          "Etelä-Pohjanmaa",
                          "Pohjanmaa",
                          "Satakunta",
                          "Pirkanmaa",
                          "Uusimaa",
                          "Lappi",
                          "Kainuu",
                          "Pohjois-Pohjanmaa",
                          "Varsinais-Suomi",
                          "Kanta-Häme",
                          "Päijät-Häme",
                          "Kymenlaakso",
                          "Etelä-Karjala",
                          "Etelä-Savo",
                          "Pohjois-Savo",
                          "Pohjois-Karjala",
                          "Keski-Pohjanmaa")

BRCfinsubs <- data.frame(maakunta = regionchoicevectemp, nutsname = plotvec2)

BRCfinsubs2 <- data.frame(maakunta = regionchoicevectemp2, nutsname = plotvec2)

BRCfinzeros <- data.frame(maakunta = regionchoicevectemp, nutsname = plotvec2, nobs = numeric(length(regionchoicevectemp)))

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

##### DEFINE URLS #####

# Currently not in use/redundant

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

##### SORT MAPDATA #####

shapefile <- shapefile[(substr(shapefile$NUTS_ID, start=1,stop=2) == "FI"|substr(shapefile$NUTS_ID, start=1,stop=2) == "SE"
                        |substr(shapefile$NUTS_ID, start=1,stop=2) == "DK"|substr(shapefile$NUTS_ID, start=1,stop=2) == "NO"),]

shapefile <-shapefile[(substr(shapefile$NUTS_ID, start=1,stop=2) != "NO" & substr(shapefile$NUTS_ID, start=1,stop=2) != "SE"&
                         substr(shapefile$NUTS_ID, start=1,stop=2) != "DK"),] #Drop all countries except Finland

mapdata <- broom::tidy(shapefile)

mydata <- data.frame(id=unique(mapdata$id), nutsname=shapefile$NUTS_ID)

uniqueids <- unique(mapdata$id)
latmids <- c()
longmids <- c()

for (i in 1:length(uniqueids)) {
  latmids[i] <- mean(mapdata$lat[mapdata$id == uniqueids[i]])
  longmids[i] <-mean(mapdata$long[mapdata$id == uniqueids[i]])
}

coordinates <- data.frame(
  NUTS3 = c(NA, NA, NA, NA, NA, NA, NA, NA,
            "FI193","FI194", "FI195", "FI196", "FI197",
            "FI1B1",
            "FI1C1", "FI1C2", "FI1C3", "FI1C4", "FI1C5",
            "FI1D1", "FI1D2", "FI1D3", "FI1D5", "FI1D7", "FI1D8", "FI1D9" ,
            "FI200"),
  name = shapefile$NAME_LATN,
  meanlat = latmids,
  meanlong = longmids,
  id = mydata$id)

NUTS3narm <- na.omit(coordinates$NUTS3)
NUTS3narminds <- round(runif(n=length(BRC$AssetsBefore), min=1, max=length(NUTS3narm)), 0)

##### SORT BANKRUPTCY DATA #####

x1 = 1:9
x2=10:14
x3=15:17
x4=20:39
x5=40:49
x6=50:51
x7=52:59
x8=60:67
x9=70:89
x10=91:99

max.len = max(length(x1),length(x2),length(x3),length(x4),length(x5),length(x6),length(x7),length(x8),length(x9),length(x10))

x1 = c(x1, rep(NA, max.len - length(x1)))
x2 = c(x2, rep(NA, max.len - length(x2)))
x3 = c(x3, rep(NA, max.len - length(x3)))
x4 = c(x4, rep(NA, max.len - length(x4)))
x5 = c(x5, rep(NA, max.len - length(x5)))
x6 = c(x6, rep(NA, max.len - length(x6)))
x7 = c(x7, rep(NA, max.len - length(x7)))
x8 = c(x8, rep(NA, max.len - length(x8)))
x9 = c(x9, rep(NA, max.len - length(x9)))
x10 = c(x10, rep(NA, max.len - length(x10)))

newdf <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

regionlatnameconversion <- data.frame(name=latnamevec, nutsname=regioncharvec)
regionlatnameconversion <- join(regionlatnameconversion, mydata, by="nutsname")

industry <- numeric(length(BRC$SICPrimary))
industryabbr <- numeric(length(BRC$SICPrimary))

BRC <- cbind(BRC, industry)
BRC <- cbind(BRC, industryabbr)

for (i in 1:length(BRC$SICPrimary)) {
  if (is.element(floor(BRC$SICPrimary[i]/100), x1)) {
    BRC$industry[i] <- "Agriculture, Forestry, And Fishing"
    BRC$industryabbr[i] <- "AFF"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x2)) {
    BRC$industry[i] <- "Mining"
    BRC$industryabbr[i] <- "MIN"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x3)) {
    BRC$industry[i] <- "Construction"
    BRC$industryabbr[i] <- "CON"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x4)) {
    BRC$industry[i] <- "Manufacturing"
    BRC$industryabbr[i] <- "MAN"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x5)) {
    BRC$industry[i] <- "Transportation, Communications, Electric, Gas, And Sanitary Services"
    BRC$industryabbr[i] <- "TCE"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x6)) {
    BRC$industry[i] <- "Wholesale Trade"
    BRC$industryabbr[i] <- "WHO"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x7)) {
    BRC$industry[i] <- "Retail Trade"
    BRC$industryabbr[i] <- "RET"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x8)) {
    BRC$industry[i] <- "Finance, Insurance, And Real Estate"
    BRC$industryabbr[i] <- "FIN"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x9)) {
    BRC$industry[i] <- "Services"
    BRC$industryabbr[i] <- "SER"
  } else if (is.element(floor(BRC$SICPrimary[i]/100), x10)) {
    BRC$industry[i] <- "Public Administration"
    BRC$industryabbr[i] <- "PUB"
  }
}

###### SORT BANKRUPTCY DATA #####

BRCfin <- BRCfin[-(1:2),]
BRCfin <- BRCfin[-(48313:48347),]

colnames(BRCfin) <- c("date", "TOL", "maakunta", "nobs")

BRCfin$date <- zoo::na.locf(BRCfin$date)

BRCfin$TOL <- zoo::na.locf(BRCfin$TOL)

BRCfin$nobs <- as.numeric(BRCfin$nobs)

##### SORT ENTRY & EXIT DATA #####

##### SORT SUBSIDY DATA #####

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

propvec <- finalfinalfinaldf$nobs

##### SORT REVENUE DATA #####

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

##### SORT FIRM DISTRIBITION DATA #####

yrityslkmvec <-c()

len <- length(scrapedtable[2,])-1

for (i in 1:len) {
  yrityslkmvec[i] <- scrapedtable[length(scrapedtable$Yritysmuoto), i+1]
}

namesvec <- colnames(scrapedtable)

for (i in 2:length(scrapedtable[2,])) {
  namesvec[i] <- substr(namesvec[i], 1, 9)
}

colnames(scrapedtable) <- namesvec

for (i in 1:length(scrapedtable[,1])) {
  for (k in 2:length(scrapedtable[1,])) {
    scrapedtable[i,k] <- gsub(" ", "", scrapedtable[i,k])
  }
}


for (i in 2:length(scrapedtable[1,])) {
  scrapedtable[,i] <- as.numeric(unlist(scrapedtable[,i]))
}

MUUT <- c(0, 0, 0, 0, 0)
indvec <- c()

for (i in 1:length(scrapedtable[,1])) {
  if (sum(scrapedtable[i,(2:6)]) < 1000) {
    MUUT = MUUT + scrapedtable[i,(2:6)]
    indvec <- append(indvec, i)
  }
}

remvec <- c(1:length(scrapedtable[,1]))

remvec <- remvec[! remvec %in% indvec]

scrapedtable <- scrapedtable[remvec,]

MUUT <- c("Muut", MUUT)

scrapedtable[nrow(scrapedtable) + 1,] <- MUUT

scrapedtable[nrow(scrapedtable) + 1,] <- scrapedtable[nrow(scrapedtable) - 1,]

scrapedtable <- scrapedtable[ -(nrow(scrapedtable) - 2),]

##### Sort productivity data #####

#print(kunnatmaakunnat)
#print(kunnattuottavuus)

kunnatmaakunnat <- read.csv2("data/kunnat_maakunnat_avain.csv")

kunnatmaakunnat <- kunnatmaakunnat[,c(1, 4)]
kunnatmaakunnat <- kunnatmaakunnat[-c(1),]

colnames(kunnatmaakunnat) <- c("kunta", "maakunnat")

kunnatmaakunnat$kunta <- gsub("'", "", kunnatmaakunnat$kunta)
kunnatmaakunnat$maakunnat <- gsub("\xe4", "ä", kunnatmaakunnat$maakunnat)


colnames(kunnattuottavuus) <- c("nro","kunta", "vuosi", "prod1", "nobs")

kunnatmaakunnat[,1] <- substr(kunnatmaakunnat[,1], 1, 3)

productivity <- join(kunnattuottavuus, kunnatmaakunnat, by="kunta")
productivity <- na.omit(productivity)

aggregateproddf <- data.frame(prod=numeric(length(unique(productivity$maakunnat))*length(unique(productivity$vuosi))),
                              maakunta = numeric(length(unique(productivity$maakunnat))*length(unique(productivity$vuosi))),
                              vuosi = numeric(length(unique(productivity$maakunnat))*length(unique(productivity$vuosi))))

for (i in 1:length(unique(productivity$vuosi))) {
  for (k in 1:length(unique(productivity$maakunnat))) {
    aggregateproddf$vuosi[k+(i-1)*length(unique(productivity$maakunnat))] <- unique(productivity$vuosi)[i]
    aggregateproddf$maakunta[k+(i-1)*length(unique(productivity$maakunnat))] <- unique(productivity$maakunnat)[k]
    aggregateproddf$prod[k+(i-1)*length(unique(productivity$maakunnat))] <- sum(productivity$prod1[productivity$vuosi == unique(productivity$vuosi)[i]
                                                                                                   & productivity$maakunnat == unique(productivity$maakunnat)[k]]*productivity$nobs[productivity$vuosi == unique(productivity$vuosi)[i]
                                                                                                                                                                                    & productivity$maakunnat == unique(productivity$maakunnat)[k]], na.omit=TRUE)/(sum(productivity$nobs[productivity$vuosi == unique(productivity$vuosi)[i]
                                                                                                                                                                                                                                                                                         & productivity$maakunnat == unique(productivity$maakunnat)[k]], na.omit=TRUE))
  }
}

##### COLORS #####

dark_green <- "#234721"
light_green <- "#AED136"
dark_blue <- "#393594"
light_blue <- "#8482BD"
dark_red <- "#721D41"
light_red <- "#CC8EA0"
yellow <- "#FBE802"
orange <- "#F16C13"
light_orange <- "#FFF1E0"

DHcolors <- c(dark_green, light_green, dark_blue, light_blue, dark_red, light_red, yellow, orange, light_orange)

DHcolors2 <- c(dark_red, light_red, yellow, orange)

DHcolors3 <- c(light_red, yellow, orange, light_orange)
