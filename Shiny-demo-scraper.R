
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

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

finalvec

table(sizedata$vuosi)

uudetvuodet <- c(rep(2018, 525), rep(2019, 525), rep(2020, 525), rep(2021, 525))
koot <- c("Yhteensä","Mikro", "Pieni", "KSuuri", "Suuri")
finalkoot <- c()

for (i in 1:(2100/5)) {
  
  finalkoot <- append(finalkoot, koot)
  i = i+1
  
}

tollit <- unique(sizedata$TOL)
print(tollit)

tollit <- rep(tollit, each=5)
print(tollit)

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


suppressMessages(
  
  konkurssisarja <- readxl::read_excel("data/konkurssit.xlsx")
  
)

konkurssisarja <- konkurssisarja[-c(1:2),]
konkurssisarja <- konkurssisarja[-c(340:382),]
print(konkurssisarja)

colnames(konkurssisarja) <- c("time", "nobs")

konkurssisarja <- as.data.frame(konkurssisarja)

print(konkurssisarja)

konkurssisarja$time <- gsub('M', '', konkurssisarja$time)

konkurssisarja$time <- as.POSIXct(paste0(as.character(konkurssisarja$time), '01'), format='%Y%m%d')

for (i in 2:length(konkurssisarja[1,])) {
  konkurssisarja[,i] <- as.numeric(unlist(konkurssisarja[,i]))
}

konkurssisarja




suppressMessages(
  
  palkkasummaindeksi <- readxl::read_excel("data/palkkasummaindeksi.xlsx")
  
)

palkkasummaindeksi <- palkkasummaindeksi[-c(1:3),]
palkkasummaindeksi <- palkkasummaindeksi[-c(449:480),]
print(palkkasummaindeksi)

colnames(palkkasummaindeksi)[1] <- "time"

palkkasummaindeksi <- as.data.frame(palkkasummaindeksi)

print(palkkasummaindeksi)

palkkasummaindeksi$time <- gsub('M', '', palkkasummaindeksi$time)

palkkasummaindeksi$time <- as.POSIXct(paste0(as.character(palkkasummaindeksi$time), '01'), format='%Y%m%d')

for (i in 2:length(palkkasummaindeksi[1,])) {
  palkkasummaindeksi[,i] <- as.numeric(unlist(palkkasummaindeksi[,i]))
}

palkkasummaindeksi
