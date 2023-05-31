##### SORT mAPDATA #####

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
fakefinregions <- c()
for (i in 1:length(NUTS3narminds)) {
  fakefinregions[i] <- NUTS3narm[NUTS3narminds[i]]
}

BRC <- cbind(BRC, fakefinregions)

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
print(handoutdata)

propvec <- finalfinalfinaldf$nobs

##### SORT REVENUE DATA #####

vec34 <- c("(B-E)", "(F)", "(G)", "(HIJLMNRS)")

namerow <- revenuedata[3,]

for (i in 1:16) {
  namerow[1+i] <- paste(namerow[1+i], vec34[ceiling(i/4)])
}

namerow[1] <- "Kuukausi"
print(namerow)

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

print(indvec)

remvec <- c(1:length(scrapedtable[,1]))

remvec <- remvec[! remvec %in% indvec]

print(remvec)

scrapedtable <- scrapedtable[remvec,]

print(scrapedtable)

MUUT <- c("Muut", MUUT)

scrapedtable[nrow(scrapedtable) + 1,] <- MUUT

scrapedtable[nrow(scrapedtable) + 1,] <- scrapedtable[nrow(scrapedtable) - 1,]

scrapedtable <- scrapedtable[ -(nrow(scrapedtable) - 2),]


##### FUNCTIONS #####

dark_green <- "#234721"
light_green <- "#AED136"
dark_blue <- "#393594"
light_blue <- "#8482BD"
dark_red <- "#721D41"
light_red <- "#CC8EA0"
yellow <- "#FBE802"
orange <- "#F16C13"
light_orange <- "#FFF1E0"

DHcolors <- c(dark_green, light_green, dark_blue, light_blue, dark_red, light_red, yellow, orange, light_orange, dark_green)

DHcolors2 <- c(dark_red, light_red, yellow, orange)

DHcolors3 <- c(light_red, yellow, orange, light_orange)

show_colors <- function(alpha = 1) {
  tibble(colors) %>% 
    ggplot()  +
    geom_treemap(aes(area = 1, fill = colors), alpha = alpha) +
    scale_fill_manual(values = colors)
}

#show_colors()

