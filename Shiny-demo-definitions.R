BRC <- read.csv(file= 'data/BRC.csv')

shapefile <- readOGR(dsn="~/R/NUTS_EURO_data", layer="NUTS_RG_20M_2021_3035")
shapefile <- shapefile[(substr(shapefile$NUTS_ID, start=1,stop=2) == "FI"|substr(shapefile$NUTS_ID, start=1,stop=2) == "SE"
                        |substr(shapefile$NUTS_ID, start=1,stop=2) == "DK"|substr(shapefile$NUTS_ID, start=1,stop=2) == "NO"),]

shapefile <-shapefile[(substr(shapefile$NUTS_ID, start=1,stop=2) != "NO" & substr(shapefile$NUTS_ID, start=1,stop=2) != "SE"&
                         substr(shapefile$NUTS_ID, start=1,stop=2) != "DK"),] #Drop all countries except from Finland

#print(shapefile$LEVL_CODE)

mapdata <- tidy(shapefile)

##### REPLACE WITH ACTUAL DATA #####
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

latnamevec <- c("Keski-Suomi", "Etelä-Pohjanmaa", "Pohjanmaa", "Satakunta", "Pirkanmaa","Uusimaa","Etelä-Savo","Pohjois-Savo", "Pohjois-Karjala","Keski-Pohjanmaa", "Lappi", "Kainuu", "Pohjois-Pohjanmaa", "Varsinais-Suomi","Kanta-Häme", "Päijät-Häme","Kymenlaakso","Etelä-Karjala","Ahvenanmaa")

industrynamevec <- c("Agriculture, Forestry, And Fishing","Mining","Construction","Manufacturing","Transportation, Communications, Electric, Gas, And Sanitary Services",
                     "Wholesale Trade","Retail Trade","Finance, Insurance, And Real Estate","Services","Public Administration")

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

##### FUNCTIONS #####

fetchyears <- function(x1, x2, x3, x4) {
  dates <- BRC$Date10kBefore[as.Date(BRC$Date10kBefore,"%m/%d/%Y") > x1 & as.Date(BRC$Date10kBefore,"%m/%d/%Y") < x2 & is.element(BRC$industryabbr, x3) & is.element(BRC$fakefinregions, x4)]
  dates <- as.POSIXct(dates, format = "%m/%d/%Y")
  years <- as.numeric(format(dates, format="%Y"))
  years
}

fetchindustries <- function(x1, x2, x3, x4) {
  relevantindustries <- BRC$industry[as.Date(BRC$Date10kBefore,"%m/%d/%Y") > x1 & as.Date(BRC$Date10kBefore,"%m/%d/%Y") < x2 & is.element(BRC$industryabbr, x3) & is.element(BRC$fakefinregions, x4)]
  relevantindustries
}

fetchregions <- function(x1, x2, x3, x4) {
  relevantregions <- BRC$fakefinregions[as.Date(BRC$Date10kBefore,"%m/%d/%Y") > x1 & as.Date(BRC$Date10kBefore,"%m/%d/%Y") < x2 & is.element(BRC$industryabbr, x3) & is.element(BRC$fakefinregions, x4)]
  relevantregions
}

fetchassets <- function(x1, x2, x3, x4) {
  relevantassets <- BRC$AssetsBefore[as.Date(BRC$Date10kBefore,"%m/%d/%Y") > x1 & as.Date(BRC$Date10kBefore,"%m/%d/%Y") < x2 & is.element(BRC$industryabbr, x3) & is.element(BRC$fakefinregions, x4)]
  relevantassets
}