list_of_packages <- c("shiny", "plyr", "plotly","shinyjs","ECharts2Shiny", "tidyverse", "highcharter", "shinydashboard")

list_of_removed_packages <- c("grid", "d3Tree", "ash", "XML", "treemapify", "shiny.router", "tidyr", "gplots", "lubridate",
                              "RColorBrewer", "treemap", "DescTools", "ggmap", "maps", "maptools", "mapdata", "rgeos", "broom")

list_of_redundant_packages <- c("TSstudio", "writexl", "gtrendsR", "rvest", "htmltools", "Hmisc", "RCurl", "zoo", "scales", 
                                "rgdal", "dplyr", "ggplot2", "readr", "readxl")

packages_to_install <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)
lapply(list_of_packages, require, character.only = TRUE)

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

vector.is.empty <- function(x) { 
  return(length(x) ==0 )
}

time_series_wrangler <- function(x) {
  
  colnames(x)[1] <- "time"
  
  x <- as.data.frame(x)
  
  print(x)
  
  x$time <- gsub('M', '', x$time)
  
  x$time <- as.POSIXct(paste0(as.character(x$time), '01'), format='%Y%m%d')
  
  for (i in 2:length(x[1,])) {
    x[,i] <- as.numeric(unlist(x[,i]))
  }
  
  print(x)
  
  return(x)
  
}

data_cropper <- function(data, cols, startrows, endrows) {
  
  data <- data[,cols]
  data <- data[-startrows,]
  data <- data[-endrows,]
  
}