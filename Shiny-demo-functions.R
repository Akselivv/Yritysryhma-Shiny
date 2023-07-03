#list_of_packages <- c("shiny", "plyr", "plotly","shinyjs","ECharts2Shiny", "tidyverse", "highcharter", "shinydashboard")

#list_of_removed_packages <- c("grid", "d3Tree", "ash", "XML", "treemapify", "shiny.router", "tidyr", "gplots", "lubridate",
#                             "RColorBrewer", "treemap", "DescTools", "ggmap", "maps", "maptools", "mapdata", "rgeos", "broom")

#list_of_redundant_packages <- c("TSstudio", "writexl", "gtrendsR", "rvest", "htmltools", "Hmisc", "RCurl", "zoo", "scales",
#                                "rgdal", "dplyr", "ggplot2", "readr", "readxl")

#packages_to_install <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
#if(length(packages_to_install)) install.packages(packages_to_install)
#lapply(list_of_packages, require, character.only = TRUE)

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


fetchyears <- function(x1, x2, x3, x4) {
  dates <- BRCfin$time[BRCfin$time > x1 & BRCfin$time < x2 & is.element(BRCfin$TOL, x3) & is.element(BRCfin$maakunta, x4)]
  #dates <- as.POSIXct(dates, format = "%m/%d/%Y")
}

fetchindustries <- function(x1, x2, x3, x4) {
  relevantindustries <- BRCfin$TOL[BRCfin$time > x1 & BRCfin$time < x2 & is.element(BRCfin$TOL, x3) & is.element(BRCfin$maakunta, x4)]
  #relevantindustries
}

fetchregions <- function(x1, x2, x3, x4) {
  relevantregions <- BRCfin$maakunta[BRCfin$time > x1 & BRCfin$time < x2 & is.element(BRCfin$TOL, x3) & is.element(BRCfin$maakunta, x4)]
  #relevantregions
}

fetchassets <- function(x1, x2, x3, x4) {
  relevantnobs <- BRCfin$nobs[BRCfin$time > x1 & BRCfin$time < x2 & is.element(BRCfin$TOL, x3) & is.element(BRCfin$maakunta, x4)]
  #relevantnobs
}

vector.is.empty <- function(x) {
  return(length(x) ==0 )
}

time_series_wrangler <- function(x) {

  colnames(x)[1] <- "time"

  x <- as.data.frame(x)

  x$time <- gsub('M', '', x$time)

  x$time <- as.POSIXct(paste0(as.character(x$time), '01'), format='%Y%m%d')

  for (i in 2:length(x[1,])) {
    x[,i] <- as.numeric(unlist(x[,i]))
  }

  return(x)

}

quartal_wrangler <- function(x) {

  colnames(x)[1] <- "time"

  x <- as.data.frame(x)

  x$time <- gsub("Q1", "01", x$time)
  x$time <- gsub("Q2", "04", x$time)
  x$time <- gsub("Q3", "07", x$time)
  x$time <- gsub("Q4", "10", x$time)

  x$time <- as.POSIXct(paste0(as.character(x$time), '01'), format='%Y%m%d')

  x <- x[!duplicated(x$time),]

  return(x)

}

data_cropper <- function(data, cols, startrows, endrows) {

  data <- data[,cols]
  data <- data[-startrows,]
  data <- data[-endrows,]

}

tuhaterotin <- function(x){
  format(x, big.mark= " ",
         decimal.mark = ",",
         scientific = FALSE)
}

prosenttierotin <- function(x){
  paste0(tuhaterotin(x*100), " %")
}

randcolor <- function() {

  y <- sample(1:8, 1, replace = TRUE)
  return(DHcolors[y])
}

colorsample <- function(x) {

  y <- sample(1:8, x, replace=FALSE)
  return(DHcolors[y])
}

month_wrangler <- function(data) {

  colnames(data)[1] <- "time"

  data$time <- gsub('M', '', data$time)
  data$time <- as.POSIXct(paste0(as.character(data$time), '01'), format='%Y%m%d')
  return(data)

}

months_to_quartals <- function(x) {

  colnames(x)[1] <- "time"

  x$time <- gsub("M01", "0101", x$time)
  x$time <- gsub("M02", "0101", x$time)
  x$time <- gsub("M03", "0101", x$time)
  x$time <- gsub("M04", "0401", x$time)
  x$time <- gsub("M05", "0401", x$time)
  x$time <- gsub("M06", "0401", x$time)
  x$time <- gsub("M07", "0701", x$time)
  x$time <- gsub("M08", "0701", x$time)
  x$time <- gsub("M09", "0701", x$time)
  x$time <- gsub("M10", "1001", x$time)
  x$time <- gsub("M11", "1001", x$time)
  x$time <- gsub("M12", "1001", x$time)

  x$time <- as.POSIXct(as.character(x$time), format='%Y%m%d')

  return(x)

}

rotate_map <- function(data, angle) {

  angle <- (1/100)*angle

  rotmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow=2, ncol=2, byrow=TRUE)

  for (i in 1:length(data$lat)) {
    temp <- c(data$lat[i], data$long[i])
    temp <- rotmatrix%*%temp
    data$lat[i] <- temp[1]
    data$long[i] <- temp[2]
  }

  for (i in 1:length(data$meanlat)) {
    temp <- c(data$meanlat[i], data$meanlong[i])
    temp <- rotmatrix%*%temp
    data$meanlat[i] <- temp[1]
    data$meanlong[i] <- temp[2]
  }

  return(data)

}

show_colors <- function(alpha = 1) {
  tibble(colors) %>%
    ggplot()  +
    geom_treemap(aes(area = 1, fill = colors), alpha = alpha) +
    scale_fill_manual(values = colors)
}
