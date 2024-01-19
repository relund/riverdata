library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tsibble)
library(fs)
library(rmarkdown)
source("functions.R")

# url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/assoc/49F1767931B31CD0C1258398007953C0/1/"
prefix <- "data/data_mv-lf"
# yr <- year(now())

#### Map ####

# MV-LF
lst <- stripKml("1_xtzzPTUUiecahNG71C_4rOY_izwVmU", Club = "MV-LF") # storå
datMarkers <- lst$datMarkers
datLines <- lst$datLines

lst <- stripKml("14vdqfI93frzgB0jmm44GPP3bIu5aVH8", Club = "MV-LF") # lake fishing
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

datMarkers <- datMarkers  %>% filter(!is.na(Icon))
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))

render("reports/mv-lf-kort.Rmd",output_dir = "docs/mv-lf")
