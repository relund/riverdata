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
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::lag
)

source("functions.R")


#### Catch ####
# To 2023 only HI-LF data
url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/club/37E6FB0707BAE0A1C1257F3A003FF013/1/"
prefix <- "data/data_mv-lf"
yr <- year(now())

#### Catch records ####
datCatch <- writeCatch(url, prefix, yr, species = "All", club = TRUE)

#### Map ####
# Stednavne Karup
lst <- stripKml("1XJoAUKY_-kbmhZgovPpLgi82Gn8")  
datMarkers <- lst$datMarkers
datLines <- lst$datLines

# Stednavne Skjern
lst <- stripKml("135J9l0kVoBKkdIdG_0vc3U9WJeuUPWyJ")  # Places
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

# Karup
lst <- stripKml("1A1Oi7hPeFbAU2ahS_zWgg6Fqcxa5Gbg", Club = "MV-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

## Skjern
lst <- stripKml("1NiY_55Mw_GUULepLXq6Wjt0Gtu2K2c0", Club = "MV-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

# Storå
lst <- stripKml("1_xtzzPTUUiecahNG71C_4rOY_izwVmU", Club = "MV-LF") 
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

# Lake
lst <- stripKml("14vdqfI93frzgB0jmm44GPP3bIu5aVH8", Club = "MV-LF") 
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)



datMarkers <- datMarkers  %>% filter(!is.na(Icon))
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))

render("reports/mv-lf-kort.Rmd",output_dir = "docs/mv-lf")
