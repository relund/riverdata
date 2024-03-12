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
library(RCurl)
library(conflicted)
conflicts_prefer(
  dplyr::filter(),
  dplyr::lag
)

source("functions.R")

url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/assoc/A97F957DD48AEDD4C1258814003E71FE/1/"
prefix <- "data/data_skjern"
yr <- year(now())

#### Catch records ####
datCatchSalmon <- writeCatch(url, prefix, yr, species = "Laks")
datCatchSeatrout <- writeCatch(url, prefix, yr, species = "Havørred")


#### Flow though lock at Hvide Sande ####
dat <- writeLockSkjern(prefix)
writeLockWeb(dat, prefix)


#### Weight ####
writeWeightEstimates(prefix, seatrout = TRUE)
writeWeightEstimates(prefix, seatrout = FALSE)


#### Hobo station (Laksens hus) ####
saveHoboData()

#### Waterlevel ####
stations <-
  tibble(id = c("24717", "24605", "24622", "54559", "24649", "24657", "24601", "468"),
         place = c("Skjern Å - Sandfeldvej",
                   "Skjern Å - Alergaard",
                   "Skjern Å - Gjaldbæk bro",
                   "Skjern Å - Landevejsbroen",
                   "Karstoft Å - Fiskedamme",
                   "Rind Å - Arnborg kirke",
                   "Omme Å - Sønderskov bro",
                   "Fjederholt Å - A18"))
writeTimeSeriesData(stations, prefix, prefix1 = "waterlevel", days = 15)  
# d <- as.integer(now() - ymd_hms("2017-01-01 12:00:00"))
# writeTimeSeriesData(stations, prefix, prefix1 = "waterlevel", days = d)  # if update from 2017

## Calc moving average 
dat <- readDataFiles("data_skjern_waterlevel_[0-9]{4}")
rMeans <- writeWaterMovAvg(dat, prefix)
## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)
## Dataset for web 
dat <- writeWaterLevelsWeb(dat, prefix)


#### Water temperature ####
## Add Hobo data
hobo <- read_csv(str_c(prefix, "_watertemp_hobo.csv"))
for (y in unique(year(hobo$Date))) {
  write_csv(hobo %>% filter(year(Date) == y),
    str_c(prefix, "_watertemp_", y, ".csv"), 
    append = T)
}
hobo <- hobo %>% slice_head(n = 0)
write_csv(hobo, str_c(prefix, "_watertemp_hobo.csv"))


#### Map ####
lst <- stripKml("135J9l0kVoBKkdIdG_0vc3U9WJeuUPWyJ")  # Places
datMarkers <- lst$datMarkers
datLines <- lst$datLines

## MV-LF
lst <- stripKml("1NiY_55Mw_GUULepLXq6Wjt0Gtu2K2c0", Club = "MV-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

## Skj-LF
lst <- stripKml("1MzpHBDHJqemOQK81Z7z2CVwzdzrXGDlF", Club = "Skj-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers)
datLines <- bind_rows(datLines, lst$datLines)

## BFF
lst <- stripKml("1-B74S5cts6E4KNUyP2vxBpQ_r9pZcGSD", Club = "BFF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers)
datLines <- bind_rows(datLines, lst$datLines)

## LF1926 (has a map for each place with no layers, try to hack)
# first add medlem zones
mapIds <- c("1d8I43tTbY5IyOjzTHpqlY8hd7F0", # Sdr. Felding
            "1lHHXIhjmF23X1wG0MWqLPiazhjg", # Udløbet 
            "1BxltqquXBJRRxj2_GVWzjA1TbhQ", # Skarrild
            "1OIEh02I2rpO8a1F05Lq3LijF4O8", # Albæk
            "1kkZdxvxzBZ03pGrDprGEDnHEqS0", # Karstoft Å ved Skarrildhus
            "1MGXP6audGZCH0Dz6iNJ27mHGUOA", # Omme Å ved Farre
            "1q0d_VvbuvqHvHljTSeuv3QkMty9Sjg6O", # Omme Å ved Stovstrup,
            "1tmD0iHbHGayL4GgtiKWyBESDu4uamFmC", # Omme Å ved Rabæk
            "1ojlbyHZ7lwkeiYrUWFN6nz3Qunw", # Holtum å v Fasterholt
            "12DDsCWF1P8bRYwaDuZa0jhPTetw", # Vorgod Å ved Troldhede
            "1Xoln0k9Xf7qS05QKrFzovBLkOk8" # Vorgod Å nedre
            )
for (i in 1:length(mapIds)) {
  lst <- stripKml(mapIds[i], Club = "LF1926", GroupNameLines = "medlem") 
  lst$datLines$LineGroupId <- lst$datLines$LineGroupId + i*10
  datMarkers <- bind_rows(datMarkers, lst$datMarkers) %>% filter(!is.na(Icon))
  datLines <- bind_rows(datLines, lst$datLines)
}
# lst <- stripKml("1BxltqquXBJRRxj2_GVWzjA1TbhQ", Club = "LF1926", GroupNameMarkers = "parkering", GroupNameLines = "medlem")  # Skarrild
# lst$datLines <- lst$datLines %>% mutate(Group = if_else(str_detect(Text, fixed('clasonborg', ignore_case=TRUE)), "dagkort", Group))
# datMarkers <- bind_rows(datMarkers, lst$datMarkers) %>% 
#   filter(!is.na(Icon))
# datLines <- bind_rows(datLines, lst$datLines)

## LFSO
lst <- stripKml("1epDPyEYZsmz3gGpgUZi5UbnnlPc", Club = "LFSO")
datMarkers <- bind_rows(datMarkers, lst$datMarkers)
datLines <- bind_rows(datLines, lst$datLines)


## Write to csv
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))

## Render reports
render("reports/skjern/skjern-kort.Rmd",output_dir = "docs/skjern")
render("reports/skjern/skjern-salmometer.Rmd",output_dir = "docs/skjern")
render("reports/skjern/skjern-lock-flow.Rmd",output_dir = "docs/skjern")
render("reports/skjern/skjern-weight.Rmd",output_dir = "docs/skjern")
render("reports/skjern/skjern-waterlevel.Rmd",output_dir = "docs/skjern")
render("reports/skjern/skjern-catch.Rmd",output_dir = "docs/skjern")
render("docs/index.md",output_dir = "docs/")
