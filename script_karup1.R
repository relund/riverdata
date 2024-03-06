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

url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/assoc/49F1767931B31CD0C1258398007953C0/1/"
prefix <- "data/data_karup"
yr <- year(now())

#### Catch records ####
datCatchSeatrout <- writeCatch(url, prefix, yr)

#### Weight ####
writeWeightEstimates(prefix, seatrout = TRUE)


#### Waterlevel ####
stations <- tibble(id = c("24262", "24265", "23645"), place = c("Karup By", "Hagebro", "Nørkærbro"))
writeTimeSeriesData(stations, prefix, prefix1 = "waterlevel", days = 15)  
# d <- as.integer(now() - ymd_hms("2017-01-01 12:00:00"))
# writeTimeSeriesData(stations, prefix, prefix1 = "waterlevel", days = d)  # if update from 2017

## Calc moving average 
dat <- readDataFiles("data_karup_waterlevel_[0-9]{4}")
rMeans <- writeWaterMovAvg(dat, prefix)
# ggplot(dat %>% filter (year(Date) == 2022), aes(x = Date, y = Value)) + geom_line() + facet_wrap(vars(Place), scales = "free", nrow = 3)
# dat <- dat %>% mutate(Day = yday(Date)) %>% left_join(rMeans)
# ggplot(dat, aes(x = Date, y = Value)) + geom_line() + geom_line(aes(x = Date, y = Level_rAvg90), color = "blue") + facet_wrap(vars(Place), scales = "free", nrow = 3)
# ggplot(rMeans, aes(x = Day, y = Level_rAvg90)) + geom_line() + facet_grid(rows = vars(Place), scales = "free")

## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)
# dat1 <- dat %>% mutate(yr = year(Date), Day = yday(Date))
# ggplot(dat1, aes(x = Day, y = Level, color = factor(yr))) + geom_line() + facet_wrap(vars(Place), scales = "free", nrow = 3)

## Dataset for web 
dat <- writeWaterLevelsWeb(dat, prefix)


#### Water temperature ####
stations <- tibble(id = c("1555"), place = c("Hagebro"))
writeTimeSeriesData(stations, prefix, prefix1 = "watertemp", days = 50)  

## Calc moving average 
dat <- readWTemp(prefix, 2020:year(now()))
rMeans <- writeWaterTempMovAvg(dat, prefix)

## Dataset for web 
dat <- writeWaterTempWeb(dat, rMeans, prefix)


#### Map ####
# Places
lst <- stripKml("1XJoAUKY_-kbmhZgovPpLgi82Gn8")  
datMarkers <- lst$datMarkers
datLines <- lst$datLines

# MV-LF
lst <- stripKml("1A1Oi7hPeFbAU2ahS_zWgg6Fqcxa5Gbg", Club = "MV-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

# LFSO - Use my own maps since otherwise cannot identify dagkort/medlem
mapIds <- c("1REe4_q1yvBikiH2F7ZhsCAVCsmc", # Zone 1
            "1yXJtKx-ahWi9daAYA6Qh-AeO9lw", # Zone1a 
            "16-gVaFz4dOviJEUKHikj4lsDKVM", # Zone 2a
            "14LWr7b1Wuobkx2qW5ZYywuk8R9s", # Zone 2b
            "1z7mp4FsSawKil8dh6z-zynIZacs", # Zone 3
            "1JtSO1fPJLH37bnv0QG_svI7c4-Q", # Zone 4
            "1RO7N1_1J_LYRlDDKdkkts9ydSgw"  # Zone 5
)
for (i in 1:length(mapIds)) {
  lst <- stripKml(mapIds[i], Club = "LFSO") 
  lst$datLines$LineGroupId <- lst$datLines$LineGroupId + i*100
  datMarkers <- bind_rows(datMarkers, lst$datMarkers)
  datLines <- bind_rows(datLines, lst$datLines)
}

datMarkers <- datMarkers  %>% filter(!is.na(Icon))
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))


