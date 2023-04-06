library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tsibble)
library(fs)
source("functions.R")

url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/assoc/49F1767931B31CD0C1258398007953C0/1/"
prefix <- "data/data_karup"
yr <- year(now())

#### Catch records ####
datCatchSeatrout <- writeCatch(url, prefix, yr)

#### Weight ####
estimateWeight(prefix, seatrout = TRUE)

#### Save montly statistics for current year ####
# datStat <- datCatch %>% 
#   dplyr::filter(year(Date) == year(now()), Length > 39) %>% 
#   group_by(Month) %>% nest() %>% 
#   mutate(
#     TotalStat = map(data, function(df) {
#       summarise(df, Total = n(), 
#                 Female = sum(Sex == "Female", na.rm = T), 
#                 Male = sum(Sex == "Male", na.rm = T),
#                 SexUnknown = Total - Female - Male,
#                 Released = sum(!Killed, na.rm = T),
#                 Killed = sum(Killed, na.rm = T),
#                 KilledUnknown = Total - Released - Killed,
#                 LengthAvg = mean(Length, na.rm = T), 
#                 LengthMax = max(Length, na.rm = T),
#                 WeightAvg = mean(Weight, na.rm = T), 
#                 WeightMax = max(Weight, na.rm = T),
#                 Kg = sum(Weight, na.rm = T),
#                 FultonAvg = mean(Fulton, na.rm = T), 
#                 FultonMax = max(Fulton, na.rm = T)
#       )
#     }),
#     PlaceStat = 
#       map(data, 
#           function(df) {
#             df %>% 
#             group_by(Place) %>% 
#             summarize(TotalPlace = n())}) 
#   )
# 
# datStat <- datStat %>% 
#   mutate(PlaceStat = 
#            map(PlaceStat, function(df) {  
#              pivot_wider(df, names_from = Place, values_from = TotalPlace)})) %>% 
#   unnest(cols = c(TotalStat, PlaceStat)) %>% select(-data) %>% 
#   rename_at(vars(contains(c("Nedre","Mellem","Øvre","Haderup Å"))), 
#     function(x) {
#       case_when(
#         x == "Nedre" ~ "Lower",
#         x == "Mellem" ~ "Middle",
#         x == "Øvre" ~ "Upper",
#         x == "Haderup Å" ~ "Haderis",
#         TRUE ~ "FEJL"
#       )
#     }) %>% 
#   replace(., is.na(.), 0)
# 
# ## Save to file
# fn <- "data/data_karup_catch_seatrout_stat_month.csv"
# write_csv(datStat, fn)



#### Waterlevel ####
stations <- tibble(id = c("24262", "24265", "23645", "1040"), place = c("Karup By", "Hagebro", "Nørkærbro", "Hagebro"))
saveTimeSeriesData(stations, prefix, prefix1 = "waterlevel", days = 50)  
# saveTimeSeriesData(stations, prefix, prefix1 = "waterlevel", days = 50000)  # if update all years

## Calc moving average 
dat <- readWLevels(prefix, 1986:year(now()))
rMeans <- calcWaterMovAvg(dat, prefix)

## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)

## Dataset for web 
dat <- calcWaterLevelsWeb(dat, prefix)



#### Water temperature ####
stations <- tibble(id = c("1555"), place = c("Hagebro"))
saveTimeSeriesData(stations, prefix, prefix1 = "watertemp", days = 50)  

## Calc moving average 
dat <- readWTemp(prefix, 2020:year(now()))
rMeans <- calcWaterTempMovAvg(dat, prefix)

## Dataset for web 
dat <- calcWaterTempWeb(dat, rMeans, prefix)


#### Map ####
# Places
lst <- stripKml("1XJoAUKY_-kbmhZgovPpLgi82Gn8")  
datMarkers <- lst$datMarkers
datLines <- lst$datLines
# HI-LF
lst <- stripKml("1fzTAHG04McV3WO_IKxuxSPkDIaE", Club = "HI-LF")
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


