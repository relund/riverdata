library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo) 
source("functions.R")

prefix <- "data/data_karup"

#### Catch records ####
datCatchSeatrout <- writeCatchKarup()

#### Weight ####
estimateWeight(paste0(prefix, "_weight_seatrout.csv"), datCatchSeatrout, minLength =  40)

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
stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
## Update data current year
updateWaterLevel(stations, prefix)    

## Calc moving average 
dat <- readWLevels(prefix, 2013:year(now()))
rMeans <- calcWaterMovAvg(dat, prefix)

## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)

## Dataset for web 
dat <- calcWaterLevelsWeb(dat, prefix)



#### Water temperature ####
stations <- tibble(id = c("59885"), place = c("Hagebro"))

## Update data current year
updateWaterTempKarup(stations, prefix)  

## Calc moving average 
dat <- readWTemp(prefix, 2020:year(now()))
rMeans <- calcWaterTempMovAvg(dat, prefix)

## Dataset for web 
dat <- calcWaterTempWeb(dat, rMeans, prefix)


#### Map ####
mapId <- "1XJoAUKY_-kbmhZgovPpLgi82Gn8" # Places
lst1 <- stripKml(mapId)
mapId <- "1fzTAHG04McV3WO_IKxuxSPkDIaE" # HI-LF
lst2 <- stripKml(mapId, Club = "HI-LF")
datMarkers <- bind_rows(lst1$datMarkers, lst2$datMarkers)
datLines <- bind_rows(lst1$datLines, lst2$datLines)
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))


