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
  dplyr::lag,
  plotly::layout
)

here::i_am("script_karup.R")
library(riverdata)

url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/assoc/49F1767931B31CD0C1258398007953C0/1/"
prefix <- "data/data_karup"
yr <- year(now())

#### Catch records ####
datCatchSeatrout <- write_catch(url, prefix, yr, species = "seatrout")

#### Weight ####
write_weight_estimates(prefix, seatrout = TRUE)


#### Waterlevel ####
stations <- tibble(id = c("24262", "24265", "23645"), place = c("Karup By", "Hagebro", "Nørkærbro"))
write_time_series_data(stations, prefix, prefix1 = "waterlevel", days = 15)  
# d <- as.integer(now() - ymd_hms("2017-01-01 12:00:00"))
# write_time_series_data(stations, prefix, prefix1 = "waterlevel", days = d)  # if update from 2017

## Calc moving average 
dat <- read_data_files("data_karup_waterlevel_[0-9]{4}")
rMeans <- write_water_mov_avg(dat, prefix)
# ggplot(dat %>% filter (year(Date) == 2022), aes(x = Date, y = Value)) + geom_line() + facet_wrap(vars(Place), scales = "free", nrow = 3)
# dat <- dat %>% mutate(Day = yday(Date)) %>% left_join(rMeans)
# ggplot(dat, aes(x = Date, y = Value)) + geom_line() + geom_line(aes(x = Date, y = Level_rAvg90), color = "blue") + facet_wrap(vars(Place), scales = "free", nrow = 3)
# ggplot(rMeans, aes(x = Day, y = Level_rAvg90)) + geom_line() + facet_grid(rows = vars(Place), scales = "free")

## Relative datasets 
dat <- calc_water_level_relative(dat, rMeans, prefix)
# dat1 <- dat %>% mutate(yr = year(Date), Day = yday(Date))
# ggplot(dat1, aes(x = Day, y = Level, color = factor(yr))) + geom_line() + facet_wrap(vars(Place), scales = "free", nrow = 3)

## Dataset for web 
dat <- write_water_levels_web(dat, prefix)


#### Water temperature ####
stations <- tibble(id = c("1555"), place = c("Hagebro"))
write_time_series_data(stations, prefix, prefix1 = "watertemp", days = 50)  

## Calc moving average 
dat <- read_w_temp(prefix, 2020:year(now()))
rMeans <- write_water_temp_mov_avg(dat, prefix)

## Dataset for web 
dat <- write_water_temp_web(dat, rMeans, prefix)


#### Map ####
# Places
lst <- strip_kml("1XJoAUKY_-kbmhZgovPpLgi82Gn8")  
datMarkers <- lst$datMarkers
datLines <- lst$datLines

# MV-LF
lst <- strip_kml("1A1Oi7hPeFbAU2ahS_zWgg6Fqcxa5Gbg", club = "MV-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

# LFSO - Use my own maps since otherwise cannot identify dagkort/medlem
lst <- strip_kml("17UJPQS6LtN9BITZnTsUahZ9g1icjmGk", club = "LFSO")
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

## LF1926 (has a map for each place with no layers, try to hack)
# first add medlem zones
mapIds <- c("1LI-ffNiZ8Dpsy1NpzFETFx2wDCE", # Karup by
            "1rswcoFnh7eulrqrFRmce5JkBaUM", # Høgild 
            "1TBJx26ZUmmHmvKsSiDuml56BvZo", # Sdr. Resen
            "1fn5QbQsZzsENw5NtgK2I1gzUZzk", # Koldkur
            "1UqLT35e2RP5AHndu5227kVq5_lg", # Åkjær
            "1BRFJOIFKxM-zaHNc80HtjfN1cVc" # Estvad
)
for (i in 1:length(mapIds)) {
  lst <- strip_kml(mapIds[i], club = "LF1926", group_name_lines = "medlem") 
  lst$datLines$LineGroupId <- lst$datLines$LineGroupId + i*10
  datMarkers <- bind_rows(datMarkers, lst$datMarkers) %>% filter(!is.na(Icon))
  datLines <- bind_rows(datLines, lst$datLines)
}

datMarkers <- datMarkers  %>% filter(!is.na(Icon))
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))




#### Render reports ####
yr <- lubridate::year(lubridate::now())
render(here::here("reports/karup/karup-waterlevel.Rmd"), output_dir = "docs/karup")
render(here::here("reports/karup/karup-watertemp.Rmd"), output_dir = "docs/karup")
render(here::here("reports/karup/karup-kort.Rmd"), output_dir = "docs/karup")
render(here::here("reports/karup/karup-salmometer.Rmd"), output_dir = "docs/karup")
render(here::here("reports/karup/karup-catch.Rmd"), output_dir = "docs/karup")
render(here::here("docs/index.md"), output_dir = "docs/")
