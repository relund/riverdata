library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tsibble)
library(fs)
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
# stations <-
#   tibble(id = c("470"),
#          place = c("Fjederholt Å - A18"))
# 
# 
# dat <- read_csv("https://www.hobolink.com/p/129c8db1cac5dff51e4157b3644ad63e#")
# 
# 
# 
# 
# 
# library(httr)
# # library(rvest)
# url <- "https://www.hobolink.com/p/129c8db1cac5dff51e4157b3644ad63e#"
# res <- POST(url,
#             encode="json",
#             user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36"),
#             add_headers(`referer`="https://www.hobolink.com/p/129c8db1cac5dff51e4157b3644ad63e"),
#             # body = "javax.faces.partial.ajax=true&javax.faces.source=hobolink-devices-real-time-plots-form%3Aj_idt198%3Aj_idt333&javax.faces.partial.execute=%40all&javax.faces.partial.render=hobolink-devices-real-time-plots-form%3Aj_idt198%3AdayPlot406715&hobolink-devices-real-time-plots-form%3Aj_idt198%3Aj_idt333=hobolink-devices-real-time-plots-form%3Aj_idt198%3Aj_idt333&hobolink-devices-real-time-plots-form=hobolink-devices-real-time-plots-form&hobolink-devices-real-time-plots-form%3Aj_idt198_activeIndex=0&javax.faces.ViewState=-5503341876077484605%3A8942472651375839520"
#             body=list(
#               javax.faces.partial.ajax = "true",
#               javax.faces.source = "hobolink-devices-real-time-plots-form:j_idt198:j_idt333",
#               javax.faces.partial.execute = "@all",
#               javax.faces.partial.render = "hobolink-devices-real-time-plots-form:j_idt198:dayPlot406715",
#               `hobolink-devices-real-time-plots-form:j_idt198:j_idt333` = "hobolink-devices-real-time-plots-form:j_idt198:j_idt333",
#               `hobolink-devices-real-time-plots-form` = "hobolink-devices-real-time-plots-form",
#               `hobolink-devices-real-time-plots-form:j_idt198_activeIndex` = "0",
#               javax.faces.ViewState = "-5503341876077484605:8942472651375839520"
# )
# 
#               , verbose())
# 
# res_t <- content(res, as="text") %>% str_detect("dayGraphData")
# res_h <- paste0(unlist(strsplit(res_t, "\r\n"))[-1], sep="", collapse="\n")
# # 
# # 
# # 
# # monthGraphData367929
# 
# 
# 
# library(httr)
# # library(rvest)
# url <- "https://www.hobolink.com/p/129c8db1cac5dff51e4157b3644ad63e#hobolink-devices-real-time-plots-form:j_idt198:week-tab"
# x <- GET(url, add_headers('user-agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36'))
# xml <- read_html(x)
# xml %>% xml_find_all('/html/body/div[4]/div/div[2]/div/div[2]/div[2]/div/form/div/div/div[2]/div/div') %>% xml_text()
# 
# 
# 
# 
# read_html(x)
# ids <- html_nodes(page,
#                   xpath = '/html/body/div[4]/div/div[2]/div/div[2]/div[2]/div/form/div/div/div[2]/div/div/div/div[2]/div/div[4]/script')
# 
# page %>% html_elements("script")
# 
# 
# # ## Calc moving average 
# # dat <- readWTemp(prefix, 2020:year(now()))
# # rMeans <- calcWaterTempMovAvg(dat, prefix)
# # 
# # ## Dataset for web 
# # dat <- calcWaterTempWeb(dat, rMeans, prefix)



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
  lst <- stripKml(mapIds[i], Club = "LF1926", GroupNameMarkers = "parkering", GroupNameLines = "medlem") 
  lst$datLines$LineGroupId <- lst$datLines$LineGroupId + i*10
  datMarkers <- bind_rows(datMarkers, lst$datMarkers)
  datLines <- bind_rows(datLines, lst$datLines)
}
# lst <- stripKml("1BxltqquXBJRRxj2_GVWzjA1TbhQ", Club = "LF1926", GroupNameMarkers = "parkering", GroupNameLines = "medlem")  # Skarrild
# lst$datLines <- lst$datLines %>% mutate(Group = if_else(str_detect(Text, fixed('clasonborg', ignore_case=TRUE)), "dagkort", Group))
datMarkers <- bind_rows(datMarkers, lst$datMarkers) %>% 
  filter(!is.na(Icon))
datLines <- bind_rows(datLines, lst$datLines)

## Write to csv
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))

## Render reports
render("reports/skjern-kort.Rmd",output_dir = "docs/skjern")
