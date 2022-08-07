library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tsibble)
source("functions.R")

prefix <- "data/data_skjern"

#### Flow though lock at Hvide Sande ####
dat <- updateLockSkjern(prefix)
calcLockWeb(dat, prefix)

#### Catch records ####

if (day(now()) == 19 & hour(now()) == 4) {
  datCatchSalmon <- updateCatchSkjern(prefix, species = "salmon", reset = TRUE)   #, reset = T, start = 2004
  datCatchSeatrout <- updateCatchSkjern(prefix, species = "trout", reset = TRUE) 
} else {
  datCatchSalmon <- updateCatchSkjern(prefix, species = "salmon") 
  datCatchSeatrout <- updateCatchSkjern(prefix, species = "trout")
}

#### Weight ####
estimateWeight(paste0(prefix, "_weight_salmon.csv"), datCatchSalmon, minLength =  40, maxLength = 145)
estimateWeight(paste0(prefix, "_weight_seatrout.csv"), datCatchSeatrout, minLength =  40)

#### Waterlevel - Prelim data set ####
# # Find id using http://hydrometri.azurewebsites.net/Scripts/azureplot_new.js and set a breakpoint
# stations <- 
#   tibble(id = c("055416", "055414", "001862", "017898", "054757", "001855", "052386"), 
#          place = c("Vorgod Å - Vandmøllen", 
#                    "Skjern Å - Sandfeldvej", 
#                    "Skjern Å - Alergaard",
#                    "Skjern Å - Gjaldbæk bro",
#                    "Rind Å - Arnborg kirke",
#                    "Omme Å - Sønderskov bro",
#                    "Fjederholt Å - A18"))
# 
# for (y in 2013:2020) {
#   fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
#   dat <- NULL
#   iso <- format(date(paste0(y+1, "-01-14")), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
#   for (i in 1:nrow(stations)) {
#     id <- stations$id[i]
#     place <- stations$place[i]
#     tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=4000&pw=100000000&inclraw=true"))
#     offset <- as.numeric(tmp$tsh$Offset)
#     tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
#     tmp$V <- tmp$V - rep(offset, length(tmp$V))
#     colnames(tmp) <- c("Date", paste0(place, " (", id, ")"))
#     if (is.null(dat)) {
#       dat <- tmp
#     } else {
#       dat <- full_join(dat,tmp, by = "Date")
#     }
#   }
#   dat$Date <- ymd_hms(dat$Date, tz = "UTC") %>% with_tz("CET") # from UTC to CET
#   dat <- dat %>% dplyr::filter(year(Date) == y) %>% arrange(Date)
#   readr::write_csv(dat, fn)
#   print(range(dat$Date))
# }
# 
# 
# ## Tidy prelim water level datasets 
# readWLevels <- function(years) {
#   colT <- cols(
#     Date = col_datetime(format = ""),
#     `Vorgod Å - Vandmøllen (055416)` = col_double(),
#     `Skjern Å - Sandfeldvej (055414)` = col_double(),
#     `Skjern Å - Alergaard (001862)` = col_double(),
#     `Skjern Å - Gjaldbæk bro (017898)` = col_double(),
#     `Rind Å - Arnborg kirke (054757)` = col_double(),
#     `Omme Å - Sønderskov bro (001855)` = col_double(),
#     `Fjederholt Å - A18 (052386)` = col_double()
#   )
#   dat <- NULL
#   cat("Read year:")
#   for (y in years) {
#     cat(y, "\n")
#     fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
#     dat <- bind_rows(dat, read_csv(fn, col_types = colT))
#   }
#   return(dat)
# }
# dat <- readWLevels(2014:2020)
# datL <- dat %>% pivot_longer(cols = 2:ncol(dat), names_to = 'Group', values_to = 'Level')
# datS <- datL %>% dplyr::filter(year(Date)>2013)
# ggplot(data = datS, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# # Most observations are from 2017. Only consider 2017-present
# dat <- dat %>% dplyr::filter(year(Date)>2016)
# datL <- dat %>% pivot_longer(cols = 2:ncol(dat), names_to = 'Group', values_to = 'Level')
# ggplot(data = datL, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# 
# # Remove outliners:
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.1, .9), na.rm = na.rm, ...)
#   # H <- 1.5 * IQR(x, na.rm = na.rm)
#   H <- 5 * sd(x, na.rm = na.rm)   # remove 8 sd above and below mean
#   y <- x
#   idx <- which(x < (qnt[1] - H) | x > (qnt[2] + H))
#   cat("Remove", length(which(x < (qnt[1] - H) | x > (qnt[2] + H))), "outliers\n")
#   # print(x[idx])
#   y[idx] <- NA
#   y
# }
# # dat1 <- dat %>% mutate_if(is.numeric, tsclean)   # remove outliers and replace missing values
# # dat1 <- dat1 %>% mutate_if(is.numeric, as.numeric)  # remove ts class
# # colnames(dat1)[2:4] = paste0(colnames(dat)[2:4]," clean")
# dat1 <- dat %>% mutate_if(is.numeric, remove_outliers)
# datL <- dat1 %>% pivot_longer(cols = 2:ncol(dat1), names_to = 'Group', values_to = 'Level')
# ggplot(data = datL, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# 
# ## Save cleaned data
# for (y in 2017:2019) {
#   fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
#   dat2 <- dat1 %>% dplyr::filter(year(Date) == y) %>% arrange(Date)
#   write_csv(dat2, fn)
# }


#### Waterlevel @ hydrometri.dk ####

# stations <-
#   tibble(id = c("24717", "24605", "24622", "24657", "24601", "24649"), 
#          place = c("Skjern Å - Sandfeldvej", 
#                    "Skjern Å - Alergaard",
#                    "Skjern Å - Gjaldbæk bro",
#                    "Rind Å - Arnborg kirke",
#                    "Omme Å - Sønderskov bro",
#                    "Karstoft Å - Fiskedamme"))
# tsid = id seems to have been changed add new id's (7/8/2022)
stations <-
  tibble(id = c("055414", "001862", "017898", "054757", "001855"), 
         place = c("Skjern Å - Sandfeldvej", 
                   "Skjern Å - Alergaard",
                   "Skjern Å - Gjaldbæk bro",
                   "Rind Å - Arnborg kirke",
                   "Omme Å - Sønderskov bro"))
## Update and save data current year
updateWaterLevel(stations, prefix) 
# getWaterLevels(stations, prefix) # if reset

## Calc moving average 
dat <- readWLevels(prefix, 2014:year(now()))
rMeans <- calcWaterMovAvg(dat, prefix)

## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)

## Dataset for web 
dat <- calcWaterLevelsWeb(dat, prefix)


# dat %>%
#   ggplot(aes(x = Date, y = Level, color = Place)) +
#   geom_line()
# 
# dat %>% group_by(Place) %>% nest()
# 
# 
# library(plotly)
# tmp <- as_tsibble(dat, key = Place)
# tmp <- tmp %>%
#   group_by_key() %>%
#   mutate(L = tsclean(Level, replace.missing = FALSE))
# #View(tmp %>% filter(round(Level,1) != round(L,1)))
# 
# 
# tmp <- tmp %>% filter(Place == "Omme Å - Sønderskov bro")
# 
# p <- tmp %>% 
#   ggplot() +
#   geom_line(aes(x = Date, y = L), color = "black") +
#   geom_line(aes(x = Date, y = Level), color = "red", alpha = 0.1) 
# p
# ggplotly(p)


#### Water temperature ####
# 
# ## Update data current year
# updateWaterTempSkjern(stations, prefix)  
# 
# library(httr)
# library(rvest)
# library(dplyr)
# res <- POST("https://www.hobolink.com/p/05811e4cdecf4a8832047fadcb59bbaf",
#             encode="form",
#             user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.50 Safari/537.36"),
#             add_headers(`Referer`="https://www.hobolink.com/p/05811e4cdecf4a8832047fadcb59bbaf"),
#             body=list(
#               # javax.faces.partial.ajax = "true",
#               # javax.faces.source = "hobolink-devices-real-time-plots-form:j_idt198:j_idt399",
#               # javax.faces.partial.execute = "@all",
#               # javax.faces.partial.render = "hobolink-devices-real-time-plots-form:j_idt198:monthPlot367929",
#               # `hobolink-devices-real-time-plots-form:j_idt198:j_idt399` = "hobolink-devices-real-time-plots-form:j_idt198:j_idt399",
#               # `hobolink-devices-real-time-plots-form` = "hobolink-devices-real-time-plots-form",
#               # `hobolink-devices-real-time-plots-form:j_idt198_activeIndex` = "2",
#               # javax.faces.ViewState = "5420958408134968228:-5689265068462936229"
#               
#               javax.faces.partial.ajax = "true",
#               javax.faces.source = "hobolink-devices-latest-data-form:latest-query-table-id:0:j_idt309",
#               javax.faces.partial.execute = "@all",
#               javax.faces.partial.render = "hobolink-devices-latest-data-form:hiddenDownloadButton",
#               `hobolink-devices-latest-data-form:latest-query-table-id:0:j_idt309` = "hobolink-devices-latest-data-form:latest-query-table-id:0:j_idt309",
#               `hobolink-devices-latest-data-form` = "hobolink-devices-latest-data-form",
#               `hobolink-devices-latest-data-form:j_idt265_collapsed` = "false",
#               `javax.faces.ViewState` = "1359004840769262366:3886099500288169103"
#               
#               # `javax.faces.partial.ajax` = "true",
#               # `javax.faces.source` = "hobolink-devices-real-time-plots-form:j_idt198",
#               # `javax.faces.partial.execute` = "hobolink-devices-real-time-plots-form:j_idt198",
#               # `javax.faces.partial.render` = "hobolink-devices-real-time-plots-form:j_idt198",
#               # `hobolink-devices-real-time-plots-form:j_idt198` = "hobolink-devices-real-time-plots-form:j_idt198",
#               # `hobolink-devices-real-time-plots-form:j_idt198_contentLoad` = "true",
#               # `hobolink-devices-real-time-plots-form:j_idt198_newTab` = "hobolink-devices-real-time-plots-form:j_idt198:month-tab",
#               # `hobolink-devices-real-time-plots-form:j_idt198_tabindex` = "2",
#               # `hobolink-devices-real-time-plots-form` = "hobolink-devices-real-time-plots-form",
#               # `hobolink-devices-real-time-plots-form:j_idt198_activeIndex` = "2",
#               # `javax.faces.ViewState` = "6311353073065817781:-147379906274328667"              
#               
#               
#               ), verbose())
# 
# 
# 
# 
# 
# res_t <- content(res, as="text")
# res_h <- paste0(unlist(strsplit(res_t, "\r\n"))[-1], sep="", collapse="\n")
# 
# 
# 
# monthGraphData367929
# url <- "https://www.hobolink.com/p/05811e4cdecf4a8832047fadcb59bbaf"
# page <- read_html(url)
# ids <- html_nodes(page, 
#                   xpath = '/html/body/div[4]/div/div[2]/div/div[2]/div[2]/div/form/div/div/div[2]/div/div/div/div[3]/div/div[5]/script') 
# 
# ## Calc moving average 
# dat <- readWTemp(prefix, 2020:year(now()))
# rMeans <- calcWaterTempMovAvg(dat, prefix)
# 
# ## Dataset for web 
# dat <- calcWaterTempWeb(dat, rMeans, prefix)



#### Map ####
lst <- stripKml("135J9l0kVoBKkdIdG_0vc3U9WJeuUPWyJ")  # Places
datMarkers <- lst$datMarkers
datLines <- lst$datLines
## HI-LF
mapIds <- c("1EKI26YR4FQtlIKIQoAbo3Rbooj8", # Skjern Å
            "16Fh0qfhd6Aqwr5SzssWHj3bLybA", # Vorgod Å
            "1TlsJDLuLyOdClyIJjvW4PluJ0xk", # Karstoft Å
            "1lNB2Ak-WMVkA9nvVRQBcgTOLylw", # Rind Å
            "1uSITXXEXqCtKr58RYDkUmqIFOkw" # Fjederholt Å
            )
for (i in 1:length(mapIds)) {
  lst <- stripKml(mapIds[i], Club = "HI-LF") 
  lst$datLines$LineGroupId <- lst$datLines$LineGroupId + i*100
  datMarkers <- bind_rows(datMarkers, lst$datMarkers)
  datLines <- bind_rows(datLines, lst$datLines)
}
## Skj-LF
lst <- stripKml("1MzpHBDHJqemOQK81Z7z2CVwzdzrXGDlF", Club = "Skj-LF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers)
datLines <- bind_rows(datLines, lst$datLines)
## BFF
lst <- stripKml("1-B74S5cts6E4KNUyP2vxBpQ_r9pZcGSD", Club = "BFF")
datMarkers <- bind_rows(datMarkers, lst$datMarkers)
datLines <- bind_rows(datLines, lst$datLines)
## LF1926 (has a map for each place with no layers, try to hack)
mapIds <- c("1d8I43tTbY5IyOjzTHpqlY8hd7F0", # Sdr. Felding
            "1lHHXIhjmF23X1wG0MWqLPiazhjg", # Udløbet
            "1OIEh02I2rpO8a1F05Lq3LijF4O8", # Albæk
            "1kkZdxvxzBZ03pGrDprGEDnHEqS0", # Karstoft Å ved Skarrildhus
            "1MGXP6audGZCH0Dz6iNJ27mHGUOA", # Omme Å ved Farre
            "1q0d_VvbuvqHvHljTSeuv3QkMty9Sjg6O", # Omme Å ved Stovstrup,
            "1tmD0iHbHGayL4GgtiKWyBESDu4uamFmC", # Omme Å ved Rabæk
            "12DDsCWF1P8bRYwaDuZa0jhPTetw", # Vorgod Å ved Troldhede
            "1Xoln0k9Xf7qS05QKrFzovBLkOk8" # Vorgod Å nedre
            )
for (i in 1:length(mapIds)) {
  lst <- stripKml(mapIds[i], Club = "LF1926", GroupNameMarkers = "parkering", GroupNameLines = "medlem") 
  lst$datLines$LineGroupId <- lst$datLines$LineGroupId + i*10
  datMarkers <- bind_rows(datMarkers, lst$datMarkers)
  datLines <- bind_rows(datLines, lst$datLines)
}
lst <- stripKml("1BxltqquXBJRRxj2_GVWzjA1TbhQ", Club = "LF1926", GroupNameMarkers = "parkering", GroupNameLines = "medlem")  # Skarrild
lst$datLines <- lst$datLines %>% mutate(Group = if_else(str_detect(Text, fixed('clasonborg', ignore_case=TRUE)), "dagkort", Group))
datMarkers <- bind_rows(datMarkers, lst$datMarkers) %>% 
  filter(!is.na(Icon))
datLines <- bind_rows(datLines, lst$datLines)
## Write to csv
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))

