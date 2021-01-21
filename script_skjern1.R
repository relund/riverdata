library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
source("functions.R")

prefix <- "data/data_skjern"

#### Flow though lock at Hvide Sande ####
updateLockSkjern(prefix)

#### Catch records ####

if (day(now()) == 19) {
  datCatchSalmon <- updateCatchSkjern(prefix, species = "salmon", reset = TRUE, start = 2020)   #, reset = T, start = 2004
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


#### Waterlevel ####
stations <- 
  tibble(id = c("055416", "055414", "001862", "017898", "054757", "001855", "052386"), 
         place = c("Vorgod Å - Vandmøllen", 
                   "Skjern Å - Sandfeldvej", 
                   "Skjern Å - Alergaard",
                   "Skjern Å - Gjaldbæk bro",
                   "Rind Å - Arnborg kirke",
                   "Omme Å - Sønderskov bro",
                   "Fjederholt Å - A18"))
## Update data current year
updateWaterLevel(stations, prefix)    

## Calc moving average 
dat <- readWLevels(prefix, 2017:year(now()))
rMeans <- calcWaterMovAvg(dat, prefix)

## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)

## Dataset for web 
dat <- calcWaterLevelsWeb(dat, prefix)






