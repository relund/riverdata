library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)


## Flow though lock at Hvide Sande
url <- 'http://hyde.dk/Sflow/default_flow.asp'
webpage <- read_html(url)
dat <- html_nodes(webpage, xpath = "/html/body/div[2]/div[3]/script/text()")
dat <- html_text(dat)
dat <- str_replace_all(dat, "[\r\n\t]", "")

dates <- str_replace(dat, "^.*labels:", "{\n'labels' :")
dates <- str_replace(dates, "].*$", "]\n}")
dates <- str_replace_all(dates, "'", '"')
dates <- fromJSON(dates)$labels
# dates

flow <- str_replace(dat, "^.*data:", "{\n'data' :")
flow <- str_replace(flow, "].*$", "]\n}")
flow <- str_replace_all(flow, "'", '"')
flow <- fromJSON(flow)$data
# flow

if (length(dates)==length(flow)) {
  fn <- "data/data_skjern_flow_lock.csv"
  dat <- tibble(dates = dmy_hm(dates, tz = "CET"), flow = flow)  %>% 
    arrange_all(desc) %>% distinct()
  if (!file.exists(fn)) {
    write_csv(dat, fn)
    dat
  } else {
    datOld <- read_csv(fn, col_types = "Ti", locale = locale(tz = "CET"))
    dat <- bind_rows(datOld,dat) %>% arrange_all(desc) %>% distinct()
    dat
    write_csv(dat, fn)
  }
}

# ggplot(dat, aes(x = dates, y = flow)) + geom_line() + labs(x = "Dato", y = "Flow (m3/s)", title = "Sluseflow Hvide Sande")

# ## Wind, water levet etc at Hvide Sande
# url <- 'http://hyde.dk/'
# webpage <- read_html(url)
# dat <- html_nodes(webpage, xpath = "/html/body/div[4]/div[2]/div[1]/div[4]/p[2]/text()")
# dat <- html_text(dat)
# dat <- str_replace_all(dat, "[\r\n\t]", "")
# 
# dates <- str_replace(dat, "^.*labels:", "{\n'labels' :")
# dates <- str_replace(dates, "].*$", "]\n}")
# dates <- str_replace_all(dates, "'", '"')
# dates <- fromJSON(dates)$labels
# # dates
# 
# flow <- str_replace(dat, "^.*data:", "{\n'data' :")
# flow <- str_replace(flow, "].*$", "]\n}")
# flow <- str_replace_all(flow, "'", '"')
# flow <- fromJSON(flow)$data
# # flow
# 
# if (length(dates)==length(flow)) {
#   fn <- "data_flow_sluse.csv"
#   dat <- tibble(dates = dmy_hm(dates, tz = "CET"), flow = flow)
#   if (!file.exists(fn)) {
#     write_csv2(dat, fn)
#   } else {
#     datOld <- read_csv2(fn, col_types = "Ti", locale = locale(tz = "CET"))
#     dat <- bind_rows(datOld,dat)
#     dat <- distinct(dat)
#     write_csv2(dat, fn)
#   }
# }


### Catch records
# curY <- as.numeric(format(Sys.time(), "%Y"))
# dat <- NULL
# # for (y in 2004:curY) {
# #   url <- str_c("http://skjernaasam.dk/catchreport/?getyear=", y, "&species=salmon")
# #   page <- read_html(url)
# #   x <- html_node(page, xpath = '//*[@id="report-list"]')
# #   tmp <- html_table(x, header = T, trim = T, fill = T) %>% as_tibble()
# #   dat <- bind_rows(tmp, dat)
# # }
# url <- str_c("http://skjernaasam.dk/catchreport/?getyear=", curY, "&species=salmon")
# page <- read_html(url)
# x <- html_node(page, xpath = '//*[@id="report-list"]')
# dat <- html_table(x, header = T, trim = T, fill = T) %>% as_tibble()
# colnames(dat) <- c("Date", "Length", "Weight", "Name", "Place", "Method", "Cut", "Foto")
# dat <- dat %>% mutate(killed = str_detect(dat$Date, fixed("*")))
# dat$Date <- dat$Date %>% str_replace(coll(" *"), "")
# dat$Length <- dat$Length %>% str_replace_all(c(".cm" = "", "Ukendt" = "")) 
# dat$Weight <- dat$Weight %>% str_replace_all(c(".kg" = "", "Ukendt" = "")) 
# dat$Cut <- dat$Cut %>% str_replace_all(c("Ja" = "T", "Nej" = "F", "Ukendt" = "F", ".*pelvic" = "T", 
#                                          "tail" = "T", "pectoral" = "T", "anal" = "T"))
# dat$Foto <- dat$Foto %>% str_replace_all(c("ja" = "TRUE", "nej" = "FALSE", "Ukendt" = ""))
# dat$Place <- dat$Place %>% str_replace_all(c(".*Fjord.*" = "Nedre", ".*Opstrøms Tarp bro.*" = "Øvre", ".*Vorgod Å.*" = "Vorgod Å", "Skjern Å: Borris Krog bro.*" = "Mellem", "Omme Å.*" = "Omme Å", "Skjern Å: Skarrild" = "Øvre", "Ukendt" = ""))
# dat <- type_convert(dat)
# dat$Length <- as.numeric(dat$Length)
# dat$Weight<- as.numeric(dat$Weight)
# dat <- dat %>% mutate(Fulton = Weight*100000/Length^3)
# dat
# 
# fn <- "data_catch_salmon.csv"
# if (!file.exists(fn)) {
#   write_csv2(dat, fn)
# } else {
#   datOld <- read_csv2(fn)
#   dat <- bind_rows(datOld,dat)
#   dat <- distinct(dat)
#   write_csv2(dat, fn)
# }


### Estimate weight based on length
# dat1 <- dplyr::filter(dat, Length > 39 & Length < 141)
# qplot(Length, Weight, data = dat1)
# 
# 
# mod <- lm(Weight ~ -1 + Length + I(Length^2) + I(Length^3), dat1)
# par(mfrow=c(2,2))
# lim <- 0:140
# plot(mod)
# #summary(mod)
# predicted.intervals <- predict(mod, data.frame(Length=lim), interval='confidence', level=0.95)
# plot(dat1$Length, dat1$Weight, xlim = c(0,140))
# lines(lim, predicted.intervals[,1],col='green',lwd=3)
# 
# mod1 <- lm(log(Weight) ~ log(Length), dat1)
# par(mfrow=c(2,2))
# plot(mod1)
# summary(mod1)
# predicted.intervals <- predict(mod1, data.frame(Length=lim), interval='prediction', level=0.95)
# par(mfrow=c(1,1))
# lim <- 0:140
# plot(dat1$Length, dat1$Weight, xlim = c(0,140))
# lines(lim, exp(predicted.intervals[,1]),col='green',lwd=3)
# lines(lim, exp(predicted.intervals[,2]),col='blue',lwd=1)
# lines(lim, exp(predicted.intervals[,3]),col='blue',lwd=1)
# 
# ddply(dat1, .(Length), summarise, Weight = mean(Weight, na.rm = T))
# 





# reformat date 
# dat$dateStr<-timeToStr(strToTime(dat$dateStr, f = "%d-%m-%Y"))
# 
# #dat$unixTime<-unixTime(dat$date)
# #dat$dateStr<-as.character(dat$date)
# head(dat)
# dim(dat)


