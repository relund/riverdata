library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)

source("functions.R")


#### Flow though lock at Hvide Sande ####
updateLockSkjern("data/data_skjern_flow_lock.csv")


#### Catch records ####
datCatchSalmon <- updateCatchSkjern("data/data_skjern_catch_salmon.csv", species = "salmon", reset = T, start = 2004)   #, reset = T, start = 2004
datCatchSeatrout <- updateCatchSkjern("data/data_skjern_catch_seatrout.csv", species = "trout")


#### Salmon - Estimate weight given length ####
# dat <- datCatchSalmon %>% 
#   mutate(Period = factor(month(Date, label = T), ordered = F)) 
# datWeightSalmon <- estimateWeight("data/data_skjern_weight_salmon.csv", dat, minLength = 40, maxLength = 145)
# 
# dat <- datCatchSeatrout %>% 
#   mutate(Period = factor(month(Date, label = T), ordered = F)) 
# datWeightSeatrout <- estimateWeight("data/data_skjern_weight_seatrout.csv", dat, minLength = 40)

# datCKilled <- datCatchSalmon %>% 
#   mutate(Length = round(Length), Month = month(Date, label = T)) %>%
#   mutate(Period = factor(Month, ordered = F)) %>% 
#   dplyr::filter(Length > 39 & Killed) 
# mod <- lm(log(Weight) ~ Period*log(Length), datCKilled)
# datP <- expand_grid(Length = 40:145, Period = unique(datCKilled$Period))
# res <- predict(mod, datP, interval='prediction', level=0.95) 
# res <- exp(res)t
# res <- res %>% as_tibble() 
# res <- bind_cols(datP, res) %>% group_by(Period) #%>% select(Length:Avg)
# colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")
# 
# ## Save to file
# fn <- "data/data_skjern_weight_salmon.csv"
# write_csv(res, fn)


#### Seatrout - Estimate weight given length ####
# datCKilled <- datCatchSeatrout %>% 
#   mutate(Length = round(Length), Weight = round(Weight,1), Month = month(Date, label = T)) %>%
#   mutate(Period = factor(Month, ordered = F)) %>% 
#   dplyr::filter(Length > 39 & Killed) 
# mod <- lm(log(Weight) ~ Period*log(Length), datCKilled)
# datP <- expand_grid(Length = 40:max(datCKilled$Length), Period = unique(datCKilled$Period))
# res <- predict(mod, datP, interval='prediction', level=0.95) 
# res <- exp(res)
# res <- res %>% as_tibble() 
# res <- bind_cols(datP, res) %>% group_by(Period) #%>% select(Length:Avg)
# colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")
# 
# ## Save to file
# fn <- "data/data_skjern_weight_seatrout.csv"
# write_csv(res, fn)


# #### Waterlevel - Prelim data set ####
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





#### Waterlevel - Update data current year ####
y <- year(now())
fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
if (file.exists(fn)) datOld <- read_csv(fn) else datOld <- NULL
stations <- 
  tibble(id = c("055416", "055414", "001862", "017898", "054757", "001855", "052386"), 
         place = c("Vorgod Å - Vandmøllen", 
                   "Skjern Å - Sandfeldvej", 
                   "Skjern Å - Alergaard",
                   "Skjern Å - Gjaldbæk bro",
                   "Rind Å - Arnborg kirke",
                   "Omme Å - Sønderskov bro",
                   "Fjederholt Å - A18"))
iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
dat <- NULL
for (i in 1:nrow(stations)) {
  id <- stations$id[i]
  place <- stations$place[i]
  tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=100&pw=100000000&inclraw=true"))
  offset <- as.numeric(tmp$tsh$Offset)
  tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
  tmp$V <- tmp$V - rep(offset, length(tmp$V))
  colnames(tmp) <- c("Date", paste0(place, " (", id, ")"))
  if (is.null(dat)) {
    dat <- tmp
  } else {
    dat <- full_join(dat,tmp, by = "Date")
  }
}
dat$Date <- ymd_hms(dat$Date, tz = "UTC") # %>% with_tz("CET") # from UTC to CET
dat <- bind_rows(datOld, dat)
dat <- dat %>% dplyr::filter(year(Date) == y) %>%
  arrange_all(desc) %>%
  distinct(Date, .keep_all = T)

## write to file
write_csv(dat, fn)
range(dat$Date)



#### Waterlevel - Calc average ####
readWLevels <- function(years) {
  colT <- cols(
    Date = col_datetime(format = ""),
    `Vorgod Å - Vandmøllen (055416)` = col_double(),
    `Skjern Å - Sandfeldvej (055414)` = col_double(),
    `Skjern Å - Alergaard (001862)` = col_double(),
    `Skjern Å - Gjaldbæk bro (017898)` = col_double(),
    `Rind Å - Arnborg kirke (054757)` = col_double(),
    `Omme Å - Sønderskov bro (001855)` = col_double(),
    `Fjederholt Å - A18 (052386)` = col_double()
  )
  dat <- NULL
  cat("Read year:")
  for (y in years) {
    cat(y, "\n")
    fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
    dat <- bind_rows(dat, read_csv(fn, col_types = colT))
  }
  return(dat)
}
dat <- readWLevels(2017:year(now()))

## Average water levels given day
dat1 <- dat %>% mutate(Day = yday(Date)) %>% group_by(Day)
means <- dat1 %>% summarise_if(is.numeric, mean, na.rm = TRUE)
if (nrow(means) == 366) means[366, 2:ncol(means)] <- means[365, 2:ncol(means)]
colnames(means)[2:ncol(means)] = paste0(colnames(dat)[2:ncol(means)]," avg")

## Moving average
movAvg <- function(x, days = 90){ 
  n <- days
  stats::filter(x, rep(1 / n, n), sides = 2, circular = T)
}
rMeans <- mutate_at(means, 2:ncol(means), movAvg)
colnames(rMeans)[2:ncol(rMeans)] = paste0(colnames(dat)[2:ncol(rMeans)]," rAvg90")
rMeans <- rMeans %>% mutate_if(is.numeric, as.numeric)
# means <- full_join(means, rMeans)
# meansL <- means %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# ggplot(data = meansL, aes(x = Day, y = Level)) + geom_line(aes(color = Group), show.legend = T)

## Save moving average
fn <- "data/data_skjern_waterlevel_avg90.csv"
write_csv(rMeans, fn)


#### Waterlevel - Relative datasets ####
# dat <- readWLevels(2017:year(now()))
# rMeans <- read_csv(paste0(prefix,"data_skjern_waterlevel_avg90.csv"))
dat <- dat %>% mutate(Day = yday(Date))
dat <- left_join(dat, rMeans)
tmp <-  map(2:ncol(rMeans),  function(i) {
    return(unlist(dat[, i] - dat[, i + ncol(rMeans)]))
  }) 
tmp
names(tmp) <- str_remove_all(colnames(rMeans)[2:ncol(rMeans)], " \\(.*") 
tmp <- as_tibble(tmp)
dat <- bind_cols(select(dat, Date), tmp)
datL <- dat %>% pivot_longer(cols = 2:ncol(dat), names_to = 'Place', values_to = 'Level')
for (y in 2017:year(now())) {
  fn <- paste0("data/data_skjern_waterlevel_relative_long_", y, ".csv")
  write_csv(dplyr::filter(datL, year(Date) == y), fn)
}

