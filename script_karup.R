library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo) 

#### Get a prelim dataset 2003-2019 (data_karup_catch_seatrout_2003-2019.csv) ####
# ## data from 2019 to today
# dat <- fromJSON("https://fangstjournalen.dtu.dk/fangst.nsf/service.xsp?open&assoc=49F1767931B31CD0C1258398007953C0&type=1")
# cols <- dat$data$cols
# cols$label[is.na(cols$label)] <- "Unknown"
# rows <- dat$data$rows$c
# rows <- lapply(rows, FUN = function(x) {x[,1]})
# dat1 <- t(map_dfc(rows, ~ .x))
# colnames(dat1) <- cols$label
# dat1 <- as_tibble(dat1)
# dateStr <- dat1$Dato %>% str_extract_all("(?<=\\().+?(?=\\))", simplify = T) %>%
#   str_split(",", simplify = TRUE) %>% as_tibble(.name_repair = "minimal")
# colnames(dateStr) <- c("Year", "Month", "Day")
# dateStr <- type_convert(dateStr)
# dateStr <- mutate(dateStr, Month = Month + 1)
# dateStr <- str_c(dateStr$Year, "-", str_pad(dateStr$Month, 2, "left", pad="0"), "-", str_pad(dateStr$Day, 2, "left", pad="0"))
# dat1 <- bind_cols(Date=dateStr, dat1)
# dat1 <- dat1 %>% dplyr::filter(str_detect(Art, "Havørred"))
# # tail(dat1)
# # dat1 <- dat1[-c(nrow(dat1)-1,nrow(dat1)),]  # remove last 2 row since error in date
# dat2 <- dat1 %>% transmute(Date, Length = Længde, Weight = Vægt, Name = Navn, Place = Zone, Method = Agn, Cut = FALSE, Foto = Foto, Killed = !as.logical(Genudsat), Sex = Køn)
# dat2 <- type_convert(dat2)
# 
# ## Old data up to 2016 (missing 2017 and 2018)
# dat3 <- read.table(file="fangster_alle_2003-2016.csv", dec=",", sep=";", header = TRUE, stringsAsFactors = FALSE)
# dat3 <- dat3 %>% transmute(Date = dateStr, Length = length, Weight = weight, Name = name, Place = place, Method = lure, Cut = FALSE, Foto = NA, Killed = (Weight > 0 & Length > 0), Sex = NA)
# dat3$Name <- str_c(dat3$Name)
# dat3$Place <- str_c(dat3$Place)
# dat3 <- as_tibble(dat3) %>% type_convert()
# 
# ## Merge and tidy
# dat4 <- bind_rows(dat3,dat2)
# dat4 <- dat4 %>% mutate(Weight = if_else(Length >= 40, Weight, NA_real_))
# # remove outliers
# dat5 <- dplyr::filter(dat4, Length > 39 & Killed)
# mod1 <- lm(log(Weight) ~ log(Length), dat5)
# summary(mod1)
# lim <- 1:max(dat5$Length)
# res <- predict(mod1, data.frame(Length = lim), interval='prediction', level=0.95)
# res <- exp(res)
# res <- res %>% as_tibble()
# res <- res %>% add_column(Length = lim, .before = T)
# colnames(res) <- c("Length", "Avg", "Lower", "Upper")
# res <- res %>% mutate(Upper = Upper + 1.75, Lower = if_else(Lower - 1.75 > 0, Lower - 1.75, 0))
# dat4 <- dat4 %>% mutate(Weight = if_else(Weight >= res$Lower[round(Length)] & Weight <= res$Upper[round(Length)], Weight, NA_real_))
# # res <- pivot_longer(res, 2:4)
# # ggplot(dat4, aes(x = Length, y = Weight)) + geom_point(na.rm = T) +
# #   geom_line(aes(x = Length, y = value, colour = name), data = res)
# dat4$Length <- as.numeric(dat4$Length)
# dat4$Weight<- as.numeric(dat4$Weight)
# dat4 <- dat4 %>% mutate(Fulton = Weight*100000/Length^3)
# dat4 <- dat4 %>% mutate(Killed = if_else(Fulton < 0.2, FALSE, Killed, Killed)) %>% mutate(Fulton = if_else(!Killed, NA_real_, Fulton), Weight = if_else(!Killed, 0, Weight))
# dat4 <- dat4 %>% mutate(Method = str_replace_all(Method, c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", "Mede" = "Orm", "Spinflue" = "Spin")))
# unique(dat4$Method)
# dat4 <- dat4 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
# unique(dat4$Sex)
# dat4 <- dat4 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
# dat4 <- dat4 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
# dat4 <- dat4 %>% mutate(Place = str_replace_all(Place, c("Mellem.*" = "Mellem", "Øvre.*" = "Øvre", "Nedre.*" = "Nedre")))
# unique(dat4$Place)
# 
# ## Save to file
# dat4 <- dat4 %>% dplyr::filter(year(Date)<2020) %>% arrange(Date)
# fn <- "data/data_karup_catch_seatrout_2003-2019.csv"
# write_csv(dat4,fn)
# # write_delim(dat4, fn, delim = ",")


#### Get current catches (data_karup_catch_seatrout_2020-.csv) ####
## data to today
dat <- fromJSON("https://fangstjournalen.dtu.dk/fangst.nsf/service.xsp?open&assoc=49F1767931B31CD0C1258398007953C0&type=1")
cols <- dat$data$cols
cols$label[is.na(cols$label)] <- "Unknown"
rows <- dat$data$rows$c
rows <- lapply(rows, FUN = function(x) {x[,1]})
dat1 <- t(map_dfc(rows, ~ .x))
colnames(dat1) <- cols$label
dat1 <- as_tibble(dat1)
dateStr <- dat1$Dato %>% str_extract_all("(?<=\\().+?(?=\\))", simplify = T) %>%
  str_split(",", simplify = TRUE) %>% as_tibble(.name_repair = "minimal")
colnames(dateStr) <- c("Year", "Month", "Day")
dateStr <- type_convert(dateStr)
dateStr <- mutate(dateStr, Month = Month + 1)
dateStr <- str_c(dateStr$Year, "-", str_pad(dateStr$Month, 2, "left", pad="0"), "-", str_pad(dateStr$Day, 2, "left", pad="0"))
dat1 <- bind_cols(Date=dateStr, dat1)
dat1 <- dat1 %>% dplyr::filter(str_detect(Art, "Havørred"))
dat2 <- dat1 %>% transmute(Date, Length = Længde, Weight = Vægt, Name = Navn, Place = Zone, Method = Agn, Cut = FALSE, Foto = Foto, Killed = !as.logical(Genudsat), Sex = Køn)
dat2 <- type_convert(dat2)

## Merge and tidy
dat4 <- dat2 %>% dplyr::filter(year(Date)>2019)
dat4 <- dat4 %>% mutate(Weight = if_else(Length >= 40, Weight, NA_real_))
# remove outliers
dat5 <- dplyr::filter(dat4, Length > 39 & Killed)
mod1 <- lm(log(Weight) ~ log(Length), dat5)
summary(mod1)
lim <- 1:max(dat5$Length)
res <- predict(mod1, data.frame(Length = lim), interval='prediction', level=0.95)
res <- exp(res)
res <- res %>% as_tibble()
res <- res %>% add_column(Length = lim, .before = T)
colnames(res) <- c("Length", "Avg", "Lower", "Upper")
res <- res %>% mutate(Upper = Upper + 1.75, Lower = if_else(Lower - 1.75 > 0, Lower - 1.75, 0))
dat4 <- dat4 %>% mutate(Weight = if_else(Weight >= res$Lower[round(Length)] & Weight <= res$Upper[round(Length)], Weight, NA_real_))
# res <- pivot_longer(res, 2:4)
# ggplot(dat4, aes(x = Length, y = Weight)) + geom_point(na.rm = T) +
#   geom_line(aes(x = Length, y = value, colour = name), data = res)
dat4$Length <- as.numeric(dat4$Length)
dat4$Weight<- as.numeric(dat4$Weight)
dat4 <- dat4 %>% mutate(Fulton = Weight*100000/Length^3)
dat4 <- dat4 %>% mutate(Killed = if_else(Fulton < 0.2, FALSE, Killed, Killed)) %>% mutate(Fulton = if_else(!Killed, NA_real_, Fulton, NA_real_), Weight = if_else(!Killed, 0, Weight))
dat4 <- dat4 %>% mutate(Method = str_replace_all(Method, c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", "Mede" = "Orm", "Spinflue" = "Spin")))
unique(dat4$Method)
dat4 <- dat4 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
unique(dat4$Sex)
dat4 <- dat4 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
dat4 <- dat4 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
dat4 <- dat4 %>% mutate(Place = str_replace_all(Place, c("Mellem.*" = "Mellem", "Øvre.*" = "Øvre", "Nedre.*" = "Nedre")))
unique(dat4$Place)

## Save to file
fn <- "data/data_karup_catch_seatrout_2020-.csv"
write_csv(dat4, fn)



#### Get prelim data set for waterlevel (data_karup_waterlevel_[year].csv) ####
# stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
# for (y in 2013:2013) {
#   fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
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
#   write_csv(dat, fn)
#   print(range(dat$Date))
# }


#### Tidy prelim water level datasets ####
# readWLevels <- function(years) {
#   colT <- cols(
#     Date = col_datetime(format = ""),
#     'Karup By (054764)' = col_double(),
#     'Hagebro (001762)' = col_double(),
#     'Nørkærbro (001767)' = col_double()
#   )
#   dat <- NULL
#   for (y in years) {
#     fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
#     dat <- bind_rows(dat, read_csv(fn, col_types = colT))
#   }
#   return(dat)
# }
# dat <- readWLevels(2013:2020)
# datL <- dat %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# datS <- datL %>% dplyr::filter(year(Date)>2013)
# ggplot(data = datS, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# 
# # Hagebro seems to have some outliners. Remove outliners:
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
# datL <- dat1 %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# datS <- datL %>% dplyr::filter(year(Date)>2013)
# # ggplot(data = datS, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# 
# ## Nørkærbro seems to have a level shift medio 2018. Remove all data points before:
# dat1 <- dat1 %>% mutate(`Nørkærbro (001767)` = if_else(Date > as_date("2018-05-10"), `Nørkærbro (001767)`, NA_real_))
# datL <- dat1 %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# datS <- datL %>% dplyr::filter(year(Date)>2013)
# ggplot(data = datS, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# 
# ## Save cleaned data
# for (y in 2013:2019) {
#   fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
#   dat2 <- dat1 %>% dplyr::filter(year(Date) == y) %>% arrange(Date)
#   write_csv(dat2, fn)
# }


#### Calc average water level ####
# readWLevels <- function(years) {
#   colT <- cols(
#     Date = col_datetime(format = ""),
#     'Karup By (054764)' = col_double(),
#     'Hagebro (001762)' = col_double(),
#     'Nørkærbro (001767)' = col_double()
#   )
#   dat <- NULL
#   for (y in years) {
#     fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
#     dat <- bind_rows(dat, read_csv(fn, col_types = colT))
#   }
#   return(dat)
# }
# dat <- readWLevels(2013:2020)
# 
# ## Average water leves given day
# dat1 <- dat %>% mutate(Day = yday(Date)) %>% group_by(Day)
# means <- dat1 %>% summarise_if(is.numeric, mean, na.rm = TRUE) 
# means[366,c(2,4)] <- means[365,c(2,4)] 
# colnames(means)[2:4] = paste0(colnames(dat)[2:4]," avg")
# 
# ## Moving average
# movAvg <- function(x, days = 90){ # 96 obs per day
#   n <- days
#   stats::filter(x, rep(1 / n, n), sides = 2, circular = T)
# }
# # rMeans <- mutate_at(means, vars(contains(c('K','H'))), rollmean, k = 30, fill = NA, align = "right")
# rMeans <- mutate_at(means, vars(contains(c('K','H'))), movAvg)
# colnames(rMeans)[2:4] = paste0(colnames(dat)[2:4]," rAvg90")
# means <- full_join(means, rMeans)
# meansL <- means %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# ggplot(data = meansL, aes(x = Day, y = Level)) + geom_line(aes(color = Group), show.legend = T)
# 
# ## Save moving average
# fn <- "data/data_karup_waterlevel_avg90.csv"
# write_csv(rMeans, fn)




#### Get waterlevel for current year ####
y <- year(now())
fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
if (file.exists(fn)) datOld <- read_csv(fn) else datOld <- NULL
stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
dat <- NULL
for (i in 1:nrow(stations)) {
  id <- stations$id[i]
  place <- stations$place[i]
  tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=1&pw=100000000&inclraw=true"))
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
dat$Date <- ymd_hms(dat$Date, tz = "UTC") %>% with_tz("CET") # from UTC to CET
dat <- bind_rows(datOld, dat) 
dat <- dat %>% dplyr::filter(year(Date) == y) %>% arrange() %>% distinct(Date, .keep_all = T)
write_csv(dat, fn)
print(range(dat$Date))



#### Calc average waterlevels over all years ####
colT <- cols(
  Date = col_datetime(format = ""),
  'Karup By (054764)' = col_double(),
  'Hagebro (001762)' = col_double(),
  'Nørkærbro (001767)' = col_double()
)

readWLevels <- function(years) {
  dat <- NULL
  for (y in years) {
    fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
    dat <- bind_rows(dat, read_csv(fn, col_types = colT))
  }
  return(dat)
}
dat <- readWLevels(2013:2013)


y <- year(now())
fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
if (file.exists(fn)) datOld <- read_csv(fn) else datOld <- NULL
stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
dat <- NULL
for (i in 1:nrow(stations)) {
  id <- stations$id[i]
  place <- stations$place[i]
  tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=1&pw=100000000&inclraw=true"))
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
dat$Date <- ymd_hms(dat$Date, tz = "UTC") %>% with_tz("CET") # from UTC to CET
dat <- bind_rows(datOld, dat) 
dat <- dat %>% dplyr::filter(year(Date) == y) %>% arrange() %>% distinct(Date, .keep_all = T)
write_csv(dat, fn)
print(range(dat$Date))



