library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo) 
source("functions.R")

#### Catch records ####
datCatchSeatrout <- writeCatchKarup()


#### Waterlevel - Update data current year ####
fn <- paste0("data/data_karup_waterlevel_long_", year(now()), ".csv")
stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
updateWaterLevel(fn, stations)












#### Estimate weight given length ####
prefix <- "https://raw.githubusercontent.com/relund/skjern/master/data/"
readGHCatch <- function(file) {
  colT <- cols(
    Date = col_date(format = ""),
    Length = col_double(),
    Weight = col_double(),
    Name = col_character(),
    Place = col_character(),
    Method = col_character(),
    Cut = col_logical(),
    Foto = col_character(),
    Killed = col_logical(),
    Sex = col_character(),
    Fulton = col_double()
  )
  read_csv(paste0(prefix, file), col_types = colT)
}
datCatch <- readGHCatch("data_karup_catch_seatrout_2003-2019.csv")
datCatch <- bind_rows(datCatch, dat4) %>% mutate(Weight = if_else(Killed, Weight, NA_real_))
datCatch <- datCatch %>% 
  mutate(Length = round(Length), Weight = round(Weight,1), Month = month(Date, label = T)) %>% 
  mutate(Period = case_when(
    Month == "Apr" ~ "Apr-Aug",
    Month == "May" ~ "Apr-Aug",
    Month == "Jun" ~ "Apr-Aug",
    Month == "Jul" ~ "Apr-Aug",
    Month == "Aug" ~ "Apr-Aug",
    Month == "Sep" ~ "Sep-Okt",
    Month == "Oct" ~ "Sep-Okt",
    Month == "Mar" ~ "Marts",
  )) 
datCKilled <- dplyr::filter(datCatch, Length > 39 & Killed) 
mod <- lm(log(Weight) ~ Period*log(Length), datCKilled)
datP <- expand_grid(Length = 40:max(datCKilled$Length), Period = unique(datCKilled$Period))
res <- predict(mod, datP, interval='prediction', level=0.95) 
res <- exp(res)
res <- res %>% as_tibble() 
res <- bind_cols(datP, res) %>% group_by(Period) #%>% select(Length:Avg)
colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")

## Save to file
fn <- "data/data_karup_weight_seatrout.csv"
write_csv(res, fn)


#### Save catches for web ####
tabCatch <- left_join(datCatch, res, by = c("Length", "Period")) %>% 
  mutate(NoWeight = is.na(Weight), Weight = if_else(is.na(Weight), Avg, Weight)) %>% 
  mutate(Fulton = Weight*100000/Length^3)
tmp <- transmute(tabCatch, 
                 K = if_else(!Killed, '<img src="www/c_and_r.gif" alt="C&R">', "", ""),
                 M = if_else(Sex == 'Male', '<img src="www/boy.gif" alt="Han">', "", ""),
                 F = if_else(Sex == 'Female', '<img src="www/girl.gif" alt="Hun">', "", ""),
                 C = if_else(!is.na(Foto), paste0('<a href="', Foto, '", target="_blank"><img src="www/foto.gif" alt="Foto"></a>'), "", "")
) %>% transmute(Misc = paste0('<span>',K,M,F,C,'</span>'))
tabCatch <- bind_cols(tabCatch, tmp) %>% dplyr::filter(Length > 39) %>% 
  select(Date, Name, Length, Weight, Method, Place, Fulton, Misc, NoWeight) %>% 
  mutate(Weight = round(Weight,1)) %>% arrange(desc(Date))

## Save to file
fn <- "data/data_karup_catch_seatrout_web.csv"
write_csv(tabCatch, fn)



#### Save montly statistics for current year ####
datStat <- datCatch %>% 
  dplyr::filter(year(Date) == year(now()), Length > 39) %>% 
  group_by(Month) %>% nest() %>% 
  mutate(
    TotalStat = map(data, function(df) {
      summarise(df, Total = n(), 
                Female = sum(Sex == "Female", na.rm = T), 
                Male = sum(Sex == "Male", na.rm = T),
                SexUnknown = Total - Female - Male,
                Released = sum(!Killed, na.rm = T),
                Killed = sum(Killed, na.rm = T),
                KilledUnknown = Total - Released - Killed,
                LengthAvg = mean(Length, na.rm = T), 
                LengthMax = max(Length, na.rm = T),
                WeightAvg = mean(Weight, na.rm = T), 
                WeightMax = max(Weight, na.rm = T),
                Kg = sum(Weight, na.rm = T),
                FultonAvg = mean(Fulton, na.rm = T), 
                FultonMax = max(Fulton, na.rm = T)
      )
    }),
    PlaceStat = 
      map(data, 
          function(df) {
            df %>% 
            group_by(Place) %>% 
            summarize(TotalPlace = n())}) 
  )

datStat <- datStat %>% 
  mutate(PlaceStat = 
           map(PlaceStat, function(df) {  
             pivot_wider(df, names_from = Place, values_from = TotalPlace)})) %>% 
  unnest(cols = c(TotalStat, PlaceStat)) %>% select(-data) %>% 
  rename_at(vars(contains(c("Nedre","Mellem","Øvre","Haderup Å"))), 
    function(x) {
      case_when(
        x == "Nedre" ~ "Lower",
        x == "Mellem" ~ "Middle",
        x == "Øvre" ~ "Upper",
        x == "Haderup Å" ~ "Haderis",
        TRUE ~ "FEJL"
      )
    }) %>% 
  replace(., is.na(.), 0)

## Save to file
fn <- "data/data_karup_catch_seatrout_stat_month.csv"
write_csv(datStat, fn)




#### Save yearly statistics ####
datStat <- datCatch %>% mutate(Year = year(Date)) %>% group_by(Year) %>% nest() %>% 
  mutate(
    TotalStat = map(data, function(df) {
      summarise(df, Total = n(), 
                Female = sum(Sex == "Female", na.rm = T), 
                Male = sum(Sex == "Male", na.rm = T),
                SexUnknown = Total - Female - Male,
                Released = sum(!Killed, na.rm = T),
                Killed = sum(Killed, na.rm = T),
                KilledUnknown = Total - Released - Killed,
                LengthAvg = mean(Length, na.rm = T), 
                LengthMax = max(Length, na.rm = T),
                WeightAvg = mean(Weight, na.rm = T), 
                WeightMax = max(Weight, na.rm = T),
                Kg = sum(Weight, na.rm = T),
                FultonAvg = mean(Fulton, na.rm = T), 
                FultonMax = max(Fulton, na.rm = T)
      )
    }),
    PlaceStat = 
      map(data, 
          function(df) {
            df %>% 
              group_by(Place) %>% 
              summarize(TotalPlace = n())}) 
  )

datStat <- datStat %>% 
  mutate(PlaceStat = 
           map(PlaceStat, function(df) {  
             pivot_wider(df, names_from = Place, values_from = TotalPlace)})) %>% 
  unnest(cols = c(TotalStat, PlaceStat)) %>% select(-data) %>% 
  rename_at(vars(contains(c("Nedre","Mellem","Øvre","Haderup Å"))), 
            function(x) {
              case_when(
                x == "Nedre" ~ "Lower",
                x == "Mellem" ~ "Middle",
                x == "Øvre" ~ "Upper",
                x == "Haderup Å" ~ "Haderis",
                TRUE ~ "FEJL"
              )
            }) %>% 
  replace(., is.na(.), 0)

## Save to file
fn <- "data/data_karup_catch_seatrout_stat_year.csv"
write_csv(datStat, fn)




#### Update data for waterlevel in current year ####
y <- year(now())
fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
if (file.exists(fn)) datOld <- read_csv(fn) else datOld <- NULL
stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
dat <- NULL
for (i in 1:nrow(stations)) {
  id <- stations$id[i]
  place <- stations$place[i]
  tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=60&pw=100000000&inclraw=true"))
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
# unique(date(dat$Date))
range(dat$Date)





#### Calc average water level ####
readWLevels <- function(years) {
  colT <- cols(
    Date = col_datetime(format = ""),
    'Karup By (054764)' = col_double(),
    'Hagebro (001762)' = col_double(),
    'Nørkærbro (001767)' = col_double()
  )
  dat <- NULL
  for (y in years) {
    fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
    dat <- bind_rows(dat, read_csv(fn, col_types = colT))
  }
  return(dat)
}
dat <- readWLevels(2013:year(now()))

## Average water leves given day
dat1 <- dat %>% mutate(Day = yday(Date)) %>% group_by(Day)
means <- dat1 %>% summarise_if(is.numeric, mean, na.rm = TRUE)
means[366,c(2,4)] <- means[365,c(2,4)]
colnames(means)[2:4] = paste0(colnames(dat)[2:4]," avg")

## Moving average
movAvg <- function(x, days = 90){ # 96 obs per day
  n <- days
  stats::filter(x, rep(1 / n, n), sides = 2, circular = T)
}
# rMeans <- mutate_at(means, vars(contains(c('K','H'))), rollmean, k = 30, fill = NA, align = "right")
rMeans <- mutate_at(means, vars(contains(c('K','H'))), movAvg)
colnames(rMeans)[2:4] = paste0(colnames(dat)[2:4]," rAvg90")
# means <- full_join(means, rMeans)
# meansL <- means %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# ggplot(data = meansL, aes(x = Day, y = Level)) + geom_line(aes(color = Group), show.legend = T)

## Save moving average
fn <- "data/data_karup_waterlevel_avg90.csv"
write_csv(rMeans, fn)




#### Relative water level datasets ####
datWLevel <- readWLevels(2013:year(now()))
rMeans <- read_csv(paste0(prefix,"data_karup_waterlevel_avg90.csv"))
datWLevel <- datWLevel %>% mutate(Day = yday(Date))
datWLevel <- left_join(datWLevel, rMeans)
datS <- datWLevel %>% dplyr::filter(Date > now() - days(14))
datWLevel <- datWLevel %>% mutate(`Karup By` = `Karup By (054764)` - `Karup By (054764) rAvg90`,
                              `Hagebro` = `Hagebro (001762)` - `Hagebro (001762) rAvg90`,
                              `Nørkærbro` = `Nørkærbro (001767)` - `Nørkærbro (001767) rAvg90`) %>%
  select(Date, `Karup By`, `Hagebro`, `Nørkærbro`)
datWLevel <- datWLevel %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Place', values_to = 'Level')
for (y in 2013:year(now())) {
  fn <- paste0("data/data_karup_waterlevel_relative_long_", y, ".csv")
  write_csv(dplyr::filter(datWLevel, year(Date) == y), fn)
}



