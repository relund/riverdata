library(xml2)
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo) 
source("functions.R")

#### Catch records ####
dat4 <- writeCatchKarup()

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





#### Waterlevel ####
stations <- tibble(id = c("054764", "001762", "001767"), place = c("Karup By", "Hagebro", "Nørkærbro"))
prefix <- "data/data_karup_waterlevel"
## Update data current year
updateWaterLevel(stations, prefix)    

## Calc moving average 
dat <- readWLevels(prefix, 2013:year(now()))
rMeans <- calcWaterMovAvg(dat, prefix)

## Relative datasets 
dat <- calcWaterLevelRelative(dat, rMeans, prefix)

## Dataset for web 
dat <- calcWaterLevelsWeb(dat, prefix)
