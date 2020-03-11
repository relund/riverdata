library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)

conflict_prefer("filter", "dplyr")

### Get a prelim dataset 2003-2019 (data_karup_catch_seatrout_2003-2019.csv)
### --------------------------------------------------------------------------------------------
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
# dat1 <- dat1[-c(nrow(dat1)-1,nrow(dat1)),]  # remove last 2 row since error in date
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
# dat4 <- dat4 %>% mutate(Weight = ifelse(Length >= 40, Weight, NA))
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
# dat4 <- dat4 %>% mutate(Weight = ifelse(Weight >= res$Lower[round(Length)] & Weight <= res$Upper[round(Length)], Weight, NA))
# # res <- pivot_longer(res, 2:4)
# # ggplot(dat4, aes(x = Length, y = Weight)) + geom_point(na.rm = T) + 
# #   geom_line(aes(x = Length, y = value, colour = name), data = res) 
# dat4$Length <- as.numeric(dat4$Length)
# dat4$Weight<- as.numeric(dat4$Weight)
# dat4 <- dat4 %>% mutate(Fulton = Weight*100000/Length^3)
# dat4 <- dat4 %>% mutate(Killed = if_else(Fulton < 0.2, FALSE, Killed)) %>% mutate(Fulton = ifelse(!Killed, NA, Fulton), Weight = ifelse(!Killed, 0, Weight))
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
# dat4 <- dat4 %>% filter(year(Date)<2020) %>% arrange(Date)
# fn <- "data/data_karup_catch_seatrout_2003-2019.csv"
# write_csv2(dat4, fn)
### --------------------------------------------------------------------------------------------


### Get current catches (data_karup_catch_seatrout_2020-.csv)
### --------------------------------------------------------------------------------------------
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
# tail(dat1)
dat1 <- dat1[-c(nrow(dat1)-1,nrow(dat1)),]  # remove last 2 row since error in date
dat2 <- dat1 %>% transmute(Date, Length = Længde, Weight = Vægt, Name = Navn, Place = Zone, Method = Agn, Cut = FALSE, Foto = Foto, Killed = !as.logical(Genudsat), Sex = Køn)
dat2 <- type_convert(dat2)

## Merge and tidy
dat4 <- dat2 %>% filter(year(Date)>2019)
dat4 <- dat4 %>% mutate(Weight = ifelse(Length >= 40, Weight, NA))
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
dat4 <- dat4 %>% mutate(Weight = ifelse(Weight >= res$Lower[round(Length)] & Weight <= res$Upper[round(Length)], Weight, NA))
# res <- pivot_longer(res, 2:4)
# ggplot(dat4, aes(x = Length, y = Weight)) + geom_point(na.rm = T) +
#   geom_line(aes(x = Length, y = value, colour = name), data = res)
dat4$Length <- as.numeric(dat4$Length)
dat4$Weight<- as.numeric(dat4$Weight)
dat4 <- dat4 %>% mutate(Fulton = Weight*100000/Length^3)
dat4 <- dat4 %>% mutate(Killed = if_else(Fulton < 0.2, FALSE, Killed)) %>% mutate(Fulton = ifelse(!Killed, NA, Fulton), Weight = ifelse(!Killed, 0, Weight))
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
write_csv2(dat4, fn)
### --------------------------------------------------------------------------------------------


