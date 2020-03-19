# Create data set 2003-2019 (REMEMBER TO SET WD TO FILE LOC)
library(tidyverse)
library(lubridate)
library(readxl)
library(rvest)
library(jsonlite)

### Get a prelim dataset up to 2016 ####
dat <- read.table(file="fangster_alle_2003-2016.csv", dec=",", sep=";", header = TRUE, stringsAsFactors = FALSE)
dat <- dat %>% transmute(Date = dateStr, Length = length, Weight = weight, Name = name, Place = place, Method = lure, Cut = FALSE, Foto = NA, Killed = (Weight > 0 & Length > 0), Sex = NA)
dat$Name <- str_c(dat$Name)
dat$Place <- str_c(dat$Place)
dat <- as_tibble(dat) %>% type_convert() %>% mutate(Month = month(Date, label = T), Quarter = quarter(Date), id = 0)
# View(dplyr::filter(dat, Month %in% c("jan")))  # looks like month and day switched for Herning data

# Herning data
navne <- read_excel("hsf_fangster.xlsx")$Navn  # get in utf8
dat1 <- read.table(file="hsf_fangster.csv", dec=".", sep=";", header = TRUE, stringsAsFactors = FALSE)
dat1 <- as_tibble(dat1) %>% type_convert() %>% 
  transmute(Date = dmy(Dato), Length = Lgd, Weight = V.gt, Name = navne, 
            Place = "Øvre", Method = Agn, Cut = FALSE, Foto = NA, Killed = T, Sex = NA) %>% 
  mutate(Month = month(Date, label = T), Quarter = quarter(Date), id = 1) 

dat2 <- bind_rows(dat, dat1) %>% arrange(Name, Length, Weight, Date)

## Tidy
dat4 <- dat2
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
res <- pivot_longer(res, 2:4)
ggplot(dat4, aes(x = Length, y = Weight)) + geom_point(na.rm = T) +
  geom_line(aes(x = Length, y = value, colour = name), data = res)
dat4$Length <- as.numeric(dat4$Length)
dat4$Weight<- as.numeric(dat4$Weight)
dat4 <- dat4 %>% mutate(Fulton = Weight*100000/Length^3)
dat4 <- dat4 %>% mutate(Killed = if_else(Fulton < 0.2, FALSE, Killed, Killed)) %>% mutate(Fulton = if_else(!Killed, NA_real_, Fulton), Weight = if_else(!Killed, 0, Weight))
dat4 <- dat4 %>% mutate(Method = str_replace_all(Method, c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", "Mede" = "Orm", "Spinflue" = "Spin")))
unique(dat4$Method)
dat4 <- dat4 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
unique(dat4$Sex)
dat4 <- dat4 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
dat4 <- dat4 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
dat4 <- dat4 %>% mutate(Place = str_replace_all(Place, c("Mellem.*" = "Mellem", "Øvre.*" = "Øvre", "Nedre.*" = "Nedre")))
unique(dat4$Place)

# Find dublicates
dat4 <- dat4 %>% mutate(Day = day(Date), Mt = month(Date), Year = year(Date))
nrow(dat4)
dat4 <- dat4 %>% distinct(Date, Name, Length, Weight, Place, .keep_all = T) %>% rownames_to_column()
nrow(dat4)
dub <- dat4 %>% group_by(Date, Name, Length, Place) %>% dplyr::filter(n()>1, is.na(Weight)) %>% arrange(Date, Name, Length, Place)
View(dub)
dat4 <- dat4[-as.integer(dub$rowname),]

# Switch day and month if not in season
dat4 <- dat4 %>% mutate(DateStr = ifelse(Mt %in% c(1,2,11,12), paste0(Year, "-", Day, "-", Mt), paste0(Year, "-", Mt, "-", Day))) %>% 
  mutate(Date = ymd(DateStr), Mt = month(Date)) 
dat4 <- dat4 %>% dplyr::filter(!(Mt %in% c(1,2,11,12))) %>% 
  select(-rowname, -Month, -Quarter, -id, -Day, -Mt, -Year, -DateStr)
dat4 %>% dplyr::filter(month(Date) %in% c(1,2,11,12))
dat2016 <- dat4


### Get a prelim dataset 2017-2019 ####

## data from 2019 to today
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
# dat1 <- dat1[-c(nrow(dat1)-1,nrow(dat1)),]  # remove last 2 row since error in date
dat2 <- dat1 %>% transmute(Date, Length = Længde, Weight = Vægt, Name = Navn, Place = Zone, Method = Agn, Cut = FALSE, Foto = Foto, Killed = !as.logical(Genudsat), Sex = Køn)
dat2 <- type_convert(dat2) %>% arrange(Date)

## Old data up to 2017-2019
dat31 <- read_excel("kaas_fangster_2017-2019_short.xlsx")
dat31 <- dat31 %>% dplyr::filter(Catch == "Havørred") %>% 
  transmute(Date = as_date(format(Date, format = "%Y-%m-%d")), 
            Length = Length, Weight = Weight, Name = Name, Place = Where, 
            Method = Lure, Cut = FALSE, Foto = NA, Killed = TRUE, Sex = NA) 

dat3 <- bind_rows(dat2, dat31) %>% arrange(Date, Name)

## Merge and tidy
dat4 <- bind_rows(dat3,dat2) %>%   
  mutate(Killed = if_else(Weight == 0, FALSE, Killed)) %>% 
  mutate(Weight = if_else(Weight == 0, NA_real_, Weight))
dat4 <- dat4 %>% mutate(Weight = if_else(Length >= 40, Weight, NA_real_))
dat4 %>% dplyr::filter(is.na(Length))
idx <- which(is.na(dat4$Length))
idx <- c(idx-1, idx, idx+1)
dat4[sort(idx),]
dat4 <- dat4 %>% dplyr::filter(!is.na(Length)) %>% dplyr::filter(Length>=40)
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
res <- pivot_longer(res, 2:4)
ggplot(dat4, aes(x = Length, y = Weight)) + geom_point(na.rm = T) +
  geom_line(aes(x = Length, y = value, colour = name), data = res)
dat4$Length <- as.numeric(dat4$Length)
dat4$Weight<- as.numeric(dat4$Weight)
dat4 <- dat4 %>% mutate(Fulton = Weight*100000/Length^3)
dat4 <- dat4 %>% mutate(Killed = if_else(Fulton < 0.2, FALSE, Killed, Killed)) %>% mutate(Fulton = if_else(!Killed, NA_real_, Fulton), Weight = if_else(!Killed, NA_real_, Weight))
dat4 <- dat4 %>% mutate(Method = str_replace_all(Method, c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", "Mede" = "Orm", "Spinflue" = "Spin")))
unique(dat4$Method)
dat4 <- dat4 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
unique(dat4$Sex)
dat4 <- dat4 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
dat4 <- dat4 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
dat4 <- dat4 %>% mutate(Place = str_replace_all(Place, c("Mellem.*" = "Mellem", "Øvre.*" = "Øvre", "Nedre.*" = "Nedre")))
unique(dat4$Place)

# Find dublicates
dat4 <- dat4 %>% mutate(Day = day(Date), Mt = month(Date), Year = year(Date))
nrow(dat4)
dat4 <- dat4 %>% distinct(Date, Name, Length, Weight, Place, .keep_all = T) %>% rownames_to_column()
nrow(dat4)
dub <- dat4 %>% group_by(Date, Name, Length, Place) %>% dplyr::filter(n()>1, is.na(Weight)) %>% arrange(Date, Name, Length, Place)
View(dub)
dat4 <- dat4[-as.integer(dub$rowname),]

# Switch day and month if not in season
dat4 <- dat4 %>% mutate(DateStr = ifelse(Mt %in% c(1,2,11,12), paste0(Year, "-", Day, "-", Mt), paste0(Year, "-", Mt, "-", Day))) %>% 
  mutate(Date = ymd(DateStr), Mt = month(Date)) 
dat4 <- dat4 %>% dplyr::filter(!(Mt %in% c(1,2,11,12))) %>% 
  select(-rowname, -Day, -Mt, -Year, -DateStr)
dat4 %>% dplyr::filter(month(Date) %in% c(1,2,11,12))

dat4 <- bind_rows(dat2016, dat4)

## Save to file
dat4 <- dat4 %>% arrange(Date, Name, Weight, Length, Place) %>% 
  dplyr::filter(year(Date)<2020) 
fn <- "../../data/data_karup_catch_seatrout_2003-2019.csv"
write_csv(dat4,fn)



