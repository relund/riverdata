# Create data set 2003-2016 (REMEMBER TO SET WD TO FILE LOC)
library(tidyverse)
library(lubridate)
library(readxl)

## Old data up to 2016 (with some wrong dates)
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

## Save to file
dat4 <- dat4 %>% dplyr::filter(year(Date)<2020) %>% arrange(Date)
fn <- "../../data/data_karup_catch_seatrout_2003-2019.csv"
write_csv(dat4,fn)



