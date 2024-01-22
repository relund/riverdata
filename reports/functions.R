#' Merge two lists to one
#'
#' @param a First list.
#' @param b Second list (priority).
mergeLists <- function (a,b) {
  c(a[setdiff(names(a), names(b))], b)
}


#' Read catch records from a DTU json file 
#'
#' @param url Url for json (without year) ending with a slash.
#'
#' @return The data (tibble).
readCatchDTU <- function(url) {
  message("Read catch records")
  ## data to today
  dat <- fromJSON(url)
  cols <- dat$cols
  cols$label[is.na(cols$label)] <- "Unknown"
  rows <- dat$rows$c
  rows <- lapply(rows, FUN = function(x) {x[,1]})
  dat1 <-  suppressMessages(t(map_dfc(rows, ~ .x)))
  colnames(dat1) <- cols$label
  dat1 <- suppressMessages(as_tibble(dat1, .name_repair = "universal"))
  dateStr <- dat1$Dato %>% str_extract_all("(?<=\\().+?(?=\\))", simplify = T) %>%
    str_split(",", simplify = TRUE) %>% as_tibble(.name_repair = "minimal")
  colnames(dateStr) <- c("Year", "Month", "Day")
  dateStr <- suppressMessages(type_convert(dateStr))
  dateStr <- mutate(dateStr, Month = Month + 1)
  dateStr <- str_c(dateStr$Year, "-", str_pad(dateStr$Month, 2, "left", pad="0"), "-", str_pad(dateStr$Day, 2, "left", pad="0"))
  dat1 <- suppressMessages(bind_cols(Date=dateStr, dat1)) 
  dat1 <- dat1 %>% 
    rename(River = `Unknown...4`)
  dat2 <- dat1 %>% 
    transmute(Date, River, Place = Strækning.sted, Length = `Længde`, Weight = `Vægt`, Name = Navn, Method = Agn, 
              Cut = NA_character_, Foto = Foto, Killed = (Hjemtaget == "Ja"), Sex = `Køn`, Net = Garnskadet)
  if ("Fedtfinne.klippet" %in% colnames(dat1)) dat2$Cut = dat1$Fedtfinne.klippet
  dat2 <- suppressMessages(type_convert(dat2))
  
  ### Merge and tidy
  dat3 <- dat2 %>% mutate(Weight = if_else(Length >= 40, Weight, NA_real_)) %>% 
    filter(Length >= 40 | is.na(Length))
  ## Fix custom errors
  dat3 <-dat3 %>% 
    mutate(Method = str_replace_all(Method, 
                                    c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", 
                                      "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", 
                                      "Mede" = "Orm", "Spinflue" = "Spin", "Spin-flue" = "Spin", "Maddike" = "Orm", 
                                      "Orm, spinner" = "Orm", "Orm,spin" = "Orm"))
    )
  # dat3 <-dat3 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
  dat3 <-dat3 %>% mutate(Sex = str_replace_all(Sex, c("Ved ikke" = NA_character_)))
  dat3 <-dat3 %>% mutate(Cut = if_else(Cut == "Ja", TRUE, if_else(Cut == "Nej", FALSE, NA)))
  # unique(dat3$Sex)
  dat3 <-dat3 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
  dat3 <-dat3 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
  # unique(dat3$Place)
  
  return(dat3)
}



#' Read and prepare catch records for table
#'
#' @param prefix Prefix to csv files e.g. "../../data/data_skjern_catch_salmon".
#' @param datWeight Weight estimates
#'
#' @return The catch records
readCatchOld <- function(prefix, datWeight) {
  f <- paste0(prefix, "_", 2003:year(now()), ".csv")
  datCatch <- read_csv(f, col_types = "Dddcfflclf")  %>% 
    mutate(Weight = if_else(Killed, Weight, NA_real_), Place = fct_na_value_to_level(Place, "Ukendt"))
  
  datCatch <- read_csv(path, col_types = "DddcfffllcclcfTd") %>% 
    mutate(
      Misc = paste0(
        if_else(!Killed, "<img src=\"www/c_and_r.gif\" alt=\"C&R\">", "", ""),
        if_else(Cut, "<img src=\"www/cut.gif\" alt=\"Finneklippet\">", "", ""),
        if_else(Sex == 'Male', '<img src="www/boy.gif" alt="Han">', "", ""),
        if_else(Sex == 'Female', '<img src="www/girl.gif" alt="Hun">', "", ""),
        if_else(Net, '<img src="www/net.gif" alt="Garnskadet">', "", ""),
        if_else(!is.na(Foto),str_c("<a href=\"", Foto, "\", target=\"_blank\"><img src=\"www/foto.gif\" alt=\"Foto\"></a>"),"", "")
      ),
      Month = month(Date, label = T), MonthN = month(Date), Week = isoweek(Date), Year = year(Date), 
      Place = fct_explicit_na(Place, "Ukendt"),
      NoWeight = 1*is.na(Weight), MDay = mday(Date), DayStr = format(Date, "%d. %b"),
      Day = str_c(formatC(MonthN, width = 2, flag = "0"), 
                  "-",
                  formatC(MDay, width = 2, flag = "0"))
    )
  datWeight <- datWeight %>%
    mutate(Period = as.character(Period)) %>%
    mutate(Period =
             case_when(Period == "May" ~ "Maj",
                       Period == "Oct" ~ "Okt",
                       TRUE ~ Period))
  datCatch <- left_join(datCatch, datWeight, by = c("Length", "Month" = "Period")) %>% 
    mutate(Weight = if_else(is.na(Weight), round(Avg,1), Weight)) %>% 
    mutate(Fulton = Weight*100000/Length^3) %>% 
    mutate(Month = month(Date, label = T))
  return(datCatch)
}


#' Read and prepare catch records for table
#'
#' @param prefix Prefix to csv files e.g. "../../data/data_skjern_catch_salmon".
#' @param datWeight Weight estimates.
#'
#' @return The catch records
readCatch <- function(prefix, datWeight) {
  f <- paste0(prefix, "_", 2004:year(now()), ".csv")
  datCatch <- read_csv(f, col_types = "Dddcfflclfl")  %>% 
    mutate(Weight = if_else(Killed, Weight, NA_real_), Place = fct_na_value_to_level(Place, "Ukendt"))
  
  datCatch <- datCatch %>% 
    mutate(
      Misc = paste0(
        if_else(!Killed, "<img src=\"www/c_and_r.gif\" alt=\"C&R\">", "", ""),
        if_else(Cut, "<img src=\"www/cut.gif\" alt=\"Finneklippet\">", "", ""),
        if_else(Sex == 'Male', '<img src="www/boy.gif" alt="Han">', "", ""),
        if_else(Sex == 'Female', '<img src="www/girl.gif" alt="Hun">', "", ""),
        if_else(!is.na(Foto),str_c("<a href=\"", Foto, "\", target=\"_blank\"><img src=\"www/foto.gif\" alt=\"Foto\"></a>"),"", "")
      ),
      Month = factor(month(Date, label = T), ordered = F), MonthN = month(Date), Week = isoweek(Date), Year = year(Date), 
      NoWeight = 1*is.na(Weight), MDay = mday(Date), DayStr = format(Date, "%d. %b"),
      Day = str_c(formatC(MonthN, width = 2, flag = "0"), 
                  "-",
                  formatC(MDay, width = 2, flag = "0"))
    )
  datWeight <- datWeight %>%
    mutate(Period = as.character(Period)) %>%
    mutate(Period =
             case_when(Period == "May" ~ "Maj",
                       Period == "Oct" ~ "Okt",
                       TRUE ~ Period))
  datCatch <- left_join(datCatch, datWeight, by = c("Length", "Month" = "Period")) %>% 
    mutate(Weight = if_else(is.na(Weight), round(Avg,1), Weight)) %>% 
    mutate(Fulton = Weight*100000/Length^3) %>% 
    mutate(Month = month(Date, label = T))
  return(datCatch)
}


#' Calculate yearly catch statistics
#'
#' @param datCatch Catch records.
#'
#' @return Yearly catch statistics.
yearlyStat <- function(datCatch) {
  dat <- datCatch %>% 
    mutate(Year = year(Date)) %>% group_by(Year) %>% nest() %>% 
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
                summarize(TotalP = n(), 
                          KilledP = sum(Killed))}),
      MethodStat = 
        map(data, 
            function(df) {
              df %>% 
                group_by(Method) %>% 
                summarize(TotalM = n())})
    )
  
  dat <- dat %>% 
    mutate(PlaceStat = 
             map(PlaceStat, function(df) {  
               pivot_wider(df, names_from = Place, values_from = c(TotalP, KilledP))}),
           MethodStat = 
             map(MethodStat, function(df) {  
               pivot_wider(df, names_from = Method, values_from = c(TotalM))})
    ) %>% 
    unnest(cols = c(TotalStat, PlaceStat, MethodStat)) %>% select(-data) %>% 
    replace(., is.na(.), 0)
  
  dat <- 
    dat  %>% 
    ungroup() %>% 
    transmute(Year, Total, 
              Sex = paste0(round(100*Male/Total, digits = 0), "/",
                           round(100*Female/Total, digits = 0), "/",
                           round(100*SexUnknown/Total, digits = 0)),
              Place = paste0(round(100*TotalP_Nedre/Total, digits = 0), "/", 
                             round(100*TotalP_Mellem/Total, digits = 0), "/", 
                             round(100*`TotalP_Øvre`/Total, digits = 0), "/", 
                             round(100*`TotalP_Vorgod Å`/Total, digits = 0), "/",
                             round(100*`TotalP_Omme Å`/Total, digits = 0), "/",
                             round(100*`TotalP_Andet`/Total, digits = 0)),
              PlaceK = paste0(round(100*KilledP_Nedre/Killed, digits = 0), "/", 
                              round(100*KilledP_Mellem/Killed, digits = 0), "/", 
                              round(100*`KilledP_Øvre`/Killed, digits = 0), "/", 
                              round(100*`KilledP_Vorgod Å`/Killed, digits = 0), "/",
                              round(100*`KilledP_Omme Å`/Killed, digits = 0), "/",
                              round(100*`KilledP_Andet`/Killed, digits = 0)),
              Method = paste0(round(100*Flue/Total, digits = 0), "/", 
                              round(100*Spin/Total, digits = 0), "/", 
                              round(100*Orm/Total, digits = 0), "/", 
                              round(100*Andet/Total, digits = 0)),
              Released = paste0(round(100*Released/Total, 0), "/", 
                                round(100*Killed/Total, 0)),
              Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)), 
              Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)), 
              Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
    mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0")
}



#' Calculate monthly catch statistics
#'
#' @param datCatch Catch records.
#' @param year Year under consideration.
#'
#' @return Monthly catch statistics.
monthlyStat <- function(datCatch, year) {
  dat <- datCatch %>% 
    filter(year(Date) == year) %>% 
    mutate(Month = month(Date, label = TRUE)) %>% group_by(Month) %>% nest() %>% 
    mutate(keep = map_lgl(data, function(df) { # remove months where no weight or length
      if_else(nrow(df) == sum(is.na(df$Length)) | nrow(df) == sum(is.na(df$Weight)), FALSE, TRUE)
    })) %>% 
    filter(keep) %>% 
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
                summarize(TotalP = n(), 
                          KilledP = sum(Killed))}),
      MethodStat = 
        map(data, 
            function(df) {
              df %>% 
                group_by(Method) %>% 
                summarize(TotalM = n())})
    )
  
  dat <- dat %>% 
    mutate(PlaceStat = 
             map(PlaceStat, function(df) {  
               pivot_wider(df, names_from = Place, values_from = c(TotalP, KilledP))}),
           MethodStat = 
             map(MethodStat, function(df) {  
               pivot_wider(df, names_from = Method, values_from = c(TotalM))})
    ) %>% 
    unnest(cols = c(TotalStat, PlaceStat, MethodStat)) %>% select(-data) %>% 
    replace(., is.na(.), 0)
  
  if (nrow(dat) == 0) {
    dat <-
      tibble(
        Month = month(now(), label = T),
        Total = "0/0",
        Sex = "0/0/0",
        Place = "0/0/0/0/0/0",
        Method = "0/0/0/0",
        Released = "0/0",
        Length = "0/0",
        Weight = "0/0/0",
        Fulton = "0/0"
      )
  } else {
    # add missing cols
    cNames <- c("Month", "Total", "Male", "Female", "SexUnknown", "TotalP_Nedre", "TotalP_Mellem", 
                "TotalP_Øvre", "TotalP_Vorgod Å", "TotalP_Omme Å", "TotalP_Andet", 
                "KilledP_Nedre", "KilledP_Mellem", "KilledP_Øvre", "KilledP_Vorgod Å", 
                "KilledP_Omme Å", "KilledP_Andet", "Flue", "Spin", "Orm", "Andet", "Released", 
                "Killed", "LengthAvg", "LengthMax", "WeightAvg", "WeightMax", "FultonAvg", "FultonMax")
    cNames <- cNames[!(cNames %in% names(dat))]
    cols <- rep(0, length(cNames))
    names(cols) = cNames
    dat <- dat %>% add_column(!!!cols)
    dat <- 
      dat  %>% 
      ungroup() %>% 
      transmute(Month, Total, 
                Sex = paste0(round(100*Male/Total, digits = 0), "/",
                             round(100*Female/Total, digits = 0), "/",
                             round(100*SexUnknown/Total, digits = 0)),
                Place = paste0(round(100*TotalP_Nedre/Total, digits = 0), "/", 
                               round(100*TotalP_Mellem/Total, digits = 0), "/", 
                               round(100*`TotalP_Øvre`/Total, digits = 0), "/", 
                               round(100*`TotalP_Vorgod Å`/Total, digits = 0), "/",
                               round(100*`TotalP_Omme Å`/Total, digits = 0), "/",
                               round(100*`TotalP_Andet`/Total, digits = 0)),
                PlaceK = paste0(round(100*KilledP_Nedre/Killed, digits = 0), "/", 
                                round(100*KilledP_Mellem/Killed, digits = 0), "/", 
                                round(100*`KilledP_Øvre`/Killed, digits = 0), "/", 
                                round(100*`KilledP_Vorgod Å`/Killed, digits = 0), "/",
                                round(100*`KilledP_Omme Å`/Killed, digits = 0), "/",
                                round(100*`KilledP_Andet`/Killed, digits = 0)),
                Method = paste0(round(100*Flue/Total, digits = 0), "/", 
                                round(100*Spin/Total, digits = 0), "/", 
                                round(100*Orm/Total, digits = 0), "/", 
                                round(100*Andet/Total, digits = 0)),
                Released = paste0(round(100*Released/Total, 0), "/", 
                                  round(100*Killed/Total, 0)),
                Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)), 
                Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)), 
                Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
      mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0")
  }
}
