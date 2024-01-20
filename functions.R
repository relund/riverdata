### Functions for scripts


#' Estimate weight given dat
#'
#' @param fn The csv file to save to.
#' @param dat The data set with column Period
#' @param minLength 
#'
#' @return
# estimateWeightOld <- function(fn, dat, minLength = 40, maxLength = max(dat$Length, na.rm = TRUE)) {
#   message("Weight: Estimate weight")
#   dat <- dat %>% 
#     filter(Length > minLength - 1 & Killed) %>% 
#     mutate(Period = factor(month(Date, label = T), ordered = F)) %>% 
#     group_by(Period) %>% filter(n() > 5) # at least 6 obs
#   mod <- lm(log(Weight) ~ Period*log(Length), dat)
#   datP <- expand_grid(Length = minLength:maxLength, Period = unique(dat$Period))
#   res <- predict(mod, datP, interval='prediction', level=0.95) 
#   res <- exp(res)
#   res <- res %>% as_tibble() 
#   res <- bind_cols(datP, res) %>% group_by(Period) #%>% select(Length:Avg)
#   colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")
#   res <- res %>% 
#     mutate(Avg = round(Avg,3), Lower = round(Lower, 3), Upper = round(Upper, 3))
#   message("  Write data to ", fn)
#   write_csv(res, fn)
#   return(invisible(res))
# }


#' Fix UTF8 string error
#'
#' @param strings Strings to fix
#'
#' @return Fixed strings
fixStringErrors <- function(strings) {
  strings <- strings %>% str_replace("Vorgod �", "Vorgod Å") %>% 
    str_replace("Omme �", "Omme Å") %>% 
    str_replace("�vre", "Øvre") %>% 
    str_replace("Female", "Hun") %>% 
    str_replace("Male", "Han")
  return(strings)
}







#' Calc flow dataset for web
#'
#' @param dat Flow data.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The dataset with column `Open` equal 0 (closed), 1 (partly open) and 2 (open).
writeLockWeb <- function(dat, prefix) {
  message("Lock flow: Update dataset for web.")
  fn <- paste0(prefix, "_flow_lock_web.csv")
  findPeaks <- function (x, m = 2){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0 | shape > 0), FUN = function(i){
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1]) | all(x[c(z : i, (i + 2) : w)] >= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
    pks <- unique(c(1, pks, length(x)))
    pks
  }
  
  setAvgFlow <- function(x) {
    idx <- findPeaks(x)
    res <- rep(0, length(x))
    for (j in 1:(length(idx)-1)) {
      # cat("j:",j,"i:",idx[j],"x:",x[idx[j]],"j+1:",j+1,"i:",idx[j+1],"x:",x[idx[j+1]],"\n")
      if (max(x[idx[j]], x[idx[j+1]]) < -100 | 
          max(x[idx[j]], x[idx[j+1]]) > 100 |
          min(x[idx[j]], x[idx[j+1]]) > 100 |
          min(x[idx[j]], x[idx[j+1]]) < -100
      ) {
        res[idx[j]:idx[j+1]] <- 2
        next
      }
      if (abs((x[idx[j+1]] - x[idx[j]])/(idx[j+1] - idx[j])) > 0.5 |
          max(x[idx[j]], x[idx[j+1]]) > 10 |
          min(x[idx[j]], x[idx[j+1]]) < -10
      ) {
        v1 <- res[idx[j]]
        v2 <- res[idx[j+1]] 
        res[idx[j]:idx[j+1]] <- 1
        if (v1 == 2) res[idx[j]] <- 2
        if (v2 == 2) res[idx[j+1]] <- 2
        next
      }
    }
    return(res)
  }
  
  dat <- dat %>% 
    dplyr::filter(DateTime > now() - days(14)) %>% 
    mutate(Open = setAvgFlow(Flow))
  message("  Write data to ", fn)
  write_csv(dat, fn)
  return(invisible(dat))
}




#' Update and save water level data for current year
#'
#' @param stations Stations under consideration.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @note Stations can be found at http://www.hydrometri.dk/hyd/. Get obs for the last 100 days
#' @return Range of dates.
# updateWaterLevelOld <- function(stations, prefix, days = 100) {
#   yr <- year(now())
#   iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
#   fn <- paste0(prefix, "_waterlevel_", yr, ".csv")
#   message("Waterlevel: Update dataset.")
#   # read data for last 100 days
#   if (file.exists(fn)) {
#     datOld <- read_csv(fn, col_types = "Tcd") 
#   } else datOld <- NULL
#   dat <- NULL
#   for (i in 1:nrow(stations)) {
#     id <- stations$id[i]
#     place <- stations$place[i]
#     # get obs for last 100 days
#     tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=", days, "&pw=100000000&inclraw=true"))
#     offset <- as.numeric(tmp$tsh$Offset)
#     tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
#     tmp$V <- tmp$V - rep(offset, length(tmp$V))
#     colnames(tmp) <- c("Date", place)
#     if (is.null(dat)) {
#       dat <- tmp
#     } else {
#       dat <- full_join(dat, tmp, by = "Date")
#     }
#   }
#   dat$Date <- ymd_hms(dat$Date, tz = "UTC") # %>% with_tz("CET") # from UTC to CET
#   dat <- dat %>% dplyr::filter(year(Date) == yr) %>%
#     arrange_all(desc) %>%
#     distinct(Date, .keep_all = T)
#   dat <- dat %>% 
#     pivot_longer(cols = 2:ncol(dat), names_to = 'Place', values_to = 'Level') %>% 
#     filter(!is.na(Level))
#   # combine with old data
#   dat <- bind_rows(datOld, dat) %>% 
#     arrange_all(desc) %>%
#     distinct(Date, Place, .keep_all = T) %>% 
#     select(Date, Place, Level)
#   # remove outliers
#   dat <- as_tsibble(dat, key = Place, index = Date) %>% 
#     group_by_key() %>%
#     mutate(Level = tsclean(Level, replace.missing = FALSE)) %>% 
#     as_tibble()
#   # save
#   message("  Write data to ", fn)
#   write_csv(dat, fn)
#   return(invisible(dat))
# }


#' Update and save water level data for current year
#'
#' @param stations Stations under consideration.
#' @param prefix Path prefix (e.g. data/data_skjern).
#' @param days Number of days back to read.
#'
#' @note Stations can be found at https://vandportalen.dk/.
#' #' @return Range of dates.
#' updateWaterLevel_new <- function(stations, prefix, days = 100) {
#'   iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
#'   dat <- NULL
#'   for (i in 1:nrow(stations)) {
#'     id <- stations$id[i]
#'     place <- stations$place[i]
#'     # get obs for last 100 days
#'     tmp <- fromJSON(paste0("https://vandportalen.dk/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=", days, "&pw=100000000&inclraw=true"))
#'     offset <- as.numeric(tmp$tsh$Offset)
#'     tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
#'     tmp$V <- tmp$V - rep(offset, length(tmp$V))
#'     colnames(tmp) <- c("Date", place)
#'     if (is.null(dat)) {
#'       dat <- tmp
#'     } else {
#'       dat <- full_join(dat, tmp, by = "Date")
#'     }
#'   }
#'   dat$Date <- ymd_hms(dat$Date, tz = "UTC") # %>% with_tz("CET") # from UTC to CET
#'   dat <- dat %>% 
#'     pivot_longer(cols = 2:ncol(dat), names_to = 'Place', values_to = 'Level') %>% 
#'     filter(!is.na(Level))
#'   # save to files
#'   for (y in unique(year(dat$Date))) {
#'     fn <- paste0(prefix, "_waterlevel_", y, ".csv")
#'     # if (file.exists(fn)) {
#'     #   datOld <- read_csv(fn, col_types = "Tcd") 
#'     # } else 
#'     datOld <- NULL
#'     message("Waterlevel: Update dataset for year ", y)
#'     dat1 <- dat %>% dplyr::filter(year(Date) == y) %>%
#'       arrange_all(desc) %>%
#'       distinct(Date, .keep_all = T)
#'     # combine with old data
#'     dat1 <- bind_rows(datOld, dat1) %>% 
#'       arrange_all(desc) %>%
#'       distinct(Date, Place, .keep_all = T) %>% 
#'       select(Date, Place, Level)
#'     # remove outliers
#'     dat1 <- as_tsibble(dat1, key = Place, index = Date) %>% 
#'       group_by_key() %>%
#'       mutate(Level = tsclean(Level, replace.missing = FALSE)) %>% 
#'       as_tibble()
#'     # save
#'     message("Write data to ", fn)
#'     write_csv(dat1, fn)
#'   }
#'   return(range(dat$Date))
#' }


#' Get and save all water level data for 2012 until now
#'
#' @param stations Stations under consideration.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @note Stations can be found at https://vandportalen.dk/ (look at the url). 
#' @return The dataset.
# getWaterLevels <- function(stations, prefix) {
#   iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
#   message("Waterlevel: Get and save datasets.")
#   # read data 
#   dat <- NULL
#   for (i in 1:nrow(stations)) {
#     id <- stations$id[i]
#     place <- stations$place[i]
#     tmp <- fromJSON(paste0("https://vandportalen.dk/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=50000&pw=1000"))
#     tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
#     tmp <- tmp %>% filter(!(is.nan(V) | is.na(V)))
#     colnames(tmp) <- c("Date", "Level")
#     tmp <- tmp %>% mutate(Place = place, Serie = i)
#     if (is.null(dat)) {
#       dat <- tmp
#     } else {
#       dat <- bind_rows(dat, tmp)
#     }
#   }
#   dat <- dat %>% 
#     mutate(Date = ymd_hms(Date, tz = "UTC"))
#   # remove outliers
#   dat <- as_tsibble(dat, key = Serie, index = Date) %>% 
#     group_by_key() %>%
#     mutate(Level = tsclean(Level, replace.missing = FALSE)) %>% 
#     as_tibble()
#   dat <- dat %>% 
#     select(-Serie) %>% 
#     mutate(Date = floor_date(Date, unit = "30 mins")) %>% 
#     group_by(Date, Place) %>% 
#     summarise(Level = mean(Level), .groups = "drop") %>% 
#     mutate(Level = round(Level, 4)) %>% 
#     arrange(desc(Date), Place) %>% 
#     filter(!is.na(Level))
#   # dat <- dat %>% dplyr::filter(year(Date) > 2011)
# 
#   # save
#   for (y in distinct(dat, year(Date)) %>% pull()) {
#     fn <- paste0(prefix, "_waterlevel_", y, ".csv")
#     message("  Write data to ", fn)
#     write_csv(dplyr::filter(dat, year(Date) == y), fn)
#   }
#   return(invisible(dat))
# }



#' #' Read water level files
#' #'
#' #' @param prefix Path prefix (e.g. data/data_skjern). 
#' #' @param years Years to load.
#' #'
#' #' @return The data set.
#' readWLevels <- function(prefix, years) {
#'   dat <- NULL
#'   for (y in years) {
#'     fn <- paste0(prefix, "_waterlevel_", y, ".csv")
#'     dat <- 
#'       bind_rows(dat, 
#'                 read_csv(fn, col_types = cols(
#'                   Date = col_datetime(format = ""),
#'                   Place = col_character(),
#'                   Value = col_double()
#'                 )))
#'   }
#'   return(dat)
#' }

#' Read water temperature files
#'
#' @param prefix Path prefix (e.g. data/data_skjern). 
#' @param years Years to load.
#'
#' @return The data set.
readWTemp <- function(prefix, years) {
  dat <- NULL
  for (y in years) {
    fn <- paste0(prefix, "_watertemp_", y, ".csv")
    dat <- 
      bind_rows(dat, read_csv(fn, col_types = "Tfd"))
  }
  return(dat)
}


# mov avg function
movAvg <- function(x, days = 90){ 
  n <- days
  stats::filter(x, rep(1 / n, n), sides = 2, circular = T)
}


#' Calc and save moving average for water level
#'
#' @param dat Water level records.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The data set.
writeWaterMovAvg <- function(dat, prefix) {
  message("Waterlevel: Update moving averages.")
  fn <- paste0(prefix, "_waterlevel_avg90.csv")

    tmp <- dat %>% 
    select(Date, Place, Value) %>% 
    mutate(Day = yday(Date)) %>% 
    group_by(Day, Place) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    group_by(Place) %>% 
    nest() %>% 
    mutate(data = map(data, function(df) {
      df %>% mutate(Value = if_else(Day==366, lag(Value), Value)) 
    })) %>% 
    mutate(data = map(data, function(df) {
      df %>% mutate(Value = movAvg(Value)) 
    })) %>% 
    unnest(cols = c(data)) %>% 
    rename(Level_rAvg90 = Value) 
  message("  Write data to ", fn)
  write_csv(tmp, fn)
  return(tmp)
}


#' Calc moving average for water temperature
#'
#' @param dat Water temperature records.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The data set.
writeWaterTempMovAvg <- function(dat, prefix) {
  message("Water temperature: Update moving averages.")
  fn <- paste0(prefix, "_watertemp_avg.csv")
  # mov avg function
  movAvg <- function(x, days = 60){ 
    n <- days
    stats::filter(x, rep(1 / n, n), sides = 2, circular = T)
  }
  
  tmp <- dat %>% 
    select(Date, Place, Value) %>% 
    mutate(Day = yday(Date)) %>% 
    group_by(Day, Place) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    group_by(Place) %>% 
    nest() %>% 
    mutate(data = map(data, function(df) {
      df %>% mutate(Value = if_else(Day==366, lag(Value), Value)) 
    })) %>% 
    mutate(data = map(data, function(df) {
      df %>% mutate(Value = movAvg(Value)) 
    })) %>% 
    unnest(cols = c(data)) %>% 
    rename(Avg = Value) 
  message("  Write data to ", fn)
  write_csv(tmp, fn)
  return(tmp)
}



#' Calc relative water levels
#'
#' @param dat Water level records.
#' @param rMeans Average levels (tibble).
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The dataset.
calcWaterLevelRelative <- function(dat, rMeans, prefix) {
  message("Waterlevel: Calc relative values.")
  dat <- dat %>% 
    mutate(Day = yday(Date)) %>% 
    left_join(rMeans, by = c("Place", "Day")) %>% 
    mutate(Level = round(Value, 3), LevelRelative = round(Level - Level_rAvg90, 3)) %>% 
    select(-Day, -Level_rAvg90)
  return(dat)
}

findPeaks <- function (x, thresh = 0) {
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 0) + 2
  if (!missing(thresh)) {
    pks[x[pks - 1] - x[pks] > thresh]
  }
  else pks
}


#' Calculate dataset for web
#'
#' @param dat Water level data set.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The data set
writeWaterTempWeb <- function(dat, rMeans, prefix) {
  message("Water temperature: Calc dataset for web.")
  dat <- dat %>% 
    mutate(Day = yday(Date)) %>% 
    left_join(rMeans, by = c("Place", "Day")) %>% 
    select(-Day)
  fn <- paste0(prefix, "_watertemp_web.csv")
  dat <- dat %>% 
    rename(Temp = Value) %>% 
    ## data 14 days back for each year
    mutate(DaysSince = as.numeric(date(Date))) %>% 
    group_by(Place) %>% 
    arrange(desc(Date)) %>% 
    nest() %>% 
    mutate(data = 
             map(data,
                 function(df) {
                   tmp <- NULL
                   for (y in df %>% distinct(year(Date)) %>% pull()) {
                     dayS <- as.numeric(date(paste0(y, "-", month(now()), "-", day(now()))))
                     tmp1 <- df %>% filter(DaysSince <= dayS & DaysSince >= dayS - 15) %>% 
                       arrange(Date) %>% mutate(YGroup = as.character(y))  
                     if (nrow(tmp1) == 0) next
                     tmp <- bind_rows(tmp, tmp1)
                   } 
                   return(tmp)
                 })) %>% 
    unnest(cols = "data") %>% 
    # mean over each hour
    mutate(Hour = hour(Date), DateDay = date(Date)) %>% 
    group_by(Hour, Place, DateDay, YGroup) %>% 
    summarise(Date = median(Date), 
              Temp = round(mean(Temp),1), 
              Avg = round(mean(Avg),3), 
              .groups = "drop") %>% 
    select(-Hour, -DateDay) %>% 
    # set Date to same year 
    mutate(Date = ymd_hms(format(Date, "2020-%m-%d %H-%M-%S"))) %>% 
    relocate(Date) %>% 
    arrange(Place, YGroup, Date) 
  # add avg as group
  dat <- dat %>% 
    bind_rows(dat, 
              dat %>% 
                filter(YGroup == year(now())) %>% 
                group_by(Place) %>% 
                transmute(Date, YGroup = "Gens", Temp = Avg)) %>% 
    select(-Avg)
  message("  Write data to ",fn)
  write_csv(dat, fn)
  return(dat)
}


#' Calculate dataset for web
#'
#' @param dat Water level data set.
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The data set
writeWaterLevelsWeb <- function(dat, prefix) {
  message("Waterlevel: Calc dataset for web.")
  fn <- paste0(prefix, "_waterlevel_web.csv")
  dat <- dat %>% 
    ## data 14 days back for each year
    mutate(DaysSince = as.numeric(date(Date))) %>% 
    group_by(Place) %>% 
    arrange(desc(Date)) %>% 
    nest() %>% 
    mutate(data = 
             map(data,
                 function(df) {
                   tmp <- NULL
                   for (y in df %>% distinct(year(Date)) %>% pull()) {
                     dayS <- as.numeric(date(paste0(y, "-", month(now()), "-", day(now()))))
                     tmp1 <- df %>% filter(DaysSince <= dayS & DaysSince >= dayS - 15) %>% 
                       arrange(Date) %>% mutate(YGroup = y)  
                     if (nrow(tmp1) == 0) next
                     tmp <- bind_rows(tmp, tmp1)
                   } 
                   return(tmp)
                 })) %>% 
    unnest(cols = "data") 
  datLastObs <- dat %>% group_by(Place) %>% summarize(LastObs = max(Date), .groups = "drop")
  dat <- dat %>% 
    # mean over each hour
    mutate(Hour = hour(Date), DateDay = date(Date)) %>% 
    group_by(Hour, Place, DateDay, YGroup) %>% 
    summarise(Date = median(Date),
              Level = round(100*mean(Value),1), 
              LevelRelative = round(100*mean(LevelRelative),1),
              .groups = "drop") %>% 
    select(-Hour, -DateDay) %>% 
    left_join(datLastObs, by = "Place") %>% 
    # set Date to same year 
    mutate(Date = ymd_hms(format(Date, "2020-%m-%d %H-%M-%S")),
           LastObs = ymd_hms(format(LastObs, "2020-%m-%d %H-%M-%S"))) %>% 
    filter(Date <= LastObs) %>%
    select(-LastObs) %>%
    relocate(Date) %>% 
    arrange(Place, YGroup, Date) 
  message("  Write data to ",fn)
  write_csv(dat, fn)
  return(dat)
}






#' Save catch records from 2020 to present to csv file 
#'
#' @return The data.
writeCatchKarup <- function() {
  message("Catch records: Update dataset.")
  ## data to today
  dat <- fromJSON("https://fangstjournalen.dtu.dk/fangst.nsf/service.xsp?open&assoc=49F1767931B31CD0C1258398007953C0&type=1")
  cols <- dat$data$cols
  cols$label[is.na(cols$label)] <- "Unknown"
  rows <- dat$data$rows$c
  rows <- lapply(rows, FUN = function(x) {x[,1]})
  dat1 <-  suppressMessages(t(map_dfc(rows, ~ .x)))
  colnames(dat1) <- cols$label
  dat1 <- as_tibble(dat1, .name_repair = "universal")
  dateStr <- dat1$Dato %>% str_extract_all("(?<=\\().+?(?=\\))", simplify = T) %>%
    str_split(",", simplify = TRUE) %>% as_tibble(.name_repair = "minimal")
  colnames(dateStr) <- c("Year", "Month", "Day")
  dateStr <- suppressMessages(type_convert(dateStr))
  dateStr <- mutate(dateStr, Month = Month + 1)
  dateStr <- str_c(dateStr$Year, "-", str_pad(dateStr$Month, 2, "left", pad="0"), "-", str_pad(dateStr$Day, 2, "left", pad="0"))
  dat1 <- bind_cols(Date=dateStr, dat1)
  dat1 <- dat1 %>% dplyr::filter(str_detect(Art, "Havørred"))
  dat2 <- dat1 %>% transmute(Date, Length = `Længde`, Weight = `Vægt`, Name = Navn, Place = Zone, Method = Agn, Cut = FALSE, Foto = Foto, Killed = (Hjemtaget == "Ja"), Sex = `Køn`)
  dat2 <- suppressMessages(type_convert(dat2))
  
  ## Merge and tidy
  dat4 <- dat2 %>% dplyr::filter(year(Date)>2019)
  dat4 <- dat4 %>% mutate(Weight = if_else(Length >= 40, Weight, NA_real_))
  # remove outliers
  dat5 <- dplyr::filter(dat4, Length > 39 & Killed)
  mod1 <- lm(log(Weight) ~ log(Length), dat5)
  # summary(mod1)
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
  dat4 <- dat4 %>% 
    mutate(Method = 
             str_replace_all(Method, 
                             c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", 
                               "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", 
                               "Mede" = "Orm", "Spinflue" = "Spin", "Maddike" = "Orm", "Orm, spinner" = "Orm")))
  # unique(dat4$Method)
  dat4 <- dat4 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
  unique(dat4$Sex)
  dat4 <- dat4 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
  dat4 <- dat4 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
  dat4 <- dat4 %>% mutate(Place = str_replace_all(Place, c("Mellem.*" = "Mellem", "Øvre.*" = "Øvre", "Nedre.*" = "Nedre")))
  dat4 <- dat4 %>% select(-Fulton)
  # unique(dat4$Place)
  
  ## Save to file
  fn <- "data/data_karup_catch_seatrout_2020-.csv"
  message("  Write data to ",fn)
  write_csv(dat4, fn)
  return(dat4)
}



#' Update water temperature data for current year 
#'
#' @param stations Stations under consideration.
#' @param prefix Path prefix (e.g. data/data_karup).
#'
#' @note Stations can be found at http://www.hydrometri.dk/hyd/.
#' @return The dataset.
# updateWaterTempKarup <- function(stations, prefix, days = 100) {
#   year <- year(now())
#   iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
#   fn <- paste0(prefix, "_watertemp_", year, ".csv")
#   message("Water temperature: Update datasets.")
#   # read data for last 100 days
#   if (file.exists(fn)) {
#     datOld <- read_csv(fn, col_types = "Tcd") 
#   } else datOld <- NULL
#   dat <- NULL
#   for (i in 1:nrow(stations)) {
#     id <- stations$id[i]
#     place <- stations$place[i]
#     # get obs for last 100 days
#     tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=100&pw=100000000&inclraw=true"))
#     offset <- as.numeric(tmp$tsh$Offset)
#     tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
#     tmp$V <- tmp$V - rep(offset, length(tmp$V))
#     colnames(tmp) <- c("Date", paste0(place, " (", id, ")"))
#     if (is.null(dat)) {
#       dat <- tmp
#     } else {
#       dat <- full_join(dat, tmp, by = "Date")
#     }
#   }
#   dat$Date <- ymd_hms(dat$Date, tz = "UTC") # %>% with_tz("CET") # from UTC to CET
#   dat <- dat %>% dplyr::filter(year(Date) == year) %>%
#     arrange_all(desc) %>%
#     distinct(Date, .keep_all = T)
#   dat <- dat %>% 
#     pivot_longer(cols = 2:ncol(dat), names_to = 'Place', values_to = 'Temp') %>% 
#     filter(!is.na(Temp))
#   # combine with old data
#   dat <- bind_rows(datOld, dat) %>% 
#     arrange_all(desc) %>%
#     distinct(Date, Place, .keep_all = T)
#   # save
#   message("  Write data to ", fn)
#   write_csv(dat, fn)
#   return(invisible(dat))
# }


#' #' Update water temperature data for current year 
#' #'
#' #' @param stations Stations under consideration.
#' #' @param prefix Path prefix (e.g. data/data_karup).
#' #'
#' #' @note Stations can be found at http://www.hydrometri.dk/hyd/. Get obs for the last 100 days
#' #' @return The dataset.
#' updateWaterTempSkjern <- function(stations, prefix) {
#'   year <- year(now())
#'   iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
#'   fn <- paste0(prefix, "_watertemp_", year, ".csv")
#'   message("Water temperature: Update datasets.")
#'   # read data for last 100 days
#'   if (file.exists(fn)) {
#'     datOld <- read_csv(fn, col_types = "Tcdd") 
#'   } else datOld <- NULL
#'   dat <- NULL
#'   for (i in 1:nrow(stations)) {
#'     id <- stations$id[i]
#'     place <- stations$place[i]
#'     # get obs for last 100 days
#'     tmp <- fromJSON(paste0("http://hydrometri.azurewebsites.net/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=100&pw=100000000&inclraw=true"))
#'     offset <- as.numeric(tmp$tsh$Offset)
#'     tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
#'     tmp$V <- tmp$V - rep(offset, length(tmp$V))
#'     colnames(tmp) <- c("Date", paste0(place, " (", id, ")"))
#'     if (is.null(dat)) {
#'       dat <- tmp
#'     } else {
#'       dat <- full_join(dat, tmp, by = "Date")
#'     }
#'   }
#'   dat$Date <- ymd_hms(dat$Date, tz = "UTC") # %>% with_tz("CET") # from UTC to CET
#'   dat <- dat %>% dplyr::filter(year(Date) == year) %>%
#'     arrange_all(desc) %>%
#'     distinct(Date, .keep_all = T)
#'   dat <- dat %>% 
#'     pivot_longer(cols = 2:ncol(dat), names_to = 'Place', values_to = 'Temp') %>% 
#'     filter(!is.na(Temp))
#'   # combine with old data
#'   dat <- bind_rows(datOld, dat) %>% 
#'     arrange_all(desc) %>%
#'     distinct(Date, Place, .keep_all = T)
#'   # save
#'   message("  Write data to ", fn)
#'   write_csv(dat, fn)
#'   return(invisible(dat))
#' }



stripKml <- function(mapId, Club = NA, GroupNameMarkers = NULL, GroupNameLines = NULL) {
  kml <- read_xml(str_c("https://www.google.com/maps/d/u/0/kml?mid=", mapId, "&forcekml=1"))
  xml_ns_strip(kml)
  x <- xml_find_all(kml, "//Folder")
  
  datMarkers <- NULL
  if (length(xml_find_all(x, ".//Point")) > 0) {
    datMarkers <- bind_rows(map(x, function(n) {  # for each Folder
        folderName <- xml_text(xml_find_all(n, "./name"))  # Folder name
        y <- xml_find_all(n, "./Placemark[Point]")
        res <- tibble(Desc = xml_text(xml_find_first(y, "./name")),
                      Text = xml_text(xml_find_first(y, "./description")),
                      cord = xml_text(xml_find_first(y, ".//coordinates"), trim = TRUE) ) %>%
          mutate(long = as.numeric(str_split_fixed(cord, ",", 3)[,1]), lat = as.numeric(str_split_fixed(cord, ",", 3)[,2])) %>%
          select(-cord)
        tibble(Group = folderName, Point = list(res))  
      })) %>% 
      filter(map_lgl(Point, function(df) nrow(df) > 0)) %>% 
      unnest(Point) %>% mutate(Club = Club) %>% 
      mutate(Icon = case_when(
        str_detect(Group, fixed('parkering', ignore_case=TRUE)) ~ "park.png",
        str_detect(Group, fixed('standpladser', ignore_case=TRUE)) ~ "fish.png",
        str_detect(Group, fixed('bro', ignore_case=TRUE)) ~ "bridge.png",
        str_detect(Group, fixed('spang', ignore_case=TRUE)) ~ "footbridge.png",
        str_detect(Group, fixed('shelter', ignore_case=TRUE)) ~ "shelter.png",
        str_detect(Group, fixed('sten', ignore_case=TRUE)) ~ "rock.png",
        str_detect(Desc, fixed('hytte', ignore_case=TRUE)) ~ "cottage.png",
        str_detect(Desc, fixed('indhegning', ignore_case=TRUE)) ~ "fence.png",
        str_detect(Desc, fixed('hus', ignore_case=TRUE)) ~ "house.png",
        str_detect(Desc, fixed('fiskekort', ignore_case=TRUE)) ~ "house.png",
        str_detect(Desc, fixed('p-plads', ignore_case=TRUE)) ~ "park.png",
        str_detect(Desc, fixed('parkering', ignore_case=TRUE)) ~ "park.png",
        str_detect(Desc, fixed('spang', ignore_case=TRUE)) ~ "footbridge.png",
        str_detect(Desc, fixed('toilet', ignore_case=TRUE)) ~ "wc.png",
        str_detect(Desc, fixed('wc', ignore_case=TRUE)) ~ "wc.png",
        str_detect(Desc, fixed('info', ignore_case=TRUE)) ~ "infoplace.png",
        str_detect(Desc, fixed('shelter', ignore_case=TRUE)) ~ "shelter.png",
        str_detect(Desc, fixed('bro', ignore_case=TRUE)) ~ "bridge.png",
        str_detect(Desc, fixed('båd', ignore_case=TRUE)) ~ "boat.png",
        str_detect(Desc, fixed('fiskeret', ignore_case=TRUE)) ~ "infoplace.png",
        TRUE ~ NA_character_
      )) 
    if (!is.null(GroupNameMarkers)) datMarkers$Group <- GroupNameMarkers
  }

  datLines <- NULL
  if (length(xml_find_all(x, ".//LineString")) > 0) {
    ctr <- 0
    datLines <- bind_rows(map(x, function(n) {  # for each Folder
        folderName <- xml_text(xml_find_all(n, "./name"))  # Folder name
        y <- xml_find_all(n, "./Placemark[LineString]")
        res <- bind_rows(map(y, function(n) {  # for each Placemark
          lineName <- xml_text(xml_find_first(n, "./name"))
          lineText <- xml_text(xml_find_first(n, "./description"))
          txt <- xml_text(xml_find_all(n, "./LineString/coordinates"))
          l <- suppressWarnings(
            read_csv(txt, col_names = c("long", "lat", "h"), col_types = "ddd") %>%
            select(-h) %>%
            filter(!is.na(lat)))
          ctr <<- ctr + 1
          tibble(Desc = lineName, Text = lineText, LineCord = list(l), LineGroupId = ctr)
        }))
        tibble(Group = folderName, Line = list(res))
      })) %>%
      filter(map_lgl(Line, function(df) nrow(df) > 0)) %>%
      unnest(col = c(Line)) %>% 
      unnest(col = c(LineCord)) %>% 
      mutate(Club = Club)
    if (!is.null(GroupNameLines)) datLines$Group <- GroupNameLines
  }
  return(list(datMarkers = datMarkers, datLines = datLines))
}




# calcMapMarkers <- function(prefix) {
#   # Stednavne
#   datMarkers <- read_xml("https://www.google.com/maps/d/u/0/kml?mid=1XJoAUKY_-kbmhZgovPpLgi82Gn8&forcekml=1")
#   xml_ns_strip(datMarkers)
#   x <- xml_find_all(datMarkers, "//Placemark")
#   
#   datMarkers <- tibble(Club = NA, Group = "Stednavne", Desc = xml_text(xml_find_all(x, ".//name")), 
#                        cord = xml_text(xml_find_all(x, ".//coordinates"), trim = TRUE) ) %>% 
#     mutate(long = as.numeric(str_split_fixed(cord, ",", 3)[,1]), lat = as.numeric(str_split_fixed(cord, ",", 3)[,2])) %>% 
#     select(-cord)
# }



#' Save catch records from a given year to a csv file 
#'
#' @param url Url for json (without year) ending with a slash.
#' @param yr Year to get data for.
#' @param prefix Prefix for csv files such as 'data/data_karup'.
#' @param species Species. Either "Havørred" or "Laks".
#'
#' @return The data (tibble).
writeCatch <- function(url, prefix, yr, species = "Havørred") {
  message("Catch records: Update dataset for year ", yr)
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
  if (species == "Havørred") dat1 <- dat1 %>% dplyr::filter(str_detect(Art, "Havørred"))
  if (species == "Laks") dat1 <- dat1 %>% dplyr::filter(str_detect(Art, "Laks"))
  dat2 <- dat1 %>% 
    transmute(Date, Length = `Længde`, Weight = `Vægt`, Name = Navn, Place = Zone, Method = Agn, 
              Cut = NA_character_, Foto = Foto, Killed = (Hjemtaget == "Ja"), Sex = `Køn`, Net = Garnskadet)
  if ("Fedtfinne.klippet" %in% colnames(dat1)) dat2$Cut = dat1$Fedtfinne.klippet
  dat2 <- suppressMessages(type_convert(dat2))
  
  ### Merge and tidy
 dat3 <- dat2 %>% mutate(Weight = if_else(Length >= 40, Weight, NA_real_)) %>% 
   filter(Length >= 40 | is.na(Length))
  ## Remove weight outliers
 if (species == "Havørred") res <- read_csv(str_c(prefix, "_weight_seatrout.csv"), show_col_types = FALSE) 
 if (species == "Laks") res <- read_csv(str_c(prefix, "_weight_salmon.csv"), show_col_types = FALSE) 
  res <- res %>% 
    group_by(Length) %>% 
    summarise(Lower = min(Lower), Upper = max(Upper))
 dat3 <- left_join(dat3, res, by = join_by(Length))
  #dat3 %>% filter( !((Weight >= 0.8 * Lower & Weight <= 1.2 * Upper) | is.na(Weight) ))
 dat3 <-dat3 %>% 
    mutate(Weight = if_else(Weight >= 0.8 * Lower & Weight <= 1.2 * Upper, Weight, NA_real_, NA_real_)) %>% 
    select(-Upper, -Lower)
  ## Fix custom errors
 dat3 <-dat3 %>% 
    mutate(Method = str_replace_all(Method, 
                             c("Wobler" = "Spin", "Blink" = "Spin", "Spinner" = "Spin", "Jig" = "Spin", 
                               "Bombarda med flue" = "Spin", "Tørflue" = "Flue", "Pirk/Pilk" = "Spin", 
                               "Mede" = "Orm", "Spinflue" = "Spin", "Spin-flue" = "Spin", "Maddike" = "Orm", 
                               "Orm, spinner" = "Orm", "Orm,spin" = "Orm")),
           Place = case_when(
             str_detect(Place, "(Øvre.*)|(Skjern.*Rind)|(Skjern.*opstrøms)") ~ "Øvre",
             str_detect(Place, "(Mellem.*)|(Skjern.*Tarp.*Borris)") ~ "Mellem",
             str_detect(Place, "(Nedre.*)|(Skjern.*Borris.*Fjord)") ~ "Nedre",
             str_detect(Place, "Haderup|Haderis") ~ "Haderis Å",
             str_detect(Place, "Vorgod") ~ "Vorgod Å",
             str_detect(Place, "Omme") ~ "Omme Å",
             TRUE ~ "Andet")
           )
 # dat3 <-dat3 %>% mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
 dat3 <-dat3 %>% mutate(Sex = str_replace_all(Sex, c("Ved ikke" = NA_character_)))
 dat3 <-dat3 %>% mutate(Cut = if_else(Cut == "Ja", TRUE, if_else(Cut == "Nej", FALSE, NA)))
  # unique(dat3$Sex)
 dat3 <-dat3 %>% mutate(Name = str_to_title(str_replace_all(Name, c("Ikke oplyst" = NA, "Mogens Styhr Rasmussen" = "Mogens Styhr", "Ikke Oplyst" = NA, "Poul Godt Godt" = "Poul Godt", "KÅS [0-9 ]* " = "", "Kås [0-9 ]* " = "", ", Vridsted, 2017123" = "", "Xx Yy" = NA))))
 dat3 <-dat3 %>% mutate(Name = str_replace(Name, fixed("**********"), NA)) %>% mutate(Name = str_replace(Name, "Xx Yy", NA_character_))
  # unique(dat3$Place)
  
  ## Save to file
  if (species == "Havørred") res <- "seatrout"
  if (species == "Laks") res <- "salmon"
  fn <- str_c(prefix, "_catch_", res, "_", yr, ".csv")
  message("  Write data to ", fn)
  write_csv(dat3, fn)
  return(dat3)
}



#' Estimate weight ranges.
#'
#' @param prefix Prefix for csv files such as 'data/data_karup'.
#' @param seatrout If true get data for seatrout otherwise salmon.
#'
#' @return The estimated results (tibble).
writeWeightEstimates <- function(prefix, seatrout = TRUE) {
  message("Weight: Estimate weight")
  species <- if_else(seatrout, "seatrout", "salmon")
  pfx <- str_c(str_remove(prefix,".*/"), "_catch_", species)
  dat <- getAllCatches(pfx)
  minLength <- 40
  maxLength <- max(dat$Length, na.rm = TRUE)
  dat <- dat %>% 
    filter(Length > minLength - 1 & Killed) %>% 
    mutate(Period = factor(month(Date, label = T), ordered = F)) %>% 
    group_by(Period) %>% filter(n() > 5) # at least 6 obs
  mod <- lm(log(Weight) ~ Period*log(Length), dat)
  datP <- expand_grid(Length = minLength:maxLength, Period = unique(dat$Period))
  res <- predict(mod, datP, interval='prediction', level=0.95) 
  res <- exp(res)
  res <- res %>% as_tibble() 
  res <- bind_cols(datP, res) %>% group_by(Period) #%>% select(Length:Avg)
  colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")
  res <- res %>% 
    mutate(Avg = round(Avg,3), Lower = round(Lower, 3), Upper = round(Upper, 3))
  fn <- str_c(prefix, "_weight_", species, ".csv")
  message("  Write data to ", fn)
  write_csv(res, fn)
  return(invisible(res))
}


#' Read all catches from a river
#'
#' @param prefix File prefix such as 'data_karup_catch_seatrout'.
#'
#' @return The data.
getAllCatches <- function(prefix) {
  f <- dir_ls("data", regexp = str_c(prefix, "_[0-9]{4}"))
  dat <- read_csv(f, col_types = "Dddcfflclcl") %>% 
    arrange(Date)
  return(dat)
}


#' #' Update catch records for Skjern river
#' #'
#' #' @param prefix Path prefix (e.g. data/data_skjern).
#' #' @param species Either "salmon" or "trout".
#' #' @param start Year where update records from
#' #' @param reset If TRUE don't append to the old records!
#' #'
#' #' @return The updated catch records appended to the the old records. 
#' updateCatchSkjern <- function(prefix, species, start = year(now()), reset = F) {
#'   message("Catch records: Update dataset for ", species, ".")
#'   foundId <- NULL
#'   datOld <- NULL
#'   if (species == "salmon") fn <- paste0(prefix, "_catch_salmon.csv") 
#'   if (species == "trout") fn <- paste0(prefix, "_catch_seatrout.csv")
#'   if (file.exists(fn)) {
#'     datOld <- read_csv(fn, col_types = "DddccccllcclccTd")
#'     foundId <- datOld %>% pull(Id)
#'   }
#'   
#'   noError <- function(code) {
#'     tryCatch(code,
#'              error = function(c) TRUE
#'     )
#'   }
#'   
#'   curY <- year(now())
#'   dat <- NULL
#'   message("  Consider year:", appendLF = F)
#'   for (y in start:curY) {
#'     message(" ", y, appendLF = F)
#'     url <- str_c("http://skjernaasam.dk/catchreport/?getyear=", y, "&species=", species)
#'     page <- read_html(url)
#'     ids <- html_nodes(page, xpath = '//*[@id="report-list"]/tbody/tr/@data-id') %>% 
#'       html_text() %>% as.numeric()
#'     for (id in ids) {
#'       if (id %in% foundId & !reset) next
#'       url <- str_c("http://skjernaasam.dk/ajax/?action=getcatch&id=", id) 
#'       while (TRUE) {
#'         val <- noError(jsonlite::fromJSON(url))
#'         if (!is.logical(val)) break
#'       }
#'       if (is.null(val$imageid)) val$imagefile = ""
#'       val <- val %>% unlist()
#'       dat <- bind_rows(val, dat)
#'     }
#'   }
#'   message("")
#'   
#'   if (!is.null(dat)) {
#'     dat <- dat %>% select("Id" = id, "Date" = date, "Length" = length_cm, "Weight" = weight_kg, "Sex" = sex, "Place" = location, "Cut" = cut_fin, "Net" = net_injury, "NetDesc" = injury_description, "Killed" = released, "Method" = method, "Notes" = notes, "Kelt" = kelt, "ReportDate" = report_date, "Name" = name, "Foto" = imagefile)
#'     dat <- dat %>% 
#'       mutate(Killed = if_else(Killed == "Ja", FALSE, TRUE),  # Since represent released
#'              Length = str_replace_all(Length, c(".cm" = "", "Ukendt" = NA_character_)),
#'              Weight = str_replace_all(Weight, c(".kg" = "", "Ukendt" = NA_character_)),
#'              Cut = case_when(
#'                Cut %in% c("Ja", "left_pelvic", "right_pelvic", "pelvic", "Fedtfinne", "tail", "pectoral", "anal") ~ TRUE, 
#'                Cut == "Nej" ~ FALSE,
#'                TRUE ~ NA),
#'              Net = if_else(Net == "Ja", TRUE, FALSE),
#'              Place = case_when(
#'                str_detect(Place, "Øvre|Rind|Karstoft|Vinbæk|Opstrøms") ~ "Øvre",
#'                str_detect(Place, "Mellem|Borris Krog bro til Tarp Bro|Felding|Konsortiet") ~ "Mellem",
#'                str_detect(Place, "Nedre|A11|Albæk|Fjord") ~ "Nedre",
#'                str_detect(Place, "Vorgod") ~ "Vorgod Å",
#'                str_detect(Place, "Omme") ~ "Omme Å",
#'                TRUE ~ "Andet"),
#'              NetDesc = str_to_sentence(NetDesc),
#'              Sex = str_replace_all(Sex, c("Ukendt" = NA_character_)),
#'              Name = str_to_title(Name),
#'              Notes = str_to_sentence(Notes)) %>% 
#'       type_convert(col_types = cols(
#'         Date = col_date(format = ""),
#'         Length = col_double(),
#'         Weight = col_double(),
#'         Name = col_character(),
#'         Place = col_character(),
#'         Method = col_character(),
#'         Sex = col_character(),
#'         Cut = col_logical(),
#'         Killed = col_logical(),
#'         Foto = col_character(),
#'         Notes = col_character(),
#'         Net = col_logical(),
#'         NetDesc = col_character(),
#'         Kelt = col_character(),
#'         ReportDate = col_datetime(format = ""),
#'         Id = col_double()
#'       )) %>% 
#'       select(Date, Length, Weight, Name, Place, Method, Sex, Cut, Killed, Foto, Notes, 
#'              Net, NetDesc, Kelt, ReportDate, Id)
#'     
#'     # Try to fix errors
#'     dat <- dat %>% mutate(Length = if_else(Length < 40 & Weight > 0.5, NA_real_, Length))
#'     
#'     if (reset & !is.null(datOld)) datOld <- datOld %>% filter(year(Date) < start)
#'     dat <- bind_rows(datOld,dat)
#'     dat <- dat %>% 
#'       dplyr::filter(Length > 39 | is.na(Length)) %>% 
#'       mutate(Place = fixStringErrors(Place), Weight = round(Weight,1), Length = round(Length)) %>% 
#'       arrange(desc(Date), desc(ReportDate))
#'     message("  Write data to ", fn)
#'     write_csv(dat, fn)
#'   } else {
#'     dat <- datOld
#'   }
#'   return(invisible(dat))
#' }


#' Update and save time series data from vandportalen.dk
#'
#' @param stations Stations under consideration.
#' @param prefix Path prefix (e.g. data/data_skjern).
#' @param prefix1 Prefix to append to `prefix` (e.g. waterlevel).
#' @param days Number of days back to get data for.
#'
#' @note Stations can be found at https://vandportalen.dk/ (look at the url). 
#' @return Data set (tibble).
writeTimeSeriesData <- function(stations, prefix, prefix1, days) {
  message("Retrive ", prefix1, " time series data.")
  iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
  dat <- NULL
  for (i in 1:nrow(stations)) {
    id <- stations$id[i]
    place <- stations$place[i]
    tmp <- fromJSON(paste0("https://vandportalen.dk/api/hyd/getplotdata?tsid=", id, "&enddate=", iso, "&days=", days, "&pw=10000000"))
    if (length(tmp$PlotRecs) == 0) next
    tmp <- as_tibble(tmp$PlotRecs[,1:2]) %>% mutate(V = sapply(tmp$PlotRecs[,2], function(x) {x[1]}))
    tmp <- tmp %>% filter(!(is.nan(V) | is.na(V)))
    colnames(tmp) <- c("Date", "Value")
    tmp <- tmp %>% mutate(Place = place, Serie = i)
    dat <- bind_rows(dat, tmp)
  }
  dat <- dat %>% 
    mutate(Date = ymd_hms(Date, tz = "UTC")) %>% 
    arrange(Place, desc(Date)) 
  # ggplot(dat %>% filter(year(Date) == 2022), aes(x = Date, y = Value, color = Serie)) + geom_line() + facet_wrap(vars(Place, Serie), scales = "free", nrow = 4)
  # remove outliers
  dat1 <- as_tsibble(dat, key = Serie, index = Date) %>%
    group_by_key() %>%
    mutate(Value = tsclean(Value, replace.missing = FALSE)) %>%
    as_tibble() 
  # ggplot(data = dat1 %>% filter(year(Date) %in% 2017:2023)) +
  #   geom_point(aes(x = Date, y = Value, color = Serie), alpha = 0.5) +
  #   geom_line(aes(x = Date, y = Value), color = "red") +
  #   geom_line(aes(x = Date, y = Value1)) +
  #   facet_wrap(vars(Serie), scales = "free", nrow = 4)
  ## average data if more than one serie
  dat2 <- dat1 %>% 
    filter(!is.na(Value)) %>% 
    # mutate(Date2 = round_date(Date, unit = "6 hour")) %>%
    group_by(Date, Place) %>% 
    mutate(Value = mean(Value)) %>% 
    ungroup() %>% 
    select(Date, Place, Value)
  # ggplot(data = dat2 %>% filter(year(Date) %in% 2017:2023) %>% ungroup()) +
  #   # geom_point(aes(x = Date, y = Value, color = Serie), alpha = 0.15) +
  #   # geom_line(aes(x = Date, y = Value), alpha = 0.5) +
  #   geom_line(aes(x = Date, y = Value), color = "red") +
  #   facet_wrap(vars(Place), scales = "free", nrow = 4)
  # dat3 <- dat2 %>%
  #   group_by(Place) %>%
  #   nest() %>%
  #   mutate(data = map(data, function(df) {
  #     df %>% mutate(Value3 = movAvg(Value, days = 30))
  #   })) %>%
  #   unnest(cols = c(data))
  # ggplot(data = dat3 %>% filter(year(Date) %in% 2022)) +
  #   # geom_point(aes(x = Date, y = Value, color = Serie), alpha = 0.15) +
  #   geom_line(aes(x = Date, y = Value), color = "red") +
  #   geom_line(aes(x = Date, y = Value3)) +
  #   facet_wrap(vars(Place), scales = "free", nrow = 3)  
  ## merge and save
  for (y in distinct(dat2, year(Date)) %>% pull()) {
    dat3 <- dat2 %>% filter(year(Date) == y) 
    fn <- paste0(prefix, "_", prefix1, "_", y, ".csv") 
    if (file.exists(fn)) {
      datOld <- read_csv(fn, col_types = "Tcd") %>% mutate(sort = "b")
      dat3 <- bind_rows(datOld, dat3 %>% mutate(sort = "a")) %>% 
        filter(!is.na(Value)) %>% 
        arrange(Place, Date) %>% 
        group_by(Place, Date) %>% 
        arrange(sort, .by_group = TRUE) %>% 
        slice_head(n = 1) %>% 
        select(Date, Place, Value) %>% 
        arrange(Place, desc(Date)) 
    } 
    message("  Write data to ", fn)
    write_csv(dat3, fn)
  }
  return(invisible(dat))
}


#' Update and save lock data for Hvide Sande
#'
#' @param prefix Path prefix (e.g. data/data_skjern).
#'
#' @return The lock data set.
writeLockSkjern <- function(prefix) {
  message("Lock flow: Update dataset.")
  fn <- paste0(prefix, "_flow_lock.csv")
  url <- 'http://hyde.dk/Sflow/default_flow.asp'
  webpage <- read_html(url)
  dat <- html_nodes(webpage, xpath = "/html/body/div[2]/div[3]/script/text()")
  dat <- html_text(dat)
  dat <- str_replace_all(dat, "[\r\n\t]", "")
  
  dates <- str_replace(dat, "^.*labels:", "{\n'labels' :")
  dates <- str_replace(dates, "].*$", "]\n}")
  dates <- str_replace_all(dates, "'", '"')
  dates <- fromJSON(dates)$labels
  
  flow <- str_replace(dat, "^.*data:", "{\n'data' :")
  flow <- str_replace(flow, "].*$", "]\n}")
  flow <- str_replace_all(flow, "'", '"')
  flow <- fromJSON(flow)$data
  
  if (length(dates)==length(flow)) {
    dat <- tibble(DateTime = dmy_hm(dates, tz = "CET"), Flow = flow)  %>% 
      arrange_all(desc) %>% distinct()
    if (!file.exists(fn)) {
      write_csv(dat, fn)
      dat
    } else {
      datOld <- read_csv(fn, col_types = "Ti", locale = locale(tz = "CET"))
      dat <- bind_rows(datOld,dat) %>% 
        arrange_all(desc) %>% 
        distinct()
      message("  Write data to ", fn)
      write_csv(dat, fn)
    }
  }
  return(invisible(dat))
}



#' Read data files from the `data` subfolder
#'
#' @param pattern File to search for (can use regexp) (e.g. 'data_karup_catch_seatrout_[0-9]{4}').
#'
#' @return The data (tibble).
readDataFiles <- function(pattern) {
  f <- dir_ls("data", regexp = pattern)
  dat <- read_csv(f, show_col_types = FALSE) %>% 
    arrange(Date)
  return(dat)
}


#' Split a data file into year files
#'
#' @param path Path to file e.g. "data/data_karup_catch_seatrout_2020-2022.csv".
#' @param prefix Prefix path for saved files e.g. "data/data_karup_catch_seatrout".
splitDataFileByYear <- function(path, prefix) {
  dat <- read_csv(path) 
  for (y in distinct(dat, year(Date)) %>% pull()) {
    fn <- paste0(prefix, "_", y, ".csv") 
    message("  Write data to ", fn)
    write_csv(filter(dat, year(Date) == y), fn)
  }
}




fixOldDataFileByYearSkjern <- function() {
  path <- "data/data_skjern_catch_salmon_2004-2022.csv"
  prefix <- "data/data_skjern_catch_salmon"
  path <- "data/data_skjern_catch_seatrout_2004-2022.csv"
  prefix <- "data/data_skjern_catch_seatrout"
  dat <- read_csv(path) 
  dat <- dat %>% transmute(Date, Length, Weight, Name, Place, Method, Cut, Foto, Killed, Sex, Net)
  for (y in distinct(dat, year(Date)) %>% pull()) {
    fn <- paste0(prefix, "_", y, ".csv") 
    message("  Write data to ", fn)
    write_csv(filter(dat, year(Date) == y), fn)
  }
}

fixOldDataFileByYearKarup <- function() {
  prefix <- "data/data_karup_catch_seatrout"
  for (y in 2003:2022) {
    fn <- paste0(prefix, "_", y, ".csv") 
    dat <- read_csv(fn) 
    dat <- dat %>% transmute(Date, Length, Weight, Name, Place, Method, Cut, Foto, Killed, Sex, Net = NA)
    message("  Write data to ", fn)
    write_csv(dat, fn)
  }
}
