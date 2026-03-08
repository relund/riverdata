## Functions for write/save data


#' Save fangstjournalen catch records from a given year to a csv file
#'
#' Data is written to `str_c(prefix, "_catch_", res, "_", yr, ".csv")`.
#'
#' @param url Url for json (without year) ending with a slash.
#' @param yr Year to get data for (single number).
#' @param prefix Prefix path for the csv file.
#' @param species Species. Either "seatrout" or "salmon".
#' @param club False if consider association. True if consider club.
#'
#' @return The data (tibble).
#' @export
#'
#' @examples
#' url <- str_c("https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/",
#'              "catches/assoc/A97F957DD48AEDD4C1258814003E71FE/1/")
#' prefix <- "tmp/data_skjern"
#' yr <- 2023
#' write_catch(url, prefix, yr, species = "Laks")
#'
write_catch <-
   function(url,
            prefix,
            yr,
            species = "seatrout",
            club = FALSE,
            write = TRUE) {
      message("Catch records: Write dataset for year ", yr)
      ## data to today
      dat <- jsonlite::fromJSON(str_c(url, yr))
      cols <- dat$cols
      cols$label[is.na(cols$label)] <- "Unknown"
      rows <- dat$rows$c
      if (is.null(rows)) {
         fn <- paste0(prefix, "_catch_salmon_", yr - 1, ".csv")
         dat3 <- read_csv(fn, col_types = "Dddcfflclfl") |>
            slice_head(n = 0)  # get col names from last year
      } else {
         rows <- lapply(
            rows,
            FUN = function(x) {
               x[, 1]
            }
         )
         dat1 <-  suppressMessages(t(map_dfc(rows, ~ .x)))
         colnames(dat1) <- cols$label
         dat1 <-
            suppressMessages(as_tibble(dat1, .name_repair = "universal"))
         dateStr <-
            dat1$Dato |> str_extract_all("(?<=\\().+?(?=\\))", simplify = T) |>
            str_split(",", simplify = TRUE) |> as_tibble(.name_repair = "minimal")
         colnames(dateStr) <- c("Year", "Month", "Day")
         dateStr <- suppressMessages(type_convert(dateStr))
         dateStr <- mutate(dateStr, "Month" = .data$Month + 1)
         dateStr <-
            str_c(
               dateStr$Year,
               "-",
               str_pad(dateStr$Month, 2, "left", pad = "0"),
               "-",
               str_pad(dateStr$Day, 2, "left", pad = "0")
            )
         dat1 <- suppressMessages(bind_cols(Date = dateStr, dat1))
         if (club)
            dat1 <- dat1 |>
            rename(River = .data$Fiskevand)
         if (species == "seatrout")
            dat1 <- dat1 |> dplyr::filter(str_detect(.data$Art, "Havørred"))
         if (species == "salmon")
            dat1 <- dat1 |> dplyr::filter(str_detect(.data$Art, "Laks"))
         sex_col <- intersect(c("Køn...17", "Køn"), names(dat1))
         dat1$SexTmp <- if (length(sex_col) > 0) dat1[[sex_col[1]]] else NA_character_
         if (!club)
            dat2 <- dat1 |>
            transmute(
               "Date" = .data$Date,
               "Length" = .data$`Længde`,
               "Weight" = .data$`Vægt`,
               "Name" = .data$Navn,
               "Place" = .data$Zone,
               "Method" = .data$Agn,
               "Cut" = NA_character_,
               "Foto" = .data$Foto,
               "Killed" = (.data$Hjemtaget == "Ja"),
               "Sex" = .data$SexTmp,
               "Net" = .data$Garnskadet
            )
         if (club)
            dat2 <- dat1 |>
            transmute(
               "Date" = .data$Date,
               "Length" = .data$`Længde`,
               "Weight" = .data$`Vægt`,
               "Name" = .data$Navn,
               "River" = .data$River,
               "Place" = .data$Strækning.sted,
               "Method" = .data$Agn,
               "Cut" = NA_character_,
               "Foto" = .data$Foto,
               "Killed" = (.data$Hjemtaget == "Ja"),
               "Sex" = .data$SexTmp,
               "Net" = .data$Garnskadet
            )
         if ("Fedtfinne.klippet" %in% colnames(dat1))
            dat2$Cut = dat1$Fedtfinne.klippet
         dat2 <- suppressMessages(type_convert(dat2))

         ### Merge and tidy
         dat3 <-
            dat2 |> mutate(Weight = if_else(.data$Length >= 40, .data$Weight, NA_real_)) |>
            filter(.data$Length >= 40 | is.na(.data$Length))
         # if (!club) {
         #    ## Remove weight outliers
         #    if (species == "Havørred")
         #       res <-
         #          read_csv(str_c(prefix, "_weight_seatrout.csv"),
         #                   show_col_types = FALSE)
         #    if (species == "Laks")
         #       res <-
         #          read_csv(str_c(prefix, "_weight_salmon.csv"),
         #                   show_col_types = FALSE)
         #    res <- res |>
         #       group_by(.data$Length) |>
         #       summarise("Lower" = min(.data$Lower),
         #                 "Upper" = max(.data$Upper))
         #    dat3 <- left_join(dat3, res, by = join_by(.data$Length))
         #    #dat3 |> filter( !((Weight >= 0.8 * Lower & Weight <= 1.2 * Upper) | is.na(Weight) ))
         #    dat3 <- dat3 |>
         #       mutate(
         #          Weight = if_else(
         #             .data$Weight >= 0.8 * .data$Lower &
         #                .data$Weight <= 1.2 * .data$Upper,
         #             .data$Weight,
         #             NA_real_,
         #             NA_real_
         #          )
         #       ) |>
         #       select(-.data$Upper,-.data$Lower)
         # }
         ## Fix custom errors
         dat3 <- dat3 |>
            mutate("Method" = str_replace_all(
               .data$Method,
               c(
                  "Wobler" = "Spin",
                  "Blink" = "Spin",
                  "Spinner" = "Spin",
                  "Jig" = "Spin",
                  "Bombarda med flue" = "Spin",
                  "Tørflue" = "Flue",
                  "Pirk/Pilk" = "Spin",
                  "Mede" = "Orm",
                  "Spinflue" = "Spin",
                  "Spin-flue" = "Spin",
                  "Maddike" = "Orm",
                  "Spin-flue" = "Spin",
                  "Majs" = "Orm",
                  "Flåd" = "Orm",
                  "Orm, spinner" = "Orm",
                  "Orm,spin" = "Orm"
               )
            ))
         if (!club)
            dat3 <- dat3 |>
            mutate(
               Place = case_when(
                  str_detect(Place, "(Øvre.*)|(Skjern.*Rind)|(Skjern.*opstrøms)") ~ "Øvre",
                  str_detect(Place, "(Mellem.*)|(Skjern.*Tarp.*Borris)") ~ "Mellem",
                  str_detect(Place, "(Nedre.*)|(Skjern.*Borris.*Fjord)") ~ "Nedre",
                  str_detect(Place, "Haderup|Haderis") ~ "Haderis Å",
                  str_detect(Place, "Vorgod") ~ "Vorgod Å",
                  str_detect(Place, "Omme") ~ "Omme Å",
                  TRUE ~ Place
               )
            )
         # dat3 <-dat3 |> mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
         dat3 <-
            dat3 |> mutate(Sex = str_replace_all(.data$Sex, c("Ved ikke" = NA_character_)))
         dat3 <-
            dat3 |> mutate("Cut" = if_else(.data$Cut == "Ja", TRUE, if_else(.data$Cut == "Nej", FALSE, NA)))
         # unique(dat3$Sex)
         dat3 <-
            dat3 |> mutate(Name = str_to_title(str_replace_all(
               .data$Name,
               c(
                  "Ikke oplyst" = NA,
                  "Mogens Styhr Rasmussen" = "Mogens Styhr",
                  "Ikke Oplyst" = NA,
                  "Poul Godt Godt" = "Poul Godt",
                  "KÅS [0-9 ]* " = "",
                  "Kås [0-9 ]* " = "",
                  ", Vridsted, 2017123" = "",
                  "Xx Yy" = NA
               )
            )))
         dat3 <-
            dat3 |> mutate(Name = str_replace(.data$Name, fixed("**********"), NA)) |> mutate(Name = str_replace(.data$Name, "Xx Yy", NA_character_))
         # unique(dat3$Place)
      }
      ## Save to file
      if (write) {
         res <- tolower(species)
         if (species == "Havørred")
            res <- "seatrout"
         if (species == "Laks")
            res <- "salmon"
         fn <- str_c(prefix, "_catch_", res, "_", yr, ".csv")
         message("  Write data to ", fn)
         write_csv(dat3, fn)
      }

      return(dat3)
   }

#' Write lock web dataset
#'
#' @param dat Flow data.
#' @param prefix File prefix.
#'
#' @return Invisibly returns transformed data.
#' @examples
#' \dontrun{
#' write_lock_web(dat, "data/data_skjern")
#' }
write_lock_web <- function(dat, prefix) {
  message("Lock flow: Update dataset for web.")
  fn <- paste0(prefix, "_flow_lock_web.csv")
  find_peaks <- function(x, m = 2) {
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0 | shape > 0), FUN = function(i) {
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if (all(x[c(z:i, (i + 2):w)] <= x[i + 1]) | all(x[c(z:i, (i + 2):w)] >= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
    pks <- unique(c(1, pks, length(x)))
    pks
  }

  set_avg_flow <- function(x) {
    idx <- find_peaks(x)
    res <- rep(0, length(x))
    for (j in 1:(length(idx)-1)) {
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
    mutate(Open = set_avg_flow(Flow))
  message("  Write data to ", fn)
  write_csv(dat, fn)
  return(invisible(dat))
}

#' Write water level moving averages
#'
#' @param dat Water level data.
#' @param prefix File prefix.
#'
#' @return Moving-average table.
#' @examples
#' \dontrun{
#' write_water_mov_avg(dat, "data/data_karup")
#' }
write_water_mov_avg <- function(dat, prefix) {
  message("Waterlevel: Update moving averages.")
  fn <- paste0(prefix, "_waterlevel_avg90.csv")
  mov_avg <- function(x, days = 90) {
    if (length(x) < 90) return(x)
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
      df %>% mutate(Value = if_else(Day == 366, lag(Value), Value))
    })) %>%
    mutate(data = map(data, function(df) {
      df %>% mutate(Value = mov_avg(Value))
    })) %>%
    unnest(cols = c(data)) %>%
    rename(Level_rAvg90 = Value)
  message("  Write data to ", fn)
  write_csv(tmp, fn)
  return(tmp)
}

#' Write water temperature moving averages
#'
#' @param dat Water temperature data.
#' @param prefix File prefix.
#'
#' @return Moving-average table.
#' @examples
#' \dontrun{
#' write_water_temp_mov_avg(dat, "data/data_karup")
#' }
write_water_temp_mov_avg <- function(dat, prefix) {
  message("Water temperature: Update moving averages.")
  fn <- paste0(prefix, "_watertemp_avg.csv")
  mov_avg <- function(x, days = 60) {
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
      df %>% mutate(Value = if_else(Day == 366, lag(Value), Value))
    })) %>%
    mutate(data = map(data, function(df) {
      df %>% mutate(Value = mov_avg(Value))
    })) %>%
    unnest(cols = c(data)) %>%
    rename(Avg = Value)
  message("  Write data to ", fn)
  write_csv(tmp, fn)
  return(tmp)
}

#' Write water temperature web dataset
#'
#' @param dat Water temperature data.
#' @param r_means Moving-average reference table.
#' @param prefix File prefix.
#'
#' @return Web dataset.
#' @examples
#' \dontrun{
#' write_water_temp_web(dat, r_means, "data/data_skjern")
#' }
write_water_temp_web <- function(dat, r_means, prefix) {
  message("Water temperature: Calc dataset for web.")
  dat <- dat %>%
    mutate(Day = yday(Date)) %>%
    left_join(r_means, by = c("Place", "Day")) %>%
    select(-Day)
  fn <- paste0(prefix, "_watertemp_web.csv")
  dat <- dat %>%
    rename(Temp = Value) %>%
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
    mutate(Hour = hour(Date), DateDay = date(Date)) %>%
    group_by(Hour, Place, DateDay, YGroup) %>%
    summarise(Date = median(Date),
              Temp = round(mean(Temp),1),
              Avg = round(mean(Avg),3),
              .groups = "drop") %>%
    select(-Hour, -DateDay) %>%
    mutate(Date = ymd_hms(format(Date, "2020-%m-%d %H-%M-%S"))) %>%
    relocate(Date) %>%
    arrange(Place, YGroup, Date)
  dat <- dat %>%
    bind_rows(dat,
              dat %>%
                filter(YGroup == year(now())) %>%
                group_by(Place) %>%
                transmute(Date, YGroup = "Gens", Temp = Avg)) %>%
    select(-Avg)
  message("  Write data to ", fn)
  write_csv(dat, fn)
  return(dat)
}

#' Write water level web dataset
#'
#' @param dat Water level data.
#' @param prefix File prefix.
#'
#' @return Web dataset.
#' @examples
#' \dontrun{
#' write_water_levels_web(dat, "data/data_skjern")
#' }
write_water_levels_web <- function(dat, prefix) {
  message("Waterlevel: Calc dataset for web.")
  fn <- paste0(prefix, "_waterlevel_web.csv")
  dat <- dat %>%
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
    mutate(Hour = hour(Date), DateDay = date(Date)) %>%
    group_by(Hour, Place, DateDay, YGroup) %>%
    summarise(Date = median(Date),
              Level = round(100*mean(Value),1),
              LevelRelative = round(100*mean(LevelRelative),1),
              .groups = "drop") %>%
    select(-Hour, -DateDay) %>%
    left_join(datLastObs, by = "Place") %>%
    mutate(Date = ymd_hms(format(Date, "2020-%m-%d %H-%M-%S")),
           LastObs = ymd_hms(format(LastObs, "2020-%m-%d %H-%M-%S"))) %>%
    filter(Date <= LastObs) %>%
    select(-LastObs) %>%
    relocate(Date) %>%
    arrange(Place, YGroup, Date)
  message("  Write data to ", fn)
  write_csv(dat, fn)
  return(dat)
}

#' Write weight estimates
#'
#' @param prefix File prefix.
#' @param seatrout Whether to estimate seatrout (`TRUE`) or salmon (`FALSE`).
#'
#' @return Weight estimate table.
#' @examples
#' \dontrun{
#' write_weight_estimates("data/data_skjern", seatrout = TRUE)
#' }
write_weight_estimates <- function(prefix, seatrout = TRUE) {
  message("Weight: Estimate weight")
  species <- if_else(seatrout, "seatrout", "salmon")
  pfx <- str_c(str_remove(prefix, ".*/"), "_catch_", species)
  f <- dir_ls("data", regexp = str_c(pfx, "_[0-9]{4}"))
  dat <- read_csv(f, col_types = "Dddcfflclcl") %>%
    arrange(Date)
  minLength <- 40
  maxLength <- max(dat$Length, na.rm = TRUE)
  dat <- dat %>%
    filter(Length > minLength - 1 & Killed) %>%
    mutate(Period = factor(month(Date, label = T), ordered = F), MonthN = month(Date)) %>%
    group_by(Period) %>% filter(n() > 5)
  mod <- lm(log(Weight) ~ Period*log(Length), dat)
  datP <- expand_grid(Length = minLength:maxLength, Period = unique(dat$Period))
  res <- predict(mod, datP, interval = "prediction", level = 0.95)
  res <- exp(res)
  res <- res %>% as_tibble()
  res <- bind_cols(datP, res) %>% group_by(Period)
  colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")
  res <- res %>%
    mutate(Avg = round(Avg,3), Lower = round(Lower, 3), Upper = round(Upper, 3))
  res <- left_join(res, dat %>% ungroup() %>% distinct(Period, MonthN))
  fn <- str_c(prefix, "_weight_", species, ".csv")
  message("  Write data to ", fn)
  write_csv(res, fn)
  return(invisible(res))
}

#' Save HOBO data files
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' save_hobo_data()
#' }
save_hobo_data <- function() {
  url <- "ftp://web9.gigahost.dk/hobo/"
  user <- Sys.getenv("GH_FTP")
  files <- getURL(url, userpwd = user, dirlistonly = TRUE)
  files <- str_split(files, "\n")[[1]] %>% str_subset("Skjern")
  if (length(files) == 0) return(FALSE)
  dat <- NULL
  for (f in files) {
    urlF <- str_c(url, f)
    str <- getURL(urlF, userpwd = user, ftp.use.epsv = FALSE)
    dat <- bind_rows(dat, read_csv(str))
  }
  dat <- dat %>%
    transmute(Date = dmy_hms(Date),
              TempCelcius = `Water Temperature (M-WT 21143788:20833130-3), *C, Laksens Hus`,
              LevelMeters = `Water Level (M-WL04 21143788:20833130-4), meters, Laksens Hus`,
              PressureKPA = `Barometric Pressure (M-BP 21143788:21143788-1), kPa, Laksens Hus`) %>%
    mutate(Date = Date - hours(1))
  dat1 <- dat %>% transmute(Date, Place = "Skjern Å - Laksens hus", Value = LevelMeters)
  dat2 <- dat %>% transmute(Date, Place = "Skjern Å - Laksens hus", Value = PressureKPA)
  dat3 <- dat %>% transmute(Date, Place = "Skjern Å - Laksens hus", Value = TempCelcius)
  prefix <- "data/data_skjern"
  write_csv(dat1, str_c(prefix, "_waterlevel_hobo.csv"), append = T)
  write_csv(dat2, str_c(prefix, "_pressure_hobo.csv"), append = T)
  write_csv(dat3, str_c(prefix, "_watertemp_hobo.csv"), append = T)

  for (f in files) {
    tmp <- str_c("DELE ", "hobo/", f)
    curlPerform(url = "ftp://web9.gigahost.dk/", quote = tmp, userpwd = user)
  }
  return(TRUE)
}

#' Write time-series data
#'
#' @param stations Station table or `NULL`.
#' @param prefix File prefix.
#' @param prefix1 Type suffix, e.g. `"waterlevel"` or `"watertemp"`.
#' @param days Number of days to read.
#'
#' @return Invisibly returns the latest datetime.
#' @examples
#' \dontrun{
#' write_time_series_data(stations, "data/data_karup", "waterlevel", 15)
#' }
write_time_series_data <- function(stations = NULL, prefix, prefix1, days) {
  message("Retrive ", prefix1, " time series data.")
  dat2 <- NULL
  if (!is.null(stations)) {
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
      arrange(Place, desc(Date)) |>
      distinct(Date, Value, Place, Serie)
    dat1 <- as_tsibble(dat, key = Serie, index = Date) %>%
      group_by_key() %>%
      mutate(Value = tsclean(Value, replace.missing = FALSE)) %>%
      as_tibble()
    dat2 <- dat1 %>%
      filter(!is.na(Value)) %>%
      group_by(Date, Place) %>%
      mutate(Value = mean(Value)) %>%
      ungroup() %>%
      select(Date, Place, Value)
  }
  fn <- paste0(prefix, "_", prefix1, "_hobo.csv")
  if (fs::file_exists(fn)) {
    hobo <- read_csv(fn, col_types = "Tcd")
    dat2 <- bind_rows(dat2, hobo) %>%
      arrange(Place, desc(Date))
    hobo <- hobo %>% slice_head(n = 0)
    write_csv(hobo, paste0(prefix, "_", prefix1, "_hobo.csv"))
  }

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

#' Write Skjern lock flow data
#'
#' @param prefix File prefix.
#'
#' @return Lock flow table.
#' @examples
#' \dontrun{
#' write_lock_skjern("data/data_skjern")
#' }
write_lock_skjern <- function(prefix) {
  message("Lock flow: Update dataset.")
  fn <- paste0(prefix, "_flow_lock.csv")
  url <- "http://hyde.dk/Sflow/default_flow.asp"
  webpage <- read_html(url)
  dat <- html_nodes(webpage, xpath = "/html/body/div[2]/div[3]/script/text()")
  dat <- html_text(dat)
  dat <- str_replace_all(dat, "[\r\n\t]", "")

  dates <- str_replace(dat, "^.*labels:", "{\n'labels' :")
  dates <- str_replace(dates, "].*$", "]\n}")
  dates <- str_replace_all(dates, "'", "\"")
  dates <- fromJSON(dates)$labels

  flow <- str_replace(dat, "^.*data:", "{\n'data' :")
  flow <- str_replace(flow, "].*$", "]\n}")
  flow <- str_replace_all(flow, "'", "\"")
  flow <- fromJSON(flow)$data

  if (length(dates) == length(flow)) {
    dat <- tibble(DateTime = dmy_hm(dates, tz = "CET"), Flow = flow) %>%
      arrange_all(desc) %>% distinct()
    if (!file.exists(fn)) {
      write_csv(dat, fn)
      dat
    } else {
      datOld <- read_csv(fn, col_types = "Ti", locale = locale(tz = "CET"))
      dat <- bind_rows(datOld, dat) %>%
        arrange_all(desc) %>%
        distinct()
      message("  Write data to ", fn)
      write_csv(dat, fn)
    }
  }
  return(invisible(dat))
}
