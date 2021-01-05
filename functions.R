### Functions for scripts


fixStringErrors <- function(strings) {
  strings <- strings %>% str_replace("Vorgod �", "Vorgod Å") %>% 
    str_replace("Omme �", "Omme Å") %>% 
    str_replace("�vre", "Øvre") %>% 
    str_replace("Female", "Hun") %>% 
    str_replace("Male", "Han")
  return(strings)
}


#' Update catch records for Skjern river
#'
#' @param fn The csv file to save to.
#' @param species Either "salmon" or "trout".
#' @param start Year where update records from
#' @param reset If TRUE don't append to the old records!
#'
#' @return The updated catch records appended to the the old records. 
updateCatchSkjern <- function(fn, species, start = year(now()), reset = F) {
  foundId <- NULL
  datOld <- NULL
  if (file.exists(fn) & !reset) {
    datOld <- read_csv(fn)
    foundId <- datOld %>% pull(Id)
  }
  
  noError <- function(code) {
    tryCatch(code,
             error = function(c) TRUE
    )
  }
  
  curY <- year(now())
  dat <- NULL
  for (y in start:curY) {
    cat("Year:", y, "\n")
    url <- str_c("http://skjernaasam.dk/catchreport/?getyear=", y, "&species=", species)
    page <- read_html(url)
    ids <- html_nodes(page, xpath = '//*[@id="report-list"]/tbody/tr/@data-id') %>% 
      html_text() %>% as.numeric()
    for (id in ids) {
      if (id %in% foundId & !reset) next
      url <- str_c("http://skjernaasam.dk/ajax/?action=getcatch&id=", id) 
      while (TRUE) {
        val <- noError(jsonlite::fromJSON(url))
        if (!is.logical(val)) break
      }
      if (is.null(val$imageid)) val$imagefile = ""
      val <- val %>% unlist()
      dat <- bind_rows(val, dat)
    }
  }
  
  if (!is.null(dat)) {
    dat <- dat %>% select("Id" = id, "Date" = date, "Length" = length_cm, "Weight" = weight_kg, "Sex" = sex, "Place" = location, "Cut" = cut_fin, "Net" = net_injury, "NetDesc" = injury_description, "Killed" = released, "Method" = method, "Notes" = notes, "Kelt" = kelt, "ReportDate" = report_date, "Name" = name, "Foto" = imagefile)
    dat <- dat %>% 
      mutate(Killed = if_else(Killed == "Ja", FALSE, TRUE),  # Since represent released
             Length = str_replace_all(Length, c(".cm" = "", "Ukendt" = NA_character_)),
             Weight = str_replace_all(Weight, c(".kg" = "", "Ukendt" = NA_character_)),
             Cut = case_when(
               Cut %in% c("Ja", "left_pelvic", "right_pelvic", "pelvic", "Fedtfinne", "tail", "pectoral", "anal") ~ TRUE, 
               Cut == "Nej" ~ FALSE,
               TRUE ~ NA),
             Net = if_else(Net == "Ja", TRUE, FALSE),
             Place = case_when(
               str_detect(Place, "Øvre|Rind|Karstoft|Vinbæk|Opstrøms") ~ "Øvre",
               str_detect(Place, "Mellem|Borris Krog bro til Tarp Bro|Felding|Konsortiet") ~ "Mellem",
               str_detect(Place, "Nedre|A11|Albæk|Fjord") ~ "Nedre",
               str_detect(Place, "Vorgod") ~ "Vorgod Å",
               str_detect(Place, "Omme") ~ "Omme Å",
               TRUE ~ "Andet"),
             NetDesc = str_to_sentence(NetDesc),
             Sex = str_replace_all(Sex, c("Ukendt" = NA_character_)),
             Name = str_to_title(Name),
             Notes = str_to_sentence(Notes)) %>% 
      type_convert(col_types = cols(
        Date = col_date(format = ""),
        Length = col_double(),
        Weight = col_double(),
        Name = col_character(),
        Place = col_character(),
        Method = col_character(),
        Sex = col_character(),
        Cut = col_logical(),
        Killed = col_logical(),
        Foto = col_character(),
        Notes = col_character(),
        Net = col_logical(),
        NetDesc = col_character(),
        Kelt = col_character(),
        ReportDate = col_datetime(format = ""),
        Id = col_double()
      )) %>% 
      select(Date, Length, Weight, Name, Place, Method, Sex, Cut, Killed, Foto, Notes, 
             Net, NetDesc, Kelt, ReportDate, Id)
    
    # Try to fix errors
    dat <- dat %>% mutate(Length = if_else(Length < 40 & Weight > 0.5, NA_real_, Length))

    if (reset & !is.null(datOld)) datOld <- datOld %>% filter(year(date) < start)
    dat <- bind_rows(datOld,dat)
    dat <- dat %>% 
      dplyr::filter(Length > 39 | is.na(Length)) %>% 
      mutate(Place = fixStringErrors(Place), Weight = round(Weight,1), Length = round(Length)) %>% 
      arrange(desc(Date, ReportDate))
    write_csv(dat, fn)
  } else {
    dat <- datOld
  }
  return(dat)
}


#' Estimate weight given dat
#'
#' @param fn The csv file to save to.
#' @param dat The data set with column Period
#' @param minLength 
#'
#' @return
estimateWeight <- function(fn, dat, minLength = 40, maxLength = max(dat$Length, na.rm = TRUE)) {
  dat <- dat %>% filter(Length > minLength - 1 & Killed) 
  mod <- lm(log(Weight) ~ Period*log(Length), datCKilled)
  datP <- expand_grid(Length = minLength:maxLength, Period = unique(dat$Period))
  res <- predict(mod, datP, interval='prediction', level=0.95) 
  res <- exp(res)
  res <- res %>% as_tibble() 
  res <- bind_cols(datP, res) %>% group_by(Period) #%>% select(Length:Avg)
  colnames(res) <- c("Length", "Period", "Avg", "Lower", "Upper")
  write_csv(res, fn)
  return(res)
}


#' Update lock data for Hvide Sande
#'
#' @param fn The csv file to update
#'
#' @return The lock data set.
updateLockSkjern <- function(fn) {
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
      dat <- bind_rows(datOld,dat) %>% arrange_all(desc) %>% distinct()
      dat
      write_csv(dat, fn)
    }
  }
  return(dat)
}


#' Update water level data 
#'
#' @param fn File name.
#' @param stations Stations under consideration.
#'
#' @note Stations can be found at http://www.hydrometri.dk/hyd/
#' @return Range of dates.
updateWaterLevel <- function(fn, stations) {
  year <- year(now())
  if (file.exists(fn)) datOld <- read_csv(fn) else datOld <- NULL
  iso <- format(now(), format = "%Y-%m-%dT%T.111Z", tz = "GMT")
  dat <- NULL
  for (i in 1:nrow(stations)) {
    id <- stations$id[i]
    place <- stations$place[i]
    # get obs for last 100 days
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
  dat <- dat %>% dplyr::filter(year(Date) == year) %>%
    arrange_all(desc) %>%
    distinct(Date, .keep_all = T)
  dat <- dat %>% 
    pivot_longer(cols = 2:ncol(dat), names_to = 'Place', values_to = 'Level') %>% 
    filter(!is.na(Level))
  dat <- bind_rows(datOld, dat)
  write_csv(dat, fn)
  range(dat$Date)
}


#' Read water level files
#'
#' @param prefix File prefix before year including path.  
#' @param years Years to load.
#'
#' @return The data set in long format.
readWLevels <- function(prefix, years) {
  dat <- NULL
  cat("Read year:")
  for (y in years) {
    cat(y, "\n")
    fn <- paste0(prefix, y, ".csv")
    dat <- bind_rows(dat, read_csv(fn))
  }
  return(dat)
}


#' Calc moving average
#'
#' @param dat Water level records.
#' @param path File name including path.
#'
#' @return The data set.
calcWaterMovAvg <- function(dat, path) {
  # mov avg function
  movAvg <- function(x, days = 90){ 
    n <- days
    stats::filter(x, rep(1 / n, n), sides = 2, circular = T)
  }
  
  dat %>% 
    mutate(Day = yday(Date)) %>% 
    group_by(Day, Place) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    group_by(Place) %>% 
    nest() %>% 
    mutate(data = map(data, function(df) {
      df %>% mutate(Level = if_else(Day==366, lag(Level), Level)) 
    })) %>% 
    mutate(data = map(data, function(df) {
      df %>% mutate(Level = movAvg(Level)) 
    })) %>% 
    unnest(cols = c(data)) %>% 
    rename(Level_rAvg90 = Level) %>% 
    write_csv(path)
}


#' Calc relative water levels
#'
#' @param dat Water level records.
#' @param rMeans Average levels.
#'
#' @return NULL
calcWaterLevelRelative <- function(dat, rMeans) {
  dat <- dat %>% 
    mutate(Day = yday(Date)) %>% 
    left_join(rMeans) %>% 
    mutate(Level = Level - Level_rAvg90) %>% 
    select(-Level_rAvg90, -Day)
  for (y in distinct(dat, year(Date)) %>% pull()) {
    fn <- paste0("data/data_skjern_waterlevel_relative_long_", y, ".csv")
    write_csv(dplyr::filter(dat, year(Date) == y), fn)
  }
  invisible(NULL)
}

# for (y in 2017:2021) {
#   fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
#   dat <- read_csv(fn) 
#   print(head(dat))
#   fn <- paste0("data/data_skjern_waterlevel_long_", y, ".csv")
#   write_csv(dat, fn)
# }
# 
# for (y in 2017:2021) {
#   fn <- paste0("data/data_skjern_waterlevel_", y, ".csv")
#   dat <- read_csv(fn, col_types = cols(
#     Date = col_datetime(format = ""),
#     `Vorgod Å - Vandmøllen (055416)` = col_double(),
#     `Skjern Å - Sandfeldvej (055414)` = col_double(),
#     `Skjern Å - Alergaard (001862)` = col_double(),
#     `Skjern Å - Gjaldbæk bro (017898)` = col_double(),
#     `Rind Å - Arnborg kirke (054757)` = col_double(),
#     `Omme Å - Sønderskov bro (001855)` = col_double(),
#     `Fjederholt Å - A18 (052386)` = col_double()
#   )) %>%
#     pivot_longer(cols = 2:8, names_to = 'Place', values_to = 'Level') %>%
#     filter(!is.na(Level))
#   fn <- paste0("data/data_skjern_waterlevel_long_", y, ".csv")
#   write_csv(dat, fn)
# }
# 
# 
# for (y in 2013:2021) {
#   fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
#   dat <- read_csv(fn, col_types = cols(
#       Date = col_datetime(format = ""),
#       'Karup By (054764)' = col_double(),
#       'Hagebro (001762)' = col_double(),
#       'Nørkærbro (001767)' = col_double()
#     )) %>%
#     pivot_longer(cols = 2:4, names_to = 'Place', values_to = 'Level') %>%
#     filter(!is.na(Level))
#   write_csv(dat, fn)
# }
# 
# 
# for (y in 2013:2021) {
#   fn <- paste0("data/data_karup_waterlevel_", y, ".csv")
#   dat <- read_csv(fn) 
#   print(head(dat))
#   fn <- paste0("data/data_karup_waterlevel_long_", y, ".csv")
#   write_csv(dat, fn)
# }




