### Functions for scripts


fixStringErrors <- function(places) {
  places <- places %>% str_replace("Vorgod �", "Vorgod Å") %>% 
    str_replace("Omme �", "Omme Å") %>% 
    str_replace("�vre", "Øvre")
  return(places)
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
      mutate(Place = fixStringErrors(Place), Weight = round(Weight,1), Length = Round(Length)) %>% 
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
