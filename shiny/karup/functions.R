#' Merge two lists to one
#'
#' @param a First list.
#' @param b Second list (priority).
mergeLists <- function (a,b) {
  c(a[setdiff(names(a), names(b))], b)
}


#' Read and prepare catch records for table
#'
#' @param path Full path to csv file.
#' @param datWeight Weight estimates
#'
#' @return The catch records
readCatch <- function(prefix, datWeight) {
  datCatch <- 
    bind_rows(
      read_csv(paste0(prefix, "data_karup_catch_seatrout_2020-.csv"), col_types = "Dddcfflclf"),
      read_csv(paste0(prefix, "data_karup_catch_seatrout_2003-2019.csv"), col_types = "Dddcfflclf")) %>% 
    mutate(Weight = if_else(Killed, Weight, NA_real_), Place = fct_explicit_na(Place, "Ukendt"))

  datCatch <- datCatch %>% 
    mutate(
      Misc = paste0(
        if_else(!Killed, "<img src=\"www/c_and_r.gif\" alt=\"C&R\">", "", ""),
        if_else(Cut, "<img src=\"www/cut.gif\" alt=\"Finneklippet\">", "", ""),
        if_else(Sex == 'Male', '<img src="www/boy.gif" alt="Han">', "", ""),
        if_else(Sex == 'Female', '<img src="www/girl.gif" alt="Hun">', "", ""),
        if_else(!is.na(Foto),str_c("<a href=\"", Foto, "\", target=\"_blank\"><img src=\"www/foto.gif\" alt=\"Foto\"></a>"),"", "")
      ),
      Month = factor(month(Date, label = T), ordered = F), Week = week(Date), Year = year(Date), 
      NoWeight = 1*is.na(Weight), Day = yday(Date), DayStr = format(Date, "%d. %b")
    )
  
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
                  Female = sum(Sex == "Hun", na.rm = T), 
                  Male = sum(Sex == "Han", na.rm = T),
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
                summarize(TotalP = n())}),
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
               pivot_wider(df, names_from = Place, values_from = c(TotalP))}),
           MethodStat = 
             map(MethodStat, function(df) {  
               pivot_wider(df, names_from = Method, values_from = c(TotalM))})
    ) %>% 
    unnest(cols = c(TotalStat, PlaceStat, MethodStat), names_repair = "unique") %>% select(-data) %>% 
    replace(., is.na(.), 0)
  
  dat <- 
    # check for missing columns
    if (!("TotalP_Andet" %in% names(dat))) dat <- dat %>% mutate(TotalP_Andet = 0, KilledP_Andet = 0)
  dat <- 
    dat  %>% 
    ungroup() %>% 
    transmute(Year, Total, 
              Sex = paste0(format(100*Male/Total, digits = 0, trim = TRUE), "/",
                           format(100*Female/Total, digits = 0, trim = TRUE), "/",
                           format(100*SexUnknown/Total, digits = 0, trim = TRUE)),
              Place = paste0(format(100*Nedre/Total, digits = 0, trim = TRUE), "/", 
                             format(100*Mellem/Total, digits = 0, trim = TRUE), "/", 
                             format(100*`Øvre`/Total, digits = 0, trim = TRUE), "/", 
                             format(100*`Haderup Å`/Total, digits = 0, trim = TRUE), "/",
                             format(100*(Total - Nedre - Mellem - `Øvre` - `Haderup Å`)/Total, digits = 0, trim = TRUE)),
              Method = paste0(format(100*Flue/Total, digits = 0, trim = TRUE), "/", 
                              format(100*Spin/Total, digits = 0, trim = TRUE), "/", 
                              format(100*Orm/Total, digits = 0, trim = TRUE), "/", 
                              format(100*(Total - Flue - Spin - Orm)/Total, digits = 0, trim = TRUE)),
              Released = paste0(round(100*Released/Total, 0), "/", 
                                round(100*(Total - Released)/Total, 0)),
              Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)), 
              Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)), 
              Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
    mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0") %>% 
    arrange(desc(Year))
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
                summarize(TotalP = n())}),
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
               pivot_wider(df, names_from = Place, values_from = c(TotalP))}),
           MethodStat = 
             map(MethodStat, function(df) {  
               pivot_wider(df, names_from = Method, values_from = c(TotalM))})
    ) %>% 
    unnest(cols = c(TotalStat, PlaceStat, MethodStat)) %>% select(-data) %>% 
    replace(., is.na(.), 0)
  
  # check for missing columns
  if (!("TotalP_Andet" %in% names(dat))) dat <- dat %>% mutate(TotalP_Andet = 0, KilledP_Andet = 0)
  if (nrow(dat) == 0) {
    dat <-
      tibble(
        Month = month(now(), label = T),
        Total = "0/0",
        Sex = "0/0/0",
        Place = "0/0/0/0/0",
        Method = "0/0/0/0",
        Released = "0/0",
        Length = "0/0",
        Weight = "0/0/0",
        Fulton = "0/0"
      )
  } else {
    dat <- 
      dat  %>% 
      ungroup() %>% 
      transmute(Month, Total, 
                Sex = paste0(format(100*Male/Total, digits = 0, trim = TRUE), "/",
                             format(100*Female/Total, digits = 0, trim = TRUE), "/",
                             format(100*SexUnknown/Total, digits = 0, trim = TRUE)),
                Place = paste0(format(100*Nedre/Total, digits = 0, trim = TRUE), "/", 
                               format(100*Mellem/Total, digits = 0, trim = TRUE), "/", 
                               format(100*`Øvre`/Total, digits = 0, trim = TRUE), "/", 
                               format(100*`Haderup Å`/Total, digits = 0, trim = TRUE), "/",
                               format(100*(Total - Nedre - Mellem - `Øvre` - `Haderup Å`)/Total, digits = 0, trim = TRUE)),
                Method = paste0(format(100*Flue/Total, digits = 0, trim = TRUE), "/", 
                                format(100*Spin/Total, digits = 0, trim = TRUE), "/", 
                                format(100*Orm/Total, digits = 0, trim = TRUE), "/", 
                                format(100*(Total - Flue - Spin - Orm)/Total, digits = 0, trim = TRUE)),
                Released = paste0(round(100*Released/Total, 0), "/", 
                                  round(100*(Total - Released)/Total, 0)),
                Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)), 
                Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)), 
                Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
      mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0")
  }
}
