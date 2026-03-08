## Functions for getting and transforming data


#' Prepare catch records for table
#'
#' @param datCatch Catch table.
#' @param datWeight Weight estimates for missing data.
#'
#' @return The catch record table.
#' @export
get_catch_table <- function(datCatch, datWeight) {
   datCatch <- datCatch  %>%
      mutate(Weight = if_else(Killed, Weight, NA_real_), Place = fct_na_value_to_level(Place, "Ukendt"))

   datCatch <- datCatch %>%
      mutate(
         Misc = paste0(
            if_else(!Killed, "<img src=\"www/c_and_r.gif\" alt=\"C&R\">", "", ""),
            if_else(Cut, "<img src=\"www/cut.gif\" alt=\"Finneklippet\">", "", ""),
            if_else(Sex == 'Han', '<img src="www/boy.gif" alt="Han">', "", ""),
            if_else(Sex == 'Hun', '<img src="www/girl.gif" alt="Hun">', "", ""),
            if_else(!is.na(Foto),str_c("<a href=\"", Foto, "\", target=\"_blank\"><img src=\"www/foto.gif\" alt=\"Foto\"></a>"),"", "")
         ),
         Month = factor(month(Date, label = T), ordered = F), MonthN = month(Date), Week = isoweek(Date), Year = year(Date),
         NoWeight = 1*is.na(Weight), MDay = mday(Date), DayStr = format(Date, "%d. %b"),
         Day = str_c(formatC(MonthN, width = 2, flag = "0"),
                     "-",
                     formatC(MDay, width = 2, flag = "0"))
      )
   datCatch <- left_join(datCatch, datWeight, by = c("Length", "MonthN")) %>%
      mutate(Weight = if_else(is.na(Weight), round(Avg,1), Weight)) %>%
      mutate(Fulton = Weight*100000/Length^3) %>%
      mutate(Month = month(Date, label = T))
   return(datCatch)
}

#' Calculate yearly catch statistics
#'
#' @param dat_catch Catch records.
#'
#' @return Yearly summary statistics.
#' @examples
#' \dontrun{
#' yearly_stat(dat_catch)
#' }
yearly_stat <- function(dat_catch) {
  dat <- dat_catch %>%
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
                             round(100*`TotalP_Ukendt`/Total, digits = 0)),
              PlaceK = paste0(round(100*KilledP_Nedre/Killed, digits = 0), "/",
                              round(100*KilledP_Mellem/Killed, digits = 0), "/",
                              round(100*`KilledP_Øvre`/Killed, digits = 0), "/",
                              round(100*`KilledP_Vorgod Å`/Killed, digits = 0), "/",
                              round(100*`KilledP_Omme Å`/Killed, digits = 0), "/",
                              round(100*`KilledP_Ukendt`/Killed, digits = 0)),
              Method = paste0(round(100*Flue/Total, digits = 0), "/",
                              round(100*Spin/Total, digits = 0), "/",
                              round(100*Orm/Total, digits = 0), "/",
                              round(100*Ukendt/Total, digits = 0)),
              Released = paste0(round(100*Released/Total, 0), "/",
                                round(100*Killed/Total, 0)),
              Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)),
              Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)),
              Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
    mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0")
}

#' Calculate monthly catch statistics
#'
#' @param dat_catch Catch records.
#' @param year Year under consideration.
#'
#' @return Monthly summary statistics.
#' @examples
#' \dontrun{
#' monthly_stat(dat_catch, 2025)
#' }
monthly_stat <- function(dat_catch, year) {
  dat <- dat_catch %>%
    filter(year(Date) == year) %>%
    mutate(Month = month(Date, label = TRUE)) %>% group_by(Month) %>% nest() %>%
    mutate(keep = map_lgl(data, function(df) { # remove months where no weight or length
      if_else(nrow(df) == sum(is.na(df$Length)) | nrow(df) == sum(is.na(df$Weight)), FALSE, TRUE)
    })) %>%
    filter(keep) %>%
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
        PlaceK = "0/0/0/0/0/0",
        Method = "0/0/0/0",
        Released = "0/0",
        Length = "0/0",
        Weight = "0/0/0",
        Fulton = "0/0"
      )
  } else {
    cNames <- c("Month", "Total", "Male", "Female", "SexUnknown", "TotalP_Nedre", "TotalP_Mellem",
                "TotalP_Øvre", "TotalP_Vorgod Å", "TotalP_Omme Å", "TotalP_Ukendt",
                "KilledP_Nedre", "KilledP_Mellem", "KilledP_Øvre", "KilledP_Vorgod Å",
                "KilledP_Omme Å", "KilledP_Ukendt", "Flue", "Spin", "Orm", "Ukendt", "Released",
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
                               round(100*`TotalP_Ukendt`/Total, digits = 0)),
                PlaceK = paste0(round(100*KilledP_Nedre/Killed, digits = 0), "/",
                                round(100*KilledP_Mellem/Killed, digits = 0), "/",
                                round(100*`KilledP_Øvre`/Killed, digits = 0), "/",
                                round(100*`KilledP_Vorgod Å`/Killed, digits = 0), "/",
                                round(100*`KilledP_Omme Å`/Killed, digits = 0), "/",
                                round(100*`KilledP_Ukendt`/Killed, digits = 0)),
                Method = paste0(round(100*Flue/Total, digits = 0), "/",
                                round(100*Spin/Total, digits = 0), "/",
                                round(100*Orm/Total, digits = 0), "/",
                                round(100*Ukendt/Total, digits = 0)),
                Released = paste0(round(100*Released/Total, 0), "/",
                                  round(100*Killed/Total, 0)),
                Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)),
                Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)),
                Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
      mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0")
  }
}

#' Calculate yearly catch statistics for Karup
#'
#' @param dat_catch Catch records.
#'
#' @return Yearly summary statistics.
#' @examples
#' \dontrun{
#' yearly_stat_karup(dat_catch)
#' }
yearly_stat_karup <- function(dat_catch) {
  dat <- dat_catch %>%
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
    if (!("TotalP_Ukendt" %in% names(dat))) dat <- dat %>% mutate(TotalP_Ukendt = 0, KilledP_Ukendt = 0)
  dat <-
    dat  %>%
    ungroup() %>%
    transmute(Year, Total,
              Sex = paste0(round(100*Male/Total, 0), "/",
                           round(100*Female/Total, 0), "/",
                           round(100*SexUnknown/Total, 0)),
              Place = paste0(round(100*Nedre/Total, 0), "/",
                             round(100*Mellem/Total, 0), "/",
                             round(100*`Øvre`/Total, 0), "/",
                             round(100*`Haderis Å`/Total, 0), "/",
                             round(100*(Total - Nedre - Mellem - `Øvre` - `Haderis Å`)/Total, 0)),
              Method = paste0(round(100*Flue/Total, 0), "/",
                              round(100*Spin/Total, 0), "/",
                              round(100*Orm/Total, 0), "/",
                              round(100*(Total - Flue - Spin - Orm)/Total, 0)),
              Released = paste0(round(100*Released/Total, 0), "/",
                                round(100*(Total - Released)/Total, 0)),
              Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)),
              Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)),
              Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
    mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0") %>%
    arrange(desc(Year))
}

#' Calculate monthly catch statistics for Karup
#'
#' @param dat_catch Catch records.
#' @param year Year under consideration.
#'
#' @return Monthly summary statistics.
#' @examples
#' \dontrun{
#' monthly_stat_karup(dat_catch, 2025)
#' }
monthly_stat_karup <- function(dat_catch, year) {
  dat <- dat_catch %>%
    filter(year(Date) == year) %>%
    mutate(Month = month(Date, label = TRUE)) %>% group_by(Month) %>% nest() %>%
    mutate(keep = map_lgl(data, function(df) { # remove months where no weight or length
      if_else(nrow(df) == sum(is.na(df$Length)) | nrow(df) == sum(is.na(df$Weight)), FALSE, TRUE)
    })) %>%
    filter(keep) %>%
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
    unnest(cols = c(TotalStat, PlaceStat, MethodStat)) %>% select(-data) %>%
    replace(., is.na(.), 0)
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
    cNames <- c("Month", "Total", "Male", "Female", "SexUnknown", "Nedre", "Mellem", "Øvre",
                "Haderis Å", "Flue", "Spin", "Orm", "Released", "LengthAvg", "LengthMax",
                "WeightAvg", "WeightMax", "FultonAvg", "FultonMax")
    cNames <- cNames[!(cNames %in% names(dat))]
    cols <- rep(0, length(cNames))
    names(cols) = cNames
    dat <- dat %>% add_column(!!!cols)
    dat <-
      dat  %>%
      ungroup() %>%
      transmute(Month, Total,
                Sex = paste0(round(100*Male/Total, 0), "/",
                             round(100*Female/Total, 0), "/",
                             round(100*SexUnknown/Total, 0)),
                Place = paste0(round(100*Nedre/Total, 0), "/",
                               round(100*Mellem/Total, 0), "/",
                               round(100*`Øvre`/Total, 0), "/",
                               round(100*`Haderis Å`/Total, 0), "/",
                               round(100*(Total - Nedre - Mellem - `Øvre` - `Haderis Å`)/Total, 0)),
                Method = paste0(round(100*Flue/Total, 0), "/",
                                round(100*Spin/Total, 0), "/",
                                round(100*Orm/Total, 0), "/",
                                round(100*(Total - Flue - Spin - Orm)/Total, 0)),
                Released = paste0(round(100*Released/Total, 0), "/",
                                  round(100*(Total - Released)/Total, 0)),
                Length = paste0(round(LengthAvg,0), "/", round(LengthMax,0)),
                Weight = paste0(round(WeightAvg,1), "/", round(WeightMax,1), "/", round(Kg,0)),
                Fulton = paste0(round(FultonAvg,2), "/", round(FultonMax,2))) %>%
      mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0")
  }
}

#' Calculate relative water levels
#'
#' @param dat Water level data.
#' @param r_means Moving-average reference table.
#' @param prefix File prefix.
#'
#' @return Relative water-level data.
#' @examples
#' \dontrun{
#' calc_water_level_relative(dat, r_means, "data/data_karup")
#' }
calc_water_level_relative <- function(dat, r_means, prefix) {
  message("Waterlevel: Calc relative values.")
  dat <- dat %>%
    mutate(Day = yday(Date)) %>%
    left_join(r_means, by = c("Place", "Day")) %>%
    mutate(Level = round(Value, 3), LevelRelative = round(Level - Level_rAvg90, 3)) %>%
    select(-Day, -Level_rAvg90)
  return(dat)
}
