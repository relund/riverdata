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
      mutate(Weight = if_else(.data$Killed, .data$Weight, NA_real_), Place = fct_na_value_to_level(.data$Place, "Ukendt"))

   datCatch <- datCatch %>%
      mutate(
         Misc = paste0(
            if_else(!.data$Killed, "<img src=\"www/c_and_r.gif\" alt=\"C&R\">", "", ""),
            if_else(.data$Cut, "<img src=\"www/cut.gif\" alt=\"Finneklippet\">", "", ""),
            if_else(.data$Sex == 'Han', '<img src="www/boy.gif" alt="Han">', "", ""),
            if_else(.data$Sex == 'Hun', '<img src="www/girl.gif" alt="Hun">', "", ""),
            if_else(!is.na(.data$Foto),str_c("<a href=\"", .data$Foto, "\", target=\"_blank\"><img src=\"www/foto.gif\" alt=\"Foto\"></a>"),"", "")
         ),
         Month = factor(month(.data$Date, label = T), ordered = F), MonthN = month(.data$Date), Week = isoweek(.data$Date), Year = year(.data$Date),
         NoWeight = 1*is.na(.data$Weight), MDay = mday(.data$Date), DayStr = format(.data$Date, "%d. %b"),
         Day = str_c(formatC(.data$MonthN, width = 2, flag = "0"),
                     "-",
                     formatC(.data$MDay, width = 2, flag = "0"))
      )
   datCatch <- left_join(datCatch, datWeight, by = c("Length", "MonthN")) %>%
      mutate(Weight = if_else(is.na(.data$Weight), round(.data$Avg,1), .data$Weight)) %>%
      mutate(Fulton = .data$Weight*100000/.data$Length^3) %>%
      mutate(Month = month(.data$Date, label = T))
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
    mutate(Year = year(.data$Date)) %>% group_by(.data$Year) %>% nest() %>%
    mutate(
      TotalStat = map(.data$data, function(df) {
        summarise(df, Total = n(),
                  Female = sum(.data$Sex == "Hun", na.rm = T),
                  Male = sum(.data$Sex == "Han", na.rm = T),
                  SexUnknown = .data$Total - .data$Female - .data$Male,
                  Released = sum(!.data$Killed, na.rm = T),
                  Killed = sum(.data$Killed, na.rm = T),
                  KilledUnknown = .data$Total - .data$Released - .data$Killed,
                  LengthAvg = mean(.data$Length, na.rm = T),
                  LengthMax = max(.data$Length, na.rm = T),
                  WeightAvg = mean(.data$Weight, na.rm = T),
                  WeightMax = max(.data$Weight, na.rm = T),
                  Kg = sum(.data$Weight, na.rm = T),
                  FultonAvg = mean(.data$Fulton, na.rm = T),
                  FultonMax = max(.data$Fulton, na.rm = T)
        )
      }),
      PlaceStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Place) %>%
                summarize(TotalP = n(),
                          KilledP = sum(.data$Killed))}),
      MethodStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Method) %>%
                summarize(TotalM = n())})
    )

  dat <- dat %>%
    mutate(PlaceStat =
             map(.data$PlaceStat, function(df) {
               pivot_wider(df, names_from = "Place", values_from = c("TotalP", "KilledP"))}),
           MethodStat =
             map(.data$MethodStat, function(df) {
               pivot_wider(df, names_from = "Method", values_from = "TotalM")})
    ) %>%
    unnest(cols = all_of(c("TotalStat", "PlaceStat", "MethodStat"))) %>% select(-all_of("data")) %>%
    mutate(across(everything(), ~replace(.x, is.na(.x), 0)))

  dat <-
    dat  %>%
    ungroup() %>%
    transmute(Year = .data$Year, Total = .data$Total,
              Sex = paste0(round(100*.data$Male/.data$Total, digits = 0), "/",
                           round(100*.data$Female/.data$Total, digits = 0), "/",
                           round(100*.data$SexUnknown/.data$Total, digits = 0)),
              Place = paste0(round(100*.data$TotalP_Nedre/.data$Total, digits = 0), "/",
                             round(100*.data$TotalP_Mellem/.data$Total, digits = 0), "/",
                             round(100*.data[["TotalP_Øvre"]]/.data$Total, digits = 0), "/",
                             round(100*.data[["TotalP_Vorgod Å"]]/.data$Total, digits = 0), "/",
                             round(100*.data[["TotalP_Omme Å"]]/.data$Total, digits = 0), "/",
                             round(100*.data$TotalP_Ukendt/.data$Total, digits = 0)),
              PlaceK = paste0(round(100*.data$KilledP_Nedre/.data$Killed, digits = 0), "/",
                              round(100*.data$KilledP_Mellem/.data$Killed, digits = 0), "/",
                              round(100*.data[["KilledP_Øvre"]]/.data$Killed, digits = 0), "/",
                              round(100*.data[["KilledP_Vorgod Å"]]/.data$Killed, digits = 0), "/",
                              round(100*.data[["KilledP_Omme Å"]]/.data$Killed, digits = 0), "/",
                              round(100*.data$KilledP_Ukendt/.data$Killed, digits = 0)),
              Method = paste0(round(100*.data$Flue/.data$Total, digits = 0), "/",
                              round(100*.data$Spin/.data$Total, digits = 0), "/",
                              round(100*.data$Orm/.data$Total, digits = 0), "/",
                              round(100*.data$Ukendt/.data$Total, digits = 0)),
              Released = paste0(round(100*.data$Released/.data$Total, 0), "/",
                                round(100*.data$Killed/.data$Total, 0)),
              Length = paste0(round(.data$LengthAvg,0), "/", round(.data$LengthMax,0)),
              Weight = paste0(round(.data$WeightAvg,1), "/", round(.data$WeightMax,1), "/", round(.data$Kg,0)),
              Fulton = paste0(round(.data$FultonAvg,2), "/", round(.data$FultonMax,2))) %>%
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
    filter(year(.data$Date) == year) %>%
    mutate(Month = month(.data$Date, label = TRUE)) %>% group_by(.data$Month) %>% nest() %>%
    mutate(keep = map_lgl(.data$data, function(df) { # remove months where no weight or length
      if_else(nrow(df) == sum(is.na(df$Length)) | nrow(df) == sum(is.na(df$Weight)), FALSE, TRUE)
    })) %>%
    filter(.data$keep) %>%
    mutate(
      TotalStat = map(.data$data, function(df) {
        summarise(df, Total = n(),
                  Female = sum(.data$Sex == "Hun", na.rm = T),
                  Male = sum(.data$Sex == "Han", na.rm = T),
                  SexUnknown = .data$Total - .data$Female - .data$Male,
                  Released = sum(!.data$Killed, na.rm = T),
                  Killed = sum(.data$Killed, na.rm = T),
                  KilledUnknown = .data$Total - .data$Released - .data$Killed,
                  LengthAvg = mean(.data$Length, na.rm = T),
                  LengthMax = max(.data$Length, na.rm = T),
                  WeightAvg = mean(.data$Weight, na.rm = T),
                  WeightMax = max(.data$Weight, na.rm = T),
                  Kg = sum(.data$Weight, na.rm = T),
                  FultonAvg = mean(.data$Fulton, na.rm = T),
                  FultonMax = max(.data$Fulton, na.rm = T)
        )
      }),
      PlaceStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Place) %>%
                summarize(TotalP = n(),
                          KilledP = sum(.data$Killed))}),
      MethodStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Method) %>%
                summarize(TotalM = n())})
    )

  dat <- dat %>%
    mutate(PlaceStat =
             map(.data$PlaceStat, function(df) {
               pivot_wider(df, names_from = "Place", values_from = c("TotalP", "KilledP"))}),
           MethodStat =
             map(.data$MethodStat, function(df) {
               pivot_wider(df, names_from = "Method", values_from = "TotalM")})
    ) %>%
    unnest(cols = all_of(c("TotalStat", "PlaceStat", "MethodStat"))) %>% select(-all_of("data")) %>%
    mutate(across(everything(), ~replace(.x, is.na(.x), 0)))

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
      transmute(Month = .data$Month, Total = .data$Total,
                Sex = paste0(round(100*.data$Male/.data$Total, digits = 0), "/",
                             round(100*.data$Female/.data$Total, digits = 0), "/",
                             round(100*.data$SexUnknown/.data$Total, digits = 0)),
                Place = paste0(round(100*.data$TotalP_Nedre/.data$Total, digits = 0), "/",
                               round(100*.data$TotalP_Mellem/.data$Total, digits = 0), "/",
                               round(100*.data[["TotalP_Øvre"]]/.data$Total, digits = 0), "/",
                               round(100*.data[["TotalP_Vorgod Å"]]/.data$Total, digits = 0), "/",
                               round(100*.data[["TotalP_Omme Å"]]/.data$Total, digits = 0), "/",
                               round(100*.data$TotalP_Ukendt/.data$Total, digits = 0)),
                PlaceK = paste0(round(100*.data$KilledP_Nedre/.data$Killed, digits = 0), "/",
                                round(100*.data$KilledP_Mellem/.data$Killed, digits = 0), "/",
                                round(100*.data[["KilledP_Øvre"]]/.data$Killed, digits = 0), "/",
                                round(100*.data[["KilledP_Vorgod Å"]]/.data$Killed, digits = 0), "/",
                                round(100*.data[["KilledP_Omme Å"]]/.data$Killed, digits = 0), "/",
                                round(100*.data$KilledP_Ukendt/.data$Killed, digits = 0)),
                Method = paste0(round(100*.data$Flue/.data$Total, digits = 0), "/",
                                round(100*.data$Spin/.data$Total, digits = 0), "/",
                                round(100*.data$Orm/.data$Total, digits = 0), "/",
                                round(100*.data$Ukendt/.data$Total, digits = 0)),
                Released = paste0(round(100*.data$Released/.data$Total, 0), "/",
                                  round(100*.data$Killed/.data$Total, 0)),
                Length = paste0(round(.data$LengthAvg,0), "/", round(.data$LengthMax,0)),
                Weight = paste0(round(.data$WeightAvg,1), "/", round(.data$WeightMax,1), "/", round(.data$Kg,0)),
                Fulton = paste0(round(.data$FultonAvg,2), "/", round(.data$FultonMax,2))) %>%
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
    mutate(Year = year(.data$Date)) %>% group_by(.data$Year) %>% nest() %>%
    mutate(
      TotalStat = map(.data$data, function(df) {
        summarise(df, Total = n(),
                  Female = sum(.data$Sex == "Hun", na.rm = T),
                  Male = sum(.data$Sex == "Han", na.rm = T),
                  SexUnknown = .data$Total - .data$Female - .data$Male,
                  Released = sum(!.data$Killed, na.rm = T),
                  Killed = sum(.data$Killed, na.rm = T),
                  KilledUnknown = .data$Total - .data$Released - .data$Killed,
                  LengthAvg = mean(.data$Length, na.rm = T),
                  LengthMax = max(.data$Length, na.rm = T),
                  WeightAvg = mean(.data$Weight, na.rm = T),
                  WeightMax = max(.data$Weight, na.rm = T),
                  Kg = sum(.data$Weight, na.rm = T),
                  FultonAvg = mean(.data$Fulton, na.rm = T),
                  FultonMax = max(.data$Fulton, na.rm = T)
        )
      }),
      PlaceStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Place) %>%
                summarize(TotalP = n())}),
      MethodStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Method) %>%
                summarize(TotalM = n())})
    )

  dat <- dat %>%
    mutate(PlaceStat =
             map(.data$PlaceStat, function(df) {
               pivot_wider(df, names_from = "Place", values_from = "TotalP")}),
           MethodStat =
             map(.data$MethodStat, function(df) {
               pivot_wider(df, names_from = "Method", values_from = "TotalM")})
    ) %>%
    unnest(cols = all_of(c("TotalStat", "PlaceStat", "MethodStat")), names_repair = "unique") %>% select(-all_of("data")) %>%
    mutate(across(everything(), ~replace(.x, is.na(.x), 0)))

  dat <-
    if (!("TotalP_Ukendt" %in% names(dat))) dat <- dat %>% mutate(TotalP_Ukendt = 0, KilledP_Ukendt = 0)
  dat <-
    dat  %>%
    ungroup() %>%
    transmute(Year = .data$Year, Total = .data$Total,
              Sex = paste0(round(100*.data$Male/.data$Total, 0), "/",
                           round(100*.data$Female/.data$Total, 0), "/",
                           round(100*.data$SexUnknown/.data$Total, 0)),
              Place = paste0(round(100*.data$Nedre/.data$Total, 0), "/",
                             round(100*.data$Mellem/.data$Total, 0), "/",
                             round(100*.data[["Øvre"]]/.data$Total, 0), "/",
                             round(100*.data[["Haderis Å"]]/.data$Total, 0), "/",
                             round(100*(.data$Total - .data$Nedre - .data$Mellem - .data[["Øvre"]] - .data[["Haderis Å"]])/.data$Total, 0)),
              Method = paste0(round(100*.data$Flue/.data$Total, 0), "/",
                              round(100*.data$Spin/.data$Total, 0), "/",
                              round(100*.data$Orm/.data$Total, 0), "/",
                              round(100*(.data$Total - .data$Flue - .data$Spin - .data$Orm)/.data$Total, 0)),
              Released = paste0(round(100*.data$Released/.data$Total, 0), "/",
                                round(100*(.data$Total - .data$Released)/.data$Total, 0)),
              Length = paste0(round(.data$LengthAvg,0), "/", round(.data$LengthMax,0)),
              Weight = paste0(round(.data$WeightAvg,1), "/", round(.data$WeightMax,1), "/", round(.data$Kg,0)),
              Fulton = paste0(round(.data$FultonAvg,2), "/", round(.data$FultonMax,2))) %>%
    mutate_if(is.character, str_replace_all, pattern = "NaN|NA", replacement = "0") %>%
    arrange(desc(.data$Year))
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
    filter(year(.data$Date) == year) %>%
    mutate(Month = month(.data$Date, label = TRUE)) %>% group_by(.data$Month) %>% nest() %>%
    mutate(keep = map_lgl(.data$data, function(df) { # remove months where no weight or length
      if_else(nrow(df) == sum(is.na(df$Length)) | nrow(df) == sum(is.na(df$Weight)), FALSE, TRUE)
    })) %>%
    filter(.data$keep) %>%
    mutate(
      TotalStat = map(.data$data, function(df) {
        summarise(df, Total = n(),
                  Female = sum(.data$Sex == "Hun", na.rm = T),
                  Male = sum(.data$Sex == "Han", na.rm = T),
                  SexUnknown = .data$Total - .data$Female - .data$Male,
                  Released = sum(!.data$Killed, na.rm = T),
                  Killed = sum(.data$Killed, na.rm = T),
                  KilledUnknown = .data$Total - .data$Released - .data$Killed,
                  LengthAvg = mean(.data$Length, na.rm = T),
                  LengthMax = max(.data$Length, na.rm = T),
                  WeightAvg = mean(.data$Weight, na.rm = T),
                  WeightMax = max(.data$Weight, na.rm = T),
                  Kg = sum(.data$Weight, na.rm = T),
                  FultonAvg = mean(.data$Fulton, na.rm = T),
                  FultonMax = max(.data$Fulton, na.rm = T)
        )
      }),
      PlaceStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Place) %>%
                summarize(TotalP = n())}),
      MethodStat =
        map(.data$data,
            function(df) {
              df %>%
                group_by(.data$Method) %>%
                summarize(TotalM = n())})
    )

  dat <- dat %>%
    mutate(PlaceStat =
             map(.data$PlaceStat, function(df) {
               pivot_wider(df, names_from = "Place", values_from = "TotalP")}),
           MethodStat =
             map(.data$MethodStat, function(df) {
               pivot_wider(df, names_from = "Method", values_from = "TotalM")})
    ) %>%
    unnest(cols = all_of(c("TotalStat", "PlaceStat", "MethodStat"))) %>% select(-all_of("data")) %>%
    mutate(across(everything(), ~replace(.x, is.na(.x), 0)))
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
      transmute(Month = .data$Month, Total = .data$Total,
                Sex = paste0(round(100*.data$Male/.data$Total, 0), "/",
                             round(100*.data$Female/.data$Total, 0), "/",
                             round(100*.data$SexUnknown/.data$Total, 0)),
                Place = paste0(round(100*.data$Nedre/.data$Total, 0), "/",
                               round(100*.data$Mellem/.data$Total, 0), "/",
                               round(100*.data[["Øvre"]]/.data$Total, 0), "/",
                               round(100*.data[["Haderis Å"]]/.data$Total, 0), "/",
                               round(100*(.data$Total - .data$Nedre - .data$Mellem - .data[["Øvre"]] - .data[["Haderis Å"]])/.data$Total, 0)),
                Method = paste0(round(100*.data$Flue/.data$Total, 0), "/",
                                round(100*.data$Spin/.data$Total, 0), "/",
                                round(100*.data$Orm/.data$Total, 0), "/",
                                round(100*(.data$Total - .data$Flue - .data$Spin - .data$Orm)/.data$Total, 0)),
                Released = paste0(round(100*.data$Released/.data$Total, 0), "/",
                                  round(100*(.data$Total - .data$Released)/.data$Total, 0)),
                Length = paste0(round(.data$LengthAvg,0), "/", round(.data$LengthMax,0)),
                Weight = paste0(round(.data$WeightAvg,1), "/", round(.data$WeightMax,1), "/", round(.data$Kg,0)),
                Fulton = paste0(round(.data$FultonAvg,2), "/", round(.data$FultonMax,2))) %>%
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
    mutate(Day = yday(.data$Date)) %>%
    left_join(r_means, by = c("Place", "Day")) %>%
    mutate(Level = round(.data$Value, 3), LevelRelative = round(.data$Level - .data$Level_rAvg90, 3)) %>%
    select(-all_of(c("Day", "Level_rAvg90")))
  return(dat)
}
