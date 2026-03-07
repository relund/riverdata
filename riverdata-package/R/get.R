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
