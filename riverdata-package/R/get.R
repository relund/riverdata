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
  yearlyStat(dat_catch)
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
  monthlyStat(dat_catch, year)
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
  yearlyStatKarup(dat_catch)
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
  monthlyStatKarup(dat_catch, year)
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
  calcWaterLevelRelative(dat, r_means, prefix)
}

#' Parse KML map data into marker/line tables
#'
#' @param map_id Google map ID.
#' @param club Optional club label.
#' @param group_name_markers Optional marker group name filter.
#' @param group_name_lines Optional line group name filter.
#'
#' @return List with `datMarkers` and `datLines`.
#' @examples
#' \dontrun{
#' strip_kml("1XJoAUKY_-kbmhZgovPpLgi82Gn8")
#' }
strip_kml <- function(
    map_id,
    club = NA,
    group_name_markers = NULL,
    group_name_lines = NULL
) {
  stripKml(map_id, club, group_name_markers, group_name_lines)
}
