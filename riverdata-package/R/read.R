## Functions for reading data stored as csv files


#' Read data files from the `data` subfolder
#'
#' @param file_prefix File prefix, e.g. "data_skjern_catch_salmon_".
#' @param year Year(s) to consider.
#' @param path Path to data folder.
#'
#' @return The data (tibble).
#' @export
#'
#' @examples
#' \dontrun{
#' read_data(file_prefix = "data_skjern_catch_salmon_", year = 2023:2024)
#' }
read_data <- function(
      file_prefix,
      year = NULL,
      path =  "https://raw.githubusercontent.com/relund/riverdata/master/data/"
) {
   if (is.null(year)) path <- str_c(path, file_prefix,".csv") else path <- str_c(path, file_prefix, year, ".csv")
   dat <- read_csv(path, show_col_types = FALSE) %>%
      arrange(.data$Date)
   return(dat)
}

#' Read and prepare catch records for table
#'
#' @param prefix Prefix to csv files e.g. `"../../data/data_skjern_catch_salmon"`.
#' @param dat_weight Weight estimates.
#'
#' @return The catch records.
#' @export
read_catch <- function(prefix, dat_weight) {
  f <- paste0(prefix, "_", 2004:year(now()), ".csv")
  dat_catch <- read_csv(f, col_types = "Dddcfflclfl") %>%
    mutate(
      Weight = if_else(.data$Killed, .data$Weight, NA_real_),
      Place = fct_na_value_to_level(.data$Place, "Ukendt")
    )

  dat_catch <- dat_catch %>%
    mutate(
      Misc = paste0(
        if_else(!.data$Killed, "<img src=\"www/c_and_r.gif\" alt=\"C&R\">", "", ""),
        if_else(.data$Cut, "<img src=\"www/cut.gif\" alt=\"Finneklippet\">", "", ""),
        if_else(.data$Sex == "Han", '<img src="www/boy.gif" alt="Han">', "", ""),
        if_else(.data$Sex == "Hun", '<img src="www/girl.gif" alt="Hun">', "", ""),
        if_else(!is.na(.data$Foto), str_c("<a href=\"", .data$Foto, "\", target=\"_blank\"><img src=\"www/foto.gif\" alt=\"Foto\"></a>"), "", "")
      ),
      Month = factor(month(.data$Date, label = TRUE), ordered = FALSE),
      MonthN = month(.data$Date),
      Week = isoweek(.data$Date),
      Year = year(.data$Date),
      NoWeight = 1 * is.na(.data$Weight),
      MDay = mday(.data$Date),
      DayStr = format(.data$Date, "%d. %b"),
      Day = str_c(formatC(.data$MonthN, width = 2, flag = "0"), "-", formatC(.data$MDay, width = 2, flag = "0"))
    )

  dat_catch <- left_join(dat_catch, dat_weight, by = c("Length", "MonthN")) %>%
    mutate(Weight = if_else(is.na(.data$Weight), round(.data$Avg, 1), .data$Weight)) %>%
    mutate(Fulton = .data$Weight * 100000 / .data$Length^3) %>%
    mutate(Month = month(.data$Date, label = TRUE))

  return(dat_catch)
}

#' Read yearly water temperature files
#'
#' @param prefix File prefix.
#' @param years Integer vector of years.
#'
#' @return Combined water temperature data.
#' @examples
#' \dontrun{
#' read_w_temp("data/data_karup", 2020:2025)
#' }
read_w_temp <- function(prefix, years) {
  dat <- NULL
  for (y in years) {
    fn <- paste0(prefix, "_watertemp_", y, ".csv")
    if (file.exists(fn)) dat <- bind_rows(dat, read_csv(fn, col_types = "Tfd"))
  }
  return(dat)
}

#' Read and combine data files by filename pattern
#'
#' @param pattern Regex pattern used to match files in `data/`.
#' @param path Optional base data path. If `NULL`, a sensible relative path is
#'   auto-detected from the current working directory.
#'
#' @return Combined data table.
#' @examples
#' \dontrun{
#' read_data_files("data_karup_waterlevel_[0-9]{4}")
#' }
read_data_files <- function(pattern, path = NULL) {
  if (is.null(path)) {
    candidates <- c("data", "../data", "../../data")
    path <- candidates[fs::dir_exists(candidates)][1]
    if (is.na(path)) {
      stop("Could not locate a data directory. Provide `path` explicitly.")
    }
  }
  f <- fs::dir_ls(path, regexp = pattern)
  dat <- read_csv(f, col_types = "Tfd") %>%
    arrange(.data$Date)
  return(dat)
}
