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
#' read_data(file_prefix = "data_skjern_catch_salmon_", year = 2023:2024)
read_data <- function(
      file_prefix,
      year = NULL,
      path =  "https://raw.githubusercontent.com/relund/riverdata/master/data/"
) {
   if (is.null(year)) path <- str_c(path, file_prefix,".csv") else path <- str_c(path, file_prefix, year, ".csv")
   dat <- read_csv(path, show_col_types = FALSE) %>%
      arrange(Date)
   return(dat)
}

#' Read and prepare catch records
#'
#' Wrapper around legacy catch reader.
#'
#' @param prefix Prefix to csv files, e.g. `"data/data_skjern_catch_salmon"`.
#' @param dat_weight Weight estimate table.
#'
#' @return Catch records.
#' @examples
#' \dontrun{
#' read_catch("data/data_karup_catch_seatrout", tibble::tibble())
#' }
read_catch <- function(prefix, dat_weight) {
  readCatch(prefix, dat_weight)
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
  readWTemp(prefix, years)
}

#' Read and combine data files by filename pattern
#'
#' @param pattern Regex pattern used to match files in `data/`.
#'
#' @return Combined data table.
#' @examples
#' \dontrun{
#' read_data_files("data_karup_waterlevel_[0-9]{4}")
#' }
read_data_files <- function(pattern) {
  readDataFiles(pattern)
}

