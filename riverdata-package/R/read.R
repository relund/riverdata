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


