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

#' Write lock web dataset
#'
#' @param dat Flow data.
#' @param prefix File prefix.
#'
#' @return Invisibly returns transformed data.
#' @examples
#' \dontrun{
#' write_lock_web(dat, "data/data_skjern")
#' }
write_lock_web <- function(dat, prefix) {
  writeLockWeb(dat, prefix)
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

#' Write water level moving averages
#'
#' @param dat Water level data.
#' @param prefix File prefix.
#'
#' @return Moving-average table.
#' @examples
#' \dontrun{
#' write_water_mov_avg(dat, "data/data_karup")
#' }
write_water_mov_avg <- function(dat, prefix) {
  writeWaterMovAvg(dat, prefix)
}

#' Write water temperature moving averages
#'
#' @param dat Water temperature data.
#' @param prefix File prefix.
#'
#' @return Moving-average table.
#' @examples
#' \dontrun{
#' write_water_temp_mov_avg(dat, "data/data_karup")
#' }
write_water_temp_mov_avg <- function(dat, prefix) {
  writeWaterTempMovAvg(dat, prefix)
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

#' Write water temperature web dataset
#'
#' @param dat Water temperature data.
#' @param r_means Moving-average reference table.
#' @param prefix File prefix.
#'
#' @return Web dataset.
#' @examples
#' \dontrun{
#' write_water_temp_web(dat, r_means, "data/data_skjern")
#' }
write_water_temp_web <- function(dat, r_means, prefix) {
  writeWaterTempWeb(dat, r_means, prefix)
}

#' Write water level web dataset
#'
#' @param dat Water level data.
#' @param prefix File prefix.
#'
#' @return Web dataset.
#' @examples
#' \dontrun{
#' write_water_levels_web(dat, "data/data_skjern")
#' }
write_water_levels_web <- function(dat, prefix) {
  writeWaterLevelsWeb(dat, prefix)
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

#' Write weight estimates
#'
#' @param prefix File prefix.
#' @param seatrout Whether to estimate seatrout (`TRUE`) or salmon (`FALSE`).
#'
#' @return Weight estimate table.
#' @examples
#' \dontrun{
#' write_weight_estimates("data/data_skjern", seatrout = TRUE)
#' }
write_weight_estimates <- function(prefix, seatrout = TRUE) {
  writeWeightEstimates(prefix, seatrout)
}

#' Save HOBO data files
#'
#' @return Invisibly returns `NULL`.
#' @examples
#' \dontrun{
#' save_hobo_data()
#' }
save_hobo_data <- function() {
  saveHoboData()
}

#' Write time-series data
#'
#' @param stations Station table or `NULL`.
#' @param prefix File prefix.
#' @param prefix1 Type suffix, e.g. `"waterlevel"` or `"watertemp"`.
#' @param days Number of days to read.
#'
#' @return Invisibly returns the latest datetime.
#' @examples
#' \dontrun{
#' write_time_series_data(stations, "data/data_karup", "waterlevel", 15)
#' }
write_time_series_data <- function(stations = NULL, prefix, prefix1, days) {
  writeTimeSeriesData(stations, prefix, prefix1, days)
}

#' Write Skjern lock flow data
#'
#' @param prefix File prefix.
#'
#' @return Lock flow table.
#' @examples
#' \dontrun{
#' write_lock_skjern("data/data_skjern")
#' }
write_lock_skjern <- function(prefix) {
  writeLockSkjern(prefix)
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
