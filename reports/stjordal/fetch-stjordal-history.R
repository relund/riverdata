#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  library(stringr)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

river_id <- 13
station_id <- "124.12.0"
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- seq(current_year - 3L, current_year)

elveguiden_url <- "https://api.elveguiden.no/api/v2/catches/latest-catches"
hydapi_url <- "https://hydapi.nve.no/api/v1/Observations"
hydapi_key <- "di1JUtpivkKyFZ1bGoXF5Q=="

cwd <- normalizePath(getwd(), mustWork = TRUE)
report_dir <- if (file.exists(file.path(cwd, "stjordal.html"))) {
  cwd
} else {
  normalizePath(file.path(cwd, "reports", "stjordal"), mustWork = FALSE)
}
repo_dir <- if (basename(report_dir) == "stjordal" && basename(dirname(report_dir)) == "reports") {
  dirname(dirname(report_dir))
} else {
  cwd
}
output_file <- file.path(report_dir, "stjordal-history.json")
output_js_file <- file.path(report_dir, "stjordal-history.js")
docs_output_file <- file.path(repo_dir, "docs", "stjordal", "stjordal-history.json")
docs_output_js_file <- file.path(repo_dir, "docs", "stjordal", "stjordal-history.js")

neutral_abs_cms <- 1
neutral_rel <- 0.03
neutral_abs_stage_m <- 0.02
neutral_stage_rel <- 0
release_flag_schema_version <- 2

normalize_text <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("æ", "ae") %>%
    str_replace_all("ø", "o") %>%
    str_replace_all("å", "a") %>%
    iconv(from = "", to = "ASCII//TRANSLIT")
}

pluck_chr <- function(x, ..., default = NA_character_) {
  value <- purrr::pluck(x, ..., .default = default)
  if (is.null(value) || length(value) == 0) return(default)
  as.character(value[[1]])
}

pluck_num <- function(x, ..., default = NA_real_) {
  suppressWarnings(as.numeric(pluck_chr(x, ..., default = default)))
}

pluck_bool <- function(x, ..., default = FALSE) {
  value <- purrr::pluck(x, ..., .default = default)
  if (is.null(value) || length(value) == 0) return(default)
  if (is.logical(value)) return(isTRUE(value[[1]]))
  if (is.numeric(value)) return(!is.na(value[[1]]) && value[[1]] != 0)
  normalize_text(value[[1]]) %in% c("1", "true", "ja", "yes")
}

fetch_catch_page <- function(year, page) {
  body <- sprintf(
    '{"page":%d,"river_id":%d,"year":%d,"orderBy":"date","order":"desc","equipment_filter":[],"fish_type_filter":[],"catch_release_filter":[],"boat_filter":[]}',
    page,
    river_id,
    year
  )

  response <- httr::POST(
    elveguiden_url,
    body = body,
    httr::add_headers(
      Accept = "application/json",
      `Content-Type` = "application/json",
      `X-SetLanguage` = "no"
    )
  )
  httr::stop_for_status(response)

  parsed <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  if (!isTRUE(parsed$success) || is.null(parsed$data$catches)) {
    stop("Elveguiden returned an unexpected response for ", year, " page ", page, call. = FALSE)
  }

  parsed$data$catches
}

normalize_catch <- function(item, year) {
  species <- pluck_chr(item, "fish_type", "name_no") %||% pluck_chr(item, "fish_type", "name")

  tibble::tibble(
    id = pluck_chr(item, "id"),
    date = pluck_chr(item, "date"),
    time_of_day = pluck_chr(item, "time_of_day"),
    week = pluck_num(item, "week"),
    year = year,
    river = pluck_chr(item, "river", "name"),
    beat = pluck_chr(item, "beat", "name"),
    fishing_spot = pluck_chr(item, "fishing_spot"),
    fisher_name = pluck_chr(item, "fisher_name"),
    species = species,
    weight_kg = pluck_num(item, "weight"),
    length_cm = pluck_num(item, "length"),
    equipment = pluck_chr(item, "equipment", "name_no") %||% pluck_chr(item, "equipment", "name"),
    released_catch = pluck_bool(item, "released_catch"),
    image = pluck_chr(item, "image"),
    detail_url = paste0("https://elveguiden.no/no/laksebors/catches/", pluck_chr(item, "id"), "?riverId=", river_id)
  )
}

empty_catches <- function() {
  tibble::tibble(
    id = character(),
    date = as.Date(character()),
    time_of_day = character(),
    week = numeric(),
    year = integer(),
    river = character(),
    beat = character(),
    fishing_spot = character(),
    fisher_name = character(),
    species = character(),
    weight_kg = numeric(),
    length_cm = numeric(),
    equipment = character(),
    released_catch = logical(),
    image = character(),
    detail_url = character()
  )
}

empty_cached_catches <- function() {
  tibble::tibble(
    id = character(),
    date = as.Date(character()),
    time_of_day = character(),
    week = numeric(),
    year = integer(),
    river = character(),
    beat = character(),
    fishing_spot = character(),
    fisher_name = character(),
    species = character(),
    weight_kg = numeric(),
    length_cm = numeric(),
    equipment = character(),
    released_catch = logical(),
    image = character(),
    detail_url = character(),
    flow_cms = numeric(),
    flow_trend = character(),
    water_level_m = numeric(),
    water_level_trend = character(),
    water_temperature_c = numeric()
  )
}

empty_daily_measurements <- function() {
  tibble::tibble(
    date = as.Date(character()),
    flow_cms = numeric(),
    flow_trend = character(),
    water_level_m = numeric(),
    water_level_trend = character(),
    water_temperature_c = numeric()
  )
}

read_existing_payload <- function() {
  source_file <- dplyr::case_when(
    file.exists(output_file) ~ output_file,
    file.exists(docs_output_file) ~ docs_output_file,
    TRUE ~ NA_character_
  )
  if (is.na(source_file)) return(NULL)
  message("Reading cached history from ", source_file)
  jsonlite::fromJSON(source_file, simplifyDataFrame = TRUE)
}

cached_years_from_payload <- function(payload) {
  if (is.null(payload$years)) return(integer())
  if (is.null(payload$release_flag_schema_version) || payload$release_flag_schema_version < release_flag_schema_version) {
    message("Cached release flags use an old schema; refreshing all years.")
    return(integer())
  }
  as.integer(payload$years)
}

normalize_cached_catches <- function(payload) {
  if (is.null(payload$catches) || !is.data.frame(payload$catches) || !nrow(payload$catches)) {
    return(empty_cached_catches())
  }
  catches <- tibble::as_tibble(payload$catches)
  for (name in setdiff(names(empty_cached_catches()), names(catches))) {
    catches[[name]] <- NA
  }
  catches %>%
    transmute(
      id = as.character(id),
      date = as.Date(date),
      time_of_day = as.character(time_of_day),
      week = suppressWarnings(as.numeric(week)),
      year = suppressWarnings(as.integer(year)),
      river = as.character(river),
      beat = as.character(beat),
      fishing_spot = as.character(fishing_spot),
      fisher_name = as.character(fisher_name),
      species = as.character(species),
      weight_kg = suppressWarnings(as.numeric(weight_kg)),
      length_cm = suppressWarnings(as.numeric(length_cm)),
      equipment = as.character(equipment),
      released_catch = as.logical(released_catch),
      image = as.character(image),
      detail_url = as.character(detail_url),
      flow_cms = suppressWarnings(as.numeric(flow_cms)),
      flow_trend = as.character(flow_trend),
      water_level_m = suppressWarnings(as.numeric(water_level_m)),
      water_level_trend = as.character(water_level_trend),
      water_temperature_c = suppressWarnings(as.numeric(water_temperature_c))
    )
}

normalize_cached_daily_measurements <- function(payload) {
  if (is.null(payload$daily_flow) || !is.data.frame(payload$daily_flow) || !nrow(payload$daily_flow)) {
    return(empty_daily_measurements())
  }
  daily <- tibble::as_tibble(payload$daily_flow)
  for (name in setdiff(names(empty_daily_measurements()), names(daily))) {
    daily[[name]] <- NA
  }
  daily %>%
    transmute(
      date = as.Date(date),
      flow_cms = suppressWarnings(as.numeric(flow_cms)),
      flow_trend = as.character(flow_trend),
      water_level_m = suppressWarnings(as.numeric(water_level_m)),
      water_level_trend = as.character(water_level_trend),
      water_temperature_c = suppressWarnings(as.numeric(water_temperature_c))
    )
}

fetch_catches_for_year <- function(year) {
  message("Fetching catches for ", year)
  first_page <- fetch_catch_page(year, 1)
  last_page <- first_page$last_page %||% 1
  more_pages <- if (last_page > 1) {
    map(seq.int(2, last_page), ~ fetch_catch_page(year, .x))
  } else {
    list()
  }
  pages <- c(list(first_page), more_pages)
  catch_items <- pages %>%
    map("data") %>%
    flatten()

  if (!length(catch_items)) {
    return(empty_catches())
  }

  catch_items %>%
    map_dfr(normalize_catch, year = year) %>%
    filter(normalize_text(species) == "laks") %>%
    mutate(date = as.Date(date))
}

fetch_measurement_for_year <- function(year, parameter, value_name) {
  message("Fetching ", value_name, " for ", year)
  start <- sprintf("%d-01-01T00:00:00Z", year)
  end <- if (year == current_year) {
    format(with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  } else {
    sprintf("%d-12-31T23:59:59Z", year)
  }

  response <- httr::GET(
    hydapi_url,
    query = list(
      StationId = station_id,
      Parameter = parameter,
      ResolutionTime = 60,
      ReferenceTime = paste(start, end, sep = "/")
    ),
    httr::add_headers(Accept = "application/json", `X-API-Key` = hydapi_key)
  )
  httr::stop_for_status(response)

  station <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)$data[[1]]
  observations <- station$observations %||% list()
  if (!length(observations)) {
    return(tibble::tibble(time = as.POSIXct(character()), value = numeric(), date = as.Date(character())))
  }

  tibble::tibble(
    time = ymd_hms(map_chr(observations, "time", .default = NA_character_), tz = "UTC"),
    value = suppressWarnings(as.numeric(map_dbl(observations, "value", .default = NA_real_)))
  ) %>%
    filter(!is.na(time), !is.na(value)) %>%
    mutate(date = as.Date(with_tz(time, "Europe/Oslo")))
}

daily_direction <- function(value, previous_value, neutral_abs, neutral_relative = 0) {
  if (is.na(value) || is.na(previous_value)) return("neutral")
  delta <- value - previous_value
  threshold <- max(neutral_abs, abs(previous_value) * neutral_relative, na.rm = TRUE)
  case_when(
    delta > threshold ~ "stigende",
    delta < -threshold ~ "faldende",
    TRUE ~ "neutral"
  )
}

existing_payload <- read_existing_payload()
cached_years <- cached_years_from_payload(existing_payload)
years_to_fetch <- sort(unique(c(current_year, setdiff(years, cached_years))))
cached_years_to_keep <- setdiff(years, years_to_fetch)

if (length(cached_years_to_keep)) {
  message("Using cached data for ", paste(cached_years_to_keep, collapse = ", "))
}
message("Fetching fresh data for ", paste(years_to_fetch, collapse = ", "))

cached_catches <- if (is.null(existing_payload)) {
  empty_cached_catches()
} else {
  normalize_cached_catches(existing_payload) %>%
    filter(year %in% cached_years_to_keep)
}

cached_daily_measurements <- if (is.null(existing_payload)) {
  empty_daily_measurements()
} else {
  normalize_cached_daily_measurements(existing_payload) %>%
    filter(as.integer(format(date, "%Y")) %in% cached_years_to_keep)
}

fetched_catches <- map_dfr(years_to_fetch, fetch_catches_for_year)

fetched_daily_flow <- map_dfr(years_to_fetch, ~ fetch_measurement_for_year(.x, 1001, "flow")) %>%
  group_by(date) %>%
  summarise(flow_cms = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

fetched_daily_stage <- map_dfr(years_to_fetch, ~ fetch_measurement_for_year(.x, 1000, "water level")) %>%
  group_by(date) %>%
  summarise(water_level_m = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

fetched_daily_temperature <- map_dfr(years_to_fetch, ~ fetch_measurement_for_year(.x, 1003, "water temperature")) %>%
  group_by(date) %>%
  summarise(water_temperature_c = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

daily_measurements_without_trends <- bind_rows(
  cached_daily_measurements %>%
    select(date, flow_cms, water_level_m, water_temperature_c),
  fetched_daily_flow %>%
    full_join(fetched_daily_stage, by = "date") %>%
    full_join(fetched_daily_temperature, by = "date")
) %>%
  filter(as.integer(format(date, "%Y")) %in% years) %>%
  group_by(date) %>%
  summarise(
    flow_cms = dplyr::last(na.omit(flow_cms), default = NA_real_),
    water_level_m = dplyr::last(na.omit(water_level_m), default = NA_real_),
    water_temperature_c = dplyr::last(na.omit(water_temperature_c), default = NA_real_),
    .groups = "drop"
  ) %>%
  arrange(date)

daily_flow_json <- daily_measurements_without_trends %>%
  mutate(
    previous_flow = lag(flow_cms),
    flow_trend = pmap_chr(
      list(flow_cms, previous_flow),
      ~ daily_direction(..1, ..2, neutral_abs_cms, neutral_rel)
    ),
    previous_water_level = lag(water_level_m),
    water_level_trend = pmap_chr(
      list(water_level_m, previous_water_level),
      ~ daily_direction(..1, ..2, neutral_abs_stage_m, neutral_stage_rel)
    )
  ) %>%
  select(date, flow_cms, flow_trend, water_level_m, water_level_trend, water_temperature_c)

fetched_catches_with_flow <- fetched_catches %>%
  left_join(daily_flow_json, by = "date")

catches_with_flow <- bind_rows(cached_catches, fetched_catches_with_flow) %>%
  filter(year %in% years) %>%
  arrange(desc(date), desc(time_of_day)) %>%
  mutate(
    date = format(date, "%Y-%m-%d"),
    flow_cms = if_else(is.nan(flow_cms), NA_real_, flow_cms),
    water_level_m = if_else(is.nan(water_level_m), NA_real_, water_level_m),
    water_temperature_c = if_else(is.nan(water_temperature_c), NA_real_, water_temperature_c)
  )

daily_flow_json <- daily_flow_json %>%
  mutate(date = format(date, "%Y-%m-%d"))

payload <- list(
  generated_at = format(with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  river_id = river_id,
  station_id = station_id,
  years = years,
  species_filter = "Laks",
  release_flag_schema_version = release_flag_schema_version,
  flow = list(
    parameter = 1001,
    unit = "m3/s",
    trend_source = "daily average flow",
    neutral_abs_cms = neutral_abs_cms,
    neutral_rel = neutral_rel
  ),
  water_level = list(
    parameter = 1000,
    unit = "m",
    trend_source = "daily average water level",
    neutral_abs_m = neutral_abs_stage_m,
    neutral_rel = neutral_stage_rel
  ),
  water_temperature = list(
    parameter = 1003,
    unit = "C",
    source = "daily average water temperature"
  ),
  catches = catches_with_flow,
  daily_flow = daily_flow_json
)

jsonlite::write_json(payload, output_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
message("Wrote ", output_file)
writeLines(
  paste0(
    "window.STJORDAL_HISTORY_DATA = ",
    jsonlite::toJSON(payload, pretty = FALSE, auto_unbox = TRUE, na = "null"),
    ";\n"
  ),
  output_js_file,
  useBytes = TRUE
)
message("Wrote ", output_js_file)

if (dir.exists(dirname(docs_output_file))) {
  jsonlite::write_json(payload, docs_output_file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  message("Wrote ", docs_output_file)
  writeLines(
    paste0(
      "window.STJORDAL_HISTORY_DATA = ",
      jsonlite::toJSON(payload, pretty = FALSE, auto_unbox = TRUE, na = "null"),
      ";\n"
    ),
    docs_output_js_file,
    useBytes = TRUE
  )
  message("Wrote ", docs_output_js_file)
}
