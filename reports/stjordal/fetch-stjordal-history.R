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
    released_catch = isTRUE(purrr::pluck(item, "released_catch", .default = FALSE)),
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
    filter(str_detect(normalize_text(species), "laks")) %>%
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

catches <- map_dfr(years, fetch_catches_for_year)

daily_flow <- map_dfr(years, ~ fetch_measurement_for_year(.x, 1001, "flow")) %>%
  group_by(date) %>%
  summarise(flow_cms = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date) %>%
  mutate(
    previous_flow = lag(flow_cms),
    flow_trend = pmap_chr(
      list(flow_cms, previous_flow),
      ~ daily_direction(..1, ..2, neutral_abs_cms, neutral_rel)
    )
  ) %>%
  select(date, flow_cms, flow_trend)

daily_stage <- map_dfr(years, ~ fetch_measurement_for_year(.x, 1000, "water level")) %>%
  group_by(date) %>%
  summarise(water_level_m = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date) %>%
  mutate(
    previous_water_level = lag(water_level_m),
    water_level_trend = pmap_chr(
      list(water_level_m, previous_water_level),
      ~ daily_direction(..1, ..2, neutral_abs_stage_m, neutral_stage_rel)
    )
  ) %>%
  select(date, water_level_m, water_level_trend)

catches_with_flow <- catches %>%
  left_join(daily_flow, by = "date") %>%
  left_join(daily_stage, by = "date") %>%
  arrange(desc(date), desc(time_of_day)) %>%
  mutate(
    date = format(date, "%Y-%m-%d"),
    flow_cms = if_else(is.nan(flow_cms), NA_real_, flow_cms),
    water_level_m = if_else(is.nan(water_level_m), NA_real_, water_level_m)
  )

daily_flow_json <- daily_flow %>%
  left_join(daily_stage, by = "date") %>%
  mutate(date = format(date, "%Y-%m-%d"))

payload <- list(
  generated_at = format(with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  river_id = river_id,
  station_id = station_id,
  years = years,
  species_filter = "Laks",
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
