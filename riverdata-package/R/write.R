## Functions for write/save data


#' Save fangstjournalen catch records from a given year to a csv file
#'
#' Data is written to `str_c(prefix, "_catch_", res, "_", yr, ".csv")`.
#'
#' @param url Url for json (without year) ending with a slash.
#' @param yr Year to get data for (single number).
#' @param prefix Prefix path for the csv file.
#' @param species Species. Either "seatrout" or "salmon".
#' @param club False if consider association. True if consider club.
#'
#' @return The data (tibble).
#' @export
#'
#' @examples
#' url <- str_c("https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/",
#'              "catches/assoc/A97F957DD48AEDD4C1258814003E71FE/1/")
#' prefix <- "tmp/data_skjern"
#' yr <- 2023
#' write_catch(url, prefix, yr, species = "Laks")
#'
write_catch <-
   function(url,
            prefix,
            yr,
            species = "seatrout",
            club = FALSE,
            write = TRUE) {
      message("Catch records: Write dataset for year ", yr)
      ## data to today
      dat <- jsonlite::fromJSON(str_c(url, yr))
      cols <- dat$cols
      cols$label[is.na(cols$label)] <- "Unknown"
      rows <- dat$rows$c
      if (is.null(rows)) {
         fn <- paste0(prefix, "_catch_salmon_", yr - 1, ".csv")
         dat3 <- read_csv(fn, col_types = "Dddcfflclfl") |>
            slice_head(n = 0)  # get col names from last year
      } else {
         rows <- lapply(
            rows,
            FUN = function(x) {
               x[, 1]
            }
         )
         dat1 <-  suppressMessages(t(map_dfc(rows, ~ .x)))
         colnames(dat1) <- cols$label
         dat1 <-
            suppressMessages(as_tibble(dat1, .name_repair = "universal"))
         dateStr <-
            dat1$Dato |> str_extract_all("(?<=\\().+?(?=\\))", simplify = T) |>
            str_split(",", simplify = TRUE) |> as_tibble(.name_repair = "minimal")
         colnames(dateStr) <- c("Year", "Month", "Day")
         dateStr <- suppressMessages(type_convert(dateStr))
         dateStr <- mutate(dateStr, "Month" = .data$Month + 1)
         dateStr <-
            str_c(
               dateStr$Year,
               "-",
               str_pad(dateStr$Month, 2, "left", pad = "0"),
               "-",
               str_pad(dateStr$Day, 2, "left", pad = "0")
            )
         dat1 <- suppressMessages(bind_cols(Date = dateStr, dat1))
         if (club)
            dat1 <- dat1 |>
            rename(River = .data$Fiskevand)
         if (species == "seatrout")
            dat1 <- dat1 |> dplyr::filter(str_detect(.data$Art, "Havørred"))
         if (species == "salmon")
            dat1 <- dat1 |> dplyr::filter(str_detect(.data$Art, "Laks"))
         sex_col <- intersect(c("Køn...17", "Køn"), names(dat1))
         dat1$SexTmp <- if (length(sex_col) > 0) dat1[[sex_col[1]]] else NA_character_
         if (!club)
            dat2 <- dat1 |>
            transmute(
               "Date" = .data$Date,
               "Length" = .data$`Længde`,
               "Weight" = .data$`Vægt`,
               "Name" = .data$Navn,
               "Place" = .data$Zone,
               "Method" = .data$Agn,
               "Cut" = NA_character_,
               "Foto" = .data$Foto,
               "Killed" = (.data$Hjemtaget == "Ja"),
               "Sex" = .data$SexTmp,
               "Net" = .data$Garnskadet
            )
         if (club)
            dat2 <- dat1 |>
            transmute(
               "Date" = .data$Date,
               "Length" = .data$`Længde`,
               "Weight" = .data$`Vægt`,
               "Name" = .data$Navn,
               "River" = .data$River,
               "Place" = .data$Strækning.sted,
               "Method" = .data$Agn,
               "Cut" = NA_character_,
               "Foto" = .data$Foto,
               "Killed" = (.data$Hjemtaget == "Ja"),
               "Sex" = .data$SexTmp,
               "Net" = .data$Garnskadet
            )
         if ("Fedtfinne.klippet" %in% colnames(dat1))
            dat2$Cut = dat1$Fedtfinne.klippet
         dat2 <- suppressMessages(type_convert(dat2))

         ### Merge and tidy
         dat3 <-
            dat2 |> mutate(Weight = if_else(.data$Length >= 40, .data$Weight, NA_real_)) |>
            filter(.data$Length >= 40 | is.na(.data$Length))
         # if (!club) {
         #    ## Remove weight outliers
         #    if (species == "Havørred")
         #       res <-
         #          read_csv(str_c(prefix, "_weight_seatrout.csv"),
         #                   show_col_types = FALSE)
         #    if (species == "Laks")
         #       res <-
         #          read_csv(str_c(prefix, "_weight_salmon.csv"),
         #                   show_col_types = FALSE)
         #    res <- res |>
         #       group_by(.data$Length) |>
         #       summarise("Lower" = min(.data$Lower),
         #                 "Upper" = max(.data$Upper))
         #    dat3 <- left_join(dat3, res, by = join_by(.data$Length))
         #    #dat3 |> filter( !((Weight >= 0.8 * Lower & Weight <= 1.2 * Upper) | is.na(Weight) ))
         #    dat3 <- dat3 |>
         #       mutate(
         #          Weight = if_else(
         #             .data$Weight >= 0.8 * .data$Lower &
         #                .data$Weight <= 1.2 * .data$Upper,
         #             .data$Weight,
         #             NA_real_,
         #             NA_real_
         #          )
         #       ) |>
         #       select(-.data$Upper,-.data$Lower)
         # }
         ## Fix custom errors
         dat3 <- dat3 |>
            mutate("Method" = str_replace_all(
               .data$Method,
               c(
                  "Wobler" = "Spin",
                  "Blink" = "Spin",
                  "Spinner" = "Spin",
                  "Jig" = "Spin",
                  "Bombarda med flue" = "Spin",
                  "Tørflue" = "Flue",
                  "Pirk/Pilk" = "Spin",
                  "Mede" = "Orm",
                  "Spinflue" = "Spin",
                  "Spin-flue" = "Spin",
                  "Maddike" = "Orm",
                  "Spin-flue" = "Spin",
                  "Majs" = "Orm",
                  "Flåd" = "Orm",
                  "Orm, spinner" = "Orm",
                  "Orm,spin" = "Orm"
               )
            ))
         if (!club)
            dat3 <- dat3 |>
            mutate(
               Place = case_when(
                  str_detect(Place, "(Øvre.*)|(Skjern.*Rind)|(Skjern.*opstrøms)") ~ "Øvre",
                  str_detect(Place, "(Mellem.*)|(Skjern.*Tarp.*Borris)") ~ "Mellem",
                  str_detect(Place, "(Nedre.*)|(Skjern.*Borris.*Fjord)") ~ "Nedre",
                  str_detect(Place, "Haderup|Haderis") ~ "Haderis Å",
                  str_detect(Place, "Vorgod") ~ "Vorgod Å",
                  str_detect(Place, "Omme") ~ "Omme Å",
                  TRUE ~ Place
               )
            )
         # dat3 <-dat3 |> mutate(Sex = str_replace_all(Sex, c("Han" = "Male", "Hun" = "Female", "Ved ikke" = NA)))
         dat3 <-
            dat3 |> mutate(Sex = str_replace_all(.data$Sex, c("Ved ikke" = NA_character_)))
         dat3 <-
            dat3 |> mutate("Cut" = if_else(.data$Cut == "Ja", TRUE, if_else(.data$Cut == "Nej", FALSE, NA)))
         # unique(dat3$Sex)
         dat3 <-
            dat3 |> mutate(Name = str_to_title(str_replace_all(
               .data$Name,
               c(
                  "Ikke oplyst" = NA,
                  "Mogens Styhr Rasmussen" = "Mogens Styhr",
                  "Ikke Oplyst" = NA,
                  "Poul Godt Godt" = "Poul Godt",
                  "KÅS [0-9 ]* " = "",
                  "Kås [0-9 ]* " = "",
                  ", Vridsted, 2017123" = "",
                  "Xx Yy" = NA
               )
            )))
         dat3 <-
            dat3 |> mutate(Name = str_replace(.data$Name, fixed("**********"), NA)) |> mutate(Name = str_replace(.data$Name, "Xx Yy", NA_character_))
         # unique(dat3$Place)
      }
      ## Save to file
      if (write) {
         res <- tolower(species)
         if (species == "Havørred")
            res <- "seatrout"
         if (species == "Laks")
            res <- "salmon"
         fn <- str_c(prefix, "_catch_", res, "_", yr, ".csv")
         message("  Write data to ", fn)
         write_csv(dat3, fn)
      }

      return(dat3)
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
