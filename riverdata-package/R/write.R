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
               "Sex" = .data$`Køn...17`,
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
               "Sex" = .data$`Køn`,
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
