## Snippets for plotting, tables etc.


#' Plot of catches given current date
#'
#' @param datCatch Catch data.
#' @param .look_back Number of time units to plot.
#' @param .unit Time unit for the look-back window. One of `"cur_year_days"`, `"cur_year_weeks"`, `"cur_year_months"`, `"months"` or `"years"`.
#' @param .group Column name used for grouping and fill in the bar plot. One of `"Place"` or `"Method"`.
#' @param .plotly Use plotly otherwise just ggplot.
#'
#' @return The plot object.
#' @export
#'
#' @examples
#' dat <- read_data("data_karup_catch_seatrout_", year = 2023)
#' snip_plot_catch(dat)
snip_plot_catch <- function(datCatch, .look_back = 30, .unit = c("cur_year_days", "cur_year_weeks", "cur_year_months", "months", "years"), .group = c("Place", "Method"), .plotly = TRUE) {
   .unit <- match.arg(.unit)
   .group <- match.arg(.group)
   is_multi_year <- .unit %in% c("months", "years")

   if (!.group %in% names(datCatch)) {
      stop("`.group` must be a column in `datCatch`.", call. = FALSE)
   }

   current_date <- today(tzone = "CET")
   current_year <- max(datCatch$Year, na.rm = TRUE)
   if (current_year < year(current_date)) {
      current_date <- max(datCatch$Date, na.rm = TRUE) + days(1)
   }
   filter_threshold <- if (.unit == "cur_year_weeks") {
      current_date - weeks(.look_back)
   } else if (.unit == "cur_year_months") {
      current_date - months(.look_back)
   } else if (is_multi_year) {
      current_date - years(.look_back)
   } else {
      current_date - days(.look_back)
   }
   dat <- datCatch %>%
      dplyr::filter(
         if (is_multi_year) {
            Date > filter_threshold
         } else {
            .data$Year == current_year & Date > filter_threshold
         }
      ) %>%
      dplyr::select(Date, dplyr::all_of(.group)) %>%
      dplyr::rename(Place = dplyr::all_of(.group)) %>%
      arrange(desc(Date))

   if (.unit == "cur_year_weeks") {
      lastDate <- floor_date(current_date, unit = "week", week_start = 1) + weeks(1)
   } else if (.unit == "cur_year_months") {
      lastDate <- floor_date(current_date, unit = "month") + months(1)
   } else if (.unit == "years") {
      lastDate <- floor_date(current_date, unit = "year") + years(1)
   } else {
      lastDate <- current_date + days(1)
   }

   if (nrow(dat) == 0) {  # find last days with catches
      dat <- datCatch %>%
         dplyr::filter(
            if (is_multi_year) {
               Date > max(Date) - years(.look_back)
            } else {
               .data$Year == current_year &
                  Date > max(Date) - if (.unit == "cur_year_weeks") {
                     weeks(.look_back)
                  } else if (.unit == "cur_year_months") {
                     months(.look_back)
                  } else {
                     days(.look_back)
                  }
            }
         ) %>%
         dplyr::select(Date, dplyr::all_of(.group)) %>%
         dplyr::rename(Place = dplyr::all_of(.group)) %>%
         arrange(desc(Date))

      if (.unit == "cur_year_weeks") {
         lastDate <- floor_date(max(dat$Date), unit = "week", week_start = 1) + weeks(1)
      } else if (.unit == "cur_year_months") {
         lastDate <- floor_date(max(dat$Date), unit = "month") + months(1)
      } else if (.unit == "years") {
         lastDate <- floor_date(max(dat$Date), unit = "year") + years(1)
      } else {
         lastDate <- max(dat$Date) + days(1)
      }
   }

   if (.unit == "cur_year_weeks") {
      dat <- dat %>%
         mutate(Date = floor_date(Date, unit = "week", week_start = 1))
   } else if (.unit == "cur_year_months") {
      dat <- dat %>%
         mutate(Date = floor_date(Date, unit = "month"))
   } else if (.unit == "months") {
      month_levels <- month(as.Date("2000-01-01") + months(0:11), label = TRUE, abbr = TRUE)
      month_values <- month(dat$Date, label = TRUE, abbr = TRUE)
      dat <- dat %>%
         mutate(Date = factor(month_values, levels = month_levels[month_levels %in% month_values], ordered = TRUE))
   } else if (.unit == "years") {
      dat <- dat %>%
         mutate(Date = floor_date(Date, unit = "year"))
   }

   if (.unit == "months") {
      pt <- ggplot(data = dat, aes(x = Date)) +
         geom_bar(aes(fill = .data$Place, y = after_stat(count) / sum(after_stat(count)))) +
         geom_text(
            aes(
               y = after_stat(count) / sum(after_stat(count)),
               label = scales::percent(round(after_stat(count) / sum(after_stat(count)), 2))
            ),
            stat = "count",
            vjust = -0.5
         ) +
         labs(fill = "") + xlab("") + ylab("") +
         scale_y_continuous(labels = scales::percent) +
         theme(legend.position = "bottom")
   } else {
      pt <- ggplot(data = dat, aes(x = Date)) +
         geom_bar(aes(fill = .data$Place)) +
         geom_text(
            stat = "count",
            aes(y = after_stat(count), label = after_stat(count)),
            vjust = -0.2,
            position = position_dodge(width = 1)
         ) +
         labs(fill = "") + xlab("") + ylab("") +
         theme(axis.text.x  = element_text(angle=45, hjust = 1, vjust = 1), legend.position="bottom")
   }

   if (.unit == "cur_year_weeks") {
      week_breaks <- seq(min(dat$Date, na.rm = TRUE), lastDate, by = "1 week")
      pt <- pt +
         scale_x_date(
            breaks = week_breaks,
            labels = function(x) {
               labels <- paste("Uge", isoweek(x))
               labels[x == max(x)] <- ""
               labels
            },
            limits = c(NA_Date_, lastDate)
         )
   } else if (.unit == "cur_year_months") {
      month_breaks <- seq(min(dat$Date, na.rm = TRUE), lastDate, by = "1 month")
      pt <- pt +
         scale_x_date(
            breaks = month_breaks,
            labels = function(x) {
               labels <- format(x, "%b")
               labels[x == max(x)] <- ""
               labels
            },
            limits = c(NA_Date_, lastDate)
         )
   } else if (.unit == "years") {
      year_breaks <- seq(min(dat$Date, na.rm = TRUE), lastDate, by = "1 year")
      pt <- pt +
         scale_x_date(
            breaks = year_breaks,
            labels = function(x) {
               labels <- format(x, "%Y")
               labels[x == max(x)] <- ""
               labels
            },
            limits = c(NA_Date_, lastDate)
         )
   } else if (.unit == "months") {
      pt <- pt +
         scale_x_discrete()
   } else {
      day_breaks <- seq(min(dat$Date, na.rm = TRUE), lastDate, by = "1 day")
      pt <- pt +
         scale_x_date(
            breaks = day_breaks,
            labels = function(x) {
               labels <- format(x, "%e. %b")
               labels[x == max(x)] <- ""
               labels
            },
            limits = c(NA_Date_, lastDate)
         )
   }

   if (.plotly) {
      pt <- ggplotly(pt, tooltip = c("count", "fill"))

      for (i in seq_along(pt$x$data)) {
         trace <- pt$x$data[[i]]
         if (!identical(trace$type, "bar")) next
         if (is.null(trace$x) || is.null(trace$y) || is.null(trace$name)) next

         if (.unit == "months") {
            pt$x$data[[i]]$hovertext <- paste0(
               "Andel: ", scales::percent(trace$y, accuracy = 1),
               "<br>Gruppe: ", trace$name
            )
         } else {
            pt$x$data[[i]]$hovertext <- paste0(
               "Antal: ", trace$y,
               "<br>Område: ", trace$name
            )
         }
         pt$x$data[[i]]$hovertemplate <- "%{hovertext}<extra></extra>"
      }

      pt <- pt %>%
         layout(
            # yaxis = list(title = "Relativ vandstand"),
            legend = list(orientation = 'h'),
            #hovermode = "x unified",
            dragmode = "orbit"
         ) |>
         config(
            displayModeBar = FALSE,
            displaylogo = FALSE,
            modeBarButtonsToRemove = c("lasso2d", "toImage", "select2d", "hoverClosestCartesian", "hoverCompareCartesian")
         )
   }
   return(pt)
}


#' Catch summary table
#'
#' @param datCatch Catch data.
#' @param .row_unit Row unit for the summary table.
#'
#' @return A formatted HTML table.
#' @export
#'
#' @examples
#' dat <- read_data("data_karup_catch_seatrout_", year = 2023)
#' snip_table(dat)
snip_table <- function(datCatch, .row_unit = c("Year")) {
   .row_unit <- match.arg(.row_unit)

   fa_icon <- function(name, title = "") {
      as.character(htmltools::tags$i(class = paste("fa", paste0("fa-", name)), title = title))
   }

   if (.row_unit == "Year") {
      yearly_summary_cols <- c("Year", "Total", "Sex", "Place", "Method", "Released", "Length", "Weight", "Fulton")

      if ("Date" %in% names(datCatch)) {
         is_skjern <- "Place" %in% names(datCatch) &&
            any(unique(datCatch$Place) %in% c("Vorgod Å", "Omme Å"))
         dat_stat <- if (is_skjern) yearly_stat(datCatch) else yearly_stat_karup(datCatch)
      } else if (all(yearly_summary_cols %in% names(datCatch))) {
         dat_stat <- datCatch
         is_skjern <- "PlaceK" %in% names(dat_stat)
      } else {
         stop(
            "`datCatch` must contain raw catch data with a `Date` column or a yearly summary table with columns ",
            paste(yearly_summary_cols, collapse = ", "),
            ".",
            call. = FALSE
         )
      }

      c_names <- c(
         paste0(fa_icon("calendar-alt", title = "År")),
         "<i class=\"fas\" title=\"Total antal\">&#x3A3;</i>",
         paste0(
            fa_icon("mars", title = "Han"), "/",
            fa_icon("venus", title = "Hun"), "/",
            fa_icon("question", title = "Ukendt"), " (%)"
         ),
         if (is_skjern) {
            "<span title='Nedre'>N</span>/<span title='Mellem'>M</span>/<span title='Øvre'>Ø</span>/<span title='Vorgod Å'>V</span>/<span title='Omme Å'>O</span>/<span title='Ukendt'>U</span> (%)"
         } else {
            "<span title='Nedre'>N</span>/<span title='Mellem'>M</span>/<span title='Øvre'>Ø</span>/<span title='Haderis Å'>H</span>/<span title='Ukendt'>U</span> (%)"
         },
         if (is_skjern) {
            "<span title='Nedre'>N</span>/<span title='Mellem'>M</span>/<span title='Øvre'>Ø</span>/<span title='Vorgod Å'>V</span>/<span title='Omme Å'>O</span>/<span title='Ukendt'>U</span> (%)"
         },
         "<span title='Flue'>F</span>/<span title='Spin'>S</span>/<span title='Orm'>O</span>/<span title='Ukendt'>U</span> (%)",
         paste0(
            fa_icon("sync", title = "C&R"), "/",
            fa_icon("times", title = "Hjemtaget"), " (%)"
         ),
         paste0(
            fa_icon("ruler-horizontal", title = "Gens længde"), "/",
            fa_icon("ruler", title = "Max længde")
         ),
         paste0(
            fa_icon("balance-scale", title = "Gens. vægt"), "/",
            fa_icon("weight-hanging", title = "Max vægt"),
            "/<i class=\"fas\" title=\"Total vægt\">&#x3A3;</i>"
         ),
         paste0(
            fa_icon("heart", title = "Gens kondition"), "/",
            fa_icon("gratipay", title = "Max kondition")
         )
      )

      header_names <- if (is_skjern) {
         c(" " = 2, "Køn", "Område (alle fangster/hjemtaget)" = 2, "Metode", "Genudsat", "Længde", "Vægt", "Kondition")
      } else {
         c(" " = 2, "Køn", "Område", "Metode", "Genudsat", "Længde", "Vægt", "Kondition")
      }

      dat_stat %>%
         arrange(desc(Year)) %>%
         knitr::kable(col.names = c_names, escape = FALSE, align = "c", format = "html") %>%
         kableExtra::kable_styling(
            fixed_thead = TRUE,
            font_size = 10,
            bootstrap_options = c("striped", "hover", "condensed", "responsive")
         ) %>%
         kableExtra::add_header_above(header_names)
   }
}



snip_leaflet <- function(prefix, show_groups = "Stednavne") {
   datMarkers <- read_csv(paste0(prefix, "mapmarkers.csv"), col_types = "fccddff") %>% 
      mutate(Desc = str_c(if_else(!is.na(Desc), str_c("<b>", if_else(is.na(Club), "", str_c(Club, " - ")), Desc, "</b>"), "", ""), 
                          if_else(!is.na(Text), str_c("<br/><br/>", Text), "", ""))) %>% 
      mutate(Desc = str_replace(Desc, "^(.*?)(http.*)([\\s$]*.*)", "\\1<a href='\\2'>\\2</a>\\3")) %>% 
      mutate(Desc = map(Desc, HTML), Icon = str_c("www/", Icon), Id = 1:n()) %>% 
      select(-Text) 
   
   datLines <- read_csv(paste0(prefix, "maplines.csv"), col_types = "fccddif")  %>%  
      mutate(Desc = str_c("<b>", if_else(is.na(Club), "", str_c(Club, " - ")), if_else(!is.na(Desc), Desc, "", ""), "</b>",
                          if_else(!is.na(Text), str_c("<br/><br/>", Text), "", ""))) %>% 
      mutate(Desc = str_replace(Desc, "^(.*?)(http.*)([\\s$]*.*)", "\\1<a href='\\2'>\\2</a>\\3")) %>% 
      mutate(Desc = map(Desc, HTML)) %>% 
      select(-Text) 
   
   # init map
   maplet <- leaflet(width = "100%", height = "100vh") %>%
      # Base groups
      addTiles(group = "Kort", options = providerTileOptions(maxZoom = 19)) %>%
      # addProviderTiles('MtbMap', group = "Kort") %>%
      addProviderTiles('Esri.WorldImagery', group = "Luftfoto", options = providerTileOptions(maxZoom = 19)) %>%  
      addProviderTiles("CartoDB.PositronOnlyLabels", group = "Luftfoto", options = providerTileOptions(maxZoom = 19))
   # setView(9.016712672451805, 56.40970340006212,  zoom = 13)
   
   
   grp <- NULL
   groups <- c("Parkering", "Shelter", "Stednavne", "Bro/spang", "Info")
   search_terms <- c("park", "shelter", "rock|fish", "bridge", "info|indhegning")
   for (i in 1:length(groups)) {
      group = groups[i]
      search_term = search_terms[i]
      if (group %in% c("Parkering", "Shelter", "Stednavne", "Bro/spang")) {
         datIds <- datMarkers %>%
            filter(
               str_detect(Icon, regex(search_term, ignore_case = T))) 
      } 
      if (group %in% c("Info")) {
         datIds <- datMarkers %>%
            filter(
               str_detect(Group, regex(search_term, ignore_case = T)) |
                  str_detect(Desc, regex(search_term, ignore_case = T)))
      }
      if (group %in% c("Shelter", "Stednavne", "Bro/spang")) {
         tmpIds <- datIds %>% filter(!is.na(Club)) %>% pull(Id)
         datIds <- datIds %>% filter(is.na(Club))
         datMarkers <- datMarkers %>% filter(!(Id %in% tmpIds))
      }
      Ids <- datIds %>% pull(Id)
      # cat("Adding group ", group, " with ", length(Ids), " markers\n")
      lst <- map_add_markers(maplet, group, datMarkers %>% filter(Id %in% Ids))
      maplet <- lst$map
      grp <- unique(c(grp, lst$groups))
      datMarkers <- datMarkers %>% filter(!(Id %in% Ids))
      # print(datMarkers)
      # print(maplet)
   }
   
   
   # Lines
   groups <- c("Parkering", "fiskevand", "medlem", "dagkort", "gæstekort")
   colors <- c("#eb9834", "#3C8AE6", "#EBE053", "#000000", "#bf5656")
   for (i in 1:length(groups)) {
      group <- groups[i]
      color <- colors[i]
      datLinesGroup <- datLines %>% filter(str_detect(Group, regex(group, ignore_case = T)))
      useClub = TRUE
      if (group %in% c("Parkering")) {
         useClub = FALSE
      }
      lst <- map_add_lines(maplet, group, datLinesGroup, color, useClub)
      maplet <- lst$map
      grp <- unique(c(grp, lst$groups))
   }
   
   show_groups <- show_groups[show_groups %in% grp]
   maplet <- maplet %>%
      # Layer control
      addLayersControl(
         baseGroups = c("Luftfoto", "Kort"),
         overlayGroups = unique(grp),
         options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(grp) %>%
      showGroup(show_groups) %>%
      addFullscreenControl()
   
   return(maplet)
}
