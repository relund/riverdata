## Snippets for plotting, tables etc.


#' Plot of catches given current date
#'
#' @param datCatch Catch data.
#' @param .look_back Number of time units to plot.
#' @param .unit Time unit for the look-back window. One of `"cur_year_days"`, `"cur_year_weeks"`, `"cur_year_months"`, `"months"` or `"years"`.
#' @param .group Column name used for grouping and fill in the bar plot. One of `"Place"`, `"Method"`, `"Size"` or `"None"`.
#' @param .plotly Use plotly otherwise just ggplot.
#' @param legend_pos Legend position. One of `"top"`, `"right"`, `"bottom"`, `"left"` or `"none"`.
#' @param up_to_curday Filter multi-year plots to catches up to the current day-of-year.
#'
#' @return The plot object.
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- read_data("data_karup_catch_seatrout_", year = 2023)
#' snip_plot_catch(dat)
#' }
snip_plot_catch <- function(datCatch, .look_back = 30, .unit = c("cur_year_days", "cur_year_weeks", "cur_year_months", "months", "years"), .group = c("Place", "Method", "Size", "None"), .plotly = TRUE, legend_pos = c("top", "right", "bottom", "left", "none"), up_to_curday = FALSE) {
   .unit <- match.arg(.unit)
   .group <- match.arg(.group)
   legend_pos <- match.arg(legend_pos)
   is_multi_year <- .unit %in% c("months", "years")
   is_grouped <- .group != "None"

   if (is_grouped && !.group %in% names(datCatch)) {
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
   dat_source <- datCatch
   if (up_to_curday && is_multi_year) {
      dat_source <- dat_source %>%
         dplyr::filter(yday(.data$Date) <= yday(current_date))
   }
   dat <- dat_source %>%
      dplyr::filter(
         if (is_multi_year) {
            Date > filter_threshold
         } else {
            .data$Year == current_year & Date > filter_threshold
         }
      )
   dat <- if (is_grouped) {
      dat %>%
         dplyr::select(dplyr::all_of(c("Date", .group))) %>%
         dplyr::rename(Place = dplyr::all_of(.group))
   } else {
      dat %>%
         dplyr::select(dplyr::all_of("Date"))
   }
   dat <- dat %>%
      arrange(desc(.data$Date))

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
      dat <- dat_source %>%
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
         )
      dat <- if (is_grouped) {
         dat %>%
            dplyr::select(dplyr::all_of(c("Date", .group))) %>%
            dplyr::rename(Place = dplyr::all_of(.group))
      } else {
         dat %>%
            dplyr::select(dplyr::all_of("Date"))
      }
      dat <- dat %>%
         arrange(desc(.data$Date))

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
         mutate(Date = floor_date(.data$Date, unit = "week", week_start = 1))
   } else if (.unit == "cur_year_months") {
      dat <- dat %>%
         mutate(Date = floor_date(.data$Date, unit = "month"))
   } else if (.unit == "months") {
      month_levels <- month(as.Date("2000-01-01") + months(0:11), label = TRUE, abbr = TRUE)
      month_values <- month(dat$Date, label = TRUE, abbr = TRUE)
      dat <- dat %>%
         mutate(Date = factor(month_values, levels = month_levels[month_levels %in% month_values], ordered = TRUE))
   } else if (.unit == "years") {
      dat <- dat %>%
         mutate(Date = floor_date(.data$Date, unit = "year"))
   }

   if (.unit == "months") {
      if (is_grouped) {
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
            theme(legend.position = legend_pos)
      } else {
         pt <- ggplot(data = dat, aes(x = Date)) +
            geom_bar() +
            geom_text(
               stat = "count",
               aes(y = after_stat(count), label = after_stat(count)),
               vjust = -0.2
            ) +
            labs(fill = "") + xlab("") + ylab("") +
            theme(legend.position = "none")
      }
   } else {
      if (is_grouped) {
         pt <- ggplot(data = dat, aes(x = Date)) +
            geom_bar(aes(fill = .data$Place)) +
            geom_text(
               stat = "count",
               aes(y = after_stat(count), label = after_stat(count)),
               vjust = -0.2,
               position = position_dodge(width = 1)
            ) +
            labs(fill = "") + xlab("") + ylab("") +
            theme(axis.text.x  = element_text(angle=45, hjust = 1, vjust = 1), legend.position = legend_pos)
      } else {
         pt <- ggplot(data = dat, aes(x = Date)) +
            geom_bar() +
            geom_text(
               stat = "count",
               aes(y = after_stat(count), label = after_stat(count)),
               vjust = -0.2
            ) +
            labs(fill = "") + xlab("") + ylab("") +
            theme(axis.text.x  = element_text(angle=45, hjust = 1, vjust = 1), legend.position = "none")
      }
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
      pt <- ggplotly(pt, dynamicTicks = TRUE, tooltip = if (is_grouped) c("count", "fill") else "count")
      plotly_bar_width <- switch(
         .unit,
         cur_year_days = 0.75 * 24 * 60 * 60 * 1000,
         cur_year_weeks = 0.75 * 7 * 24 * 60 * 60 * 1000,
         cur_year_months = 0.75 * 30 * 24 * 60 * 60 * 1000,
         years = 0.75 * 365 * 24 * 60 * 60 * 1000,
         NULL
      )
      plotly_format_date <- function(x) {
         if (.unit == "cur_year_days") {
            return(trimws(format(as.Date(x), "%e. %b %Y")))
         }
         if (.unit == "cur_year_weeks") {
            return(paste0("Uge ", isoweek(as.Date(x)), " (", trimws(format(as.Date(x), "%e. %b %Y")), ")"))
         }
         if (.unit == "cur_year_months") {
            return(format(as.Date(x), "%b %Y"))
         }
         if (.unit == "years") {
            return(format(as.Date(x), "%Y"))
         }
         as.character(x)
      }

      for (i in seq_along(pt$x$data)) {
         trace <- pt$x$data[[i]]
         if (identical(trace$type, "bar")) {
            if (is.null(trace$x) || is.null(trace$y)) next
            if (!is.null(plotly_bar_width)) {
               pt$x$data[[i]]$width <- plotly_bar_width
            }

            if (.unit == "months") {
               if (is_grouped) {
                  pt$x$data[[i]]$hovertemplate <- paste0(
                     "Andel: ", scales::percent(trace$y, accuracy = 1),
                     "<br>Gruppe: ", trace$name,
                     "<extra></extra>"
                  )
               } else {
                  pt$x$data[[i]]$hovertemplate <- paste0(
                     "Antal: ", trace$y,
                     "<extra></extra>"
                  )
               }
            } else {
               if (is_grouped) {
                  pt$x$data[[i]]$hovertemplate <- paste0(
                     "Antal: ", trace$y,
                     "<br>Område: ", trace$name,
                     "<extra></extra>"
                  )
               } else {
                  pt$x$data[[i]]$hovertemplate <- paste0(
                     "Antal: ", trace$y,
                     "<extra></extra>"
                  )
               }
            }
         } else if (identical(trace$type, "scatter") && identical(trace$mode, "text")) {
            if (is.null(trace$x) || is.null(trace$text)) next
            pt$x$data[[i]]$hovertemplate <- paste0(
               "Total: ", trace$text,
               "<br>Date: ", plotly_format_date(trace$x),
               "<extra></extra>"
            )
         }
      }

      plotly_legend <- switch(
         legend_pos,
         top = list(orientation = "h", x = 0, y = 1.08, xanchor = "left", yanchor = "bottom"),
         bottom = list(orientation = "h", x = 0, y = -0.2, xanchor = "left", yanchor = "top"),
         left = list(orientation = "v", x = -0.1, y = 1, xanchor = "right", yanchor = "top"),
         right = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
         none = list()
      )
      plotly_xaxis_range <- pt$x$layout$xaxis$range
      if (length(plotly_xaxis_range) >= 1) {
         plotly_xaxis_range <- c(plotly_xaxis_range[[1]], as.character(as.Date(lastDate)))
      }
      plotly_xaxis <- switch(
         .unit,
         cur_year_days = list(type = "date", autorange = FALSE, range = plotly_xaxis_range, tickmode = "auto", nticks = 8, tickformat = "%e. %b"),
         cur_year_weeks = list(type = "date", autorange = FALSE, range = plotly_xaxis_range, tickmode = "auto", nticks = 8, tickformat = "Uge %V"),
         cur_year_months = list(type = "date", autorange = FALSE, range = plotly_xaxis_range, tickmode = "auto", nticks = 6, tickformat = "%b"),
         years = list(type = "date", autorange = FALSE, range = plotly_xaxis_range, tickmode = "auto", nticks = 8, tickformat = "%Y"),
         list()
      )
      if (length(plotly_xaxis) > 0) {
         pt$x$layout$xaxis$tickvals <- NULL
         pt$x$layout$xaxis$ticktext <- NULL
      }

      pt <- pt %>%
         layout(
            # yaxis = list(title = "Relativ vandstand"),
            xaxis = plotly_xaxis,
            legend = plotly_legend,
            showlegend = is_grouped && legend_pos != "none",
            #hovermode = "x unified",
            dragmode = "orbit"
         ) |>
         config(
            displayModeBar = FALSE,
            displaylogo = FALSE,
            modeBarButtonsToRemove = c("lasso2d", "toImage", "select2d", "hoverClosestCartesian", "hoverCompareCartesian")
         )
      if (length(plotly_xaxis) > 0) {
         pt$x$layout$xaxis$autorange <- FALSE
         pt$x$layout$xaxis$range <- plotly_xaxis_range
      }
   }
   return(pt)
}


#' Catch summary table
#'
#' @param datCatch Catch data.
#' @param .row_unit Row unit for the summary table. One of `"Year"` or `"Month"`.
#' @param type Output table type. One of `"reactable"` or `"kable"`.
#'
#' @return A formatted HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- read_data("data_karup_catch_seatrout_", year = 2023)
#' snip_summary_table(dat)
#' }
snip_summary_table <- function(datCatch, .row_unit = c("Year", "Month"), type = c("reactable", "kable")) {
   .row_unit <- match.arg(.row_unit)
   type <- match.arg(type)
   tooltip_attrs <- 'data-toggle="tooltip" data-placement="top" data-container="body" style="cursor: help;"'

   fa_icon <- function(name, title = "") {
      as.character(htmltools::tags$i(
         class = paste("fa", paste0("fa-", name)),
         title = title,
         `data-toggle` = "tooltip",
         `data-placement` = "top",
         `data-container` = "body",
         style = "cursor: help;"
      ))
   }
   fa_icon_tag <- function(name, title = "") {
      htmltools::tags$i(
         class = paste("fa", paste0("fa-", name)),
         title = title,
         `data-toggle` = "tooltip",
         `data-placement` = "top",
         `data-container` = "body",
         style = "cursor: help;"
      )
   }

   tooltip_span <- function(text, title) {
      paste0("<span title='", title, "' ", tooltip_attrs, ">", text, "</span>")
   }
   tooltip_span_tag <- function(text, title) {
      htmltools::tags$span(
         title = title,
         `data-toggle` = "tooltip",
         `data-placement` = "top",
         `data-container` = "body",
         style = "cursor: help;",
         text
      )
   }
   sigma_icon_tag <- function(title = "") {
      htmltools::tags$i(
         class = "fas",
         title = title,
         `data-toggle` = "tooltip",
         `data-placement` = "top",
         `data-container` = "body",
         style = "cursor: help;",
         "Σ"
      )
   }

   summary_cols <- c("Total", "Sex", "Place", "Method", "Released", "Length", "Weight", "Fulton")
   row_col <- .row_unit
   summary_table_cols <- c(row_col, summary_cols)

   if (.row_unit == "Year") {
      if ("Date" %in% names(datCatch)) {
         is_skjern <- "Place" %in% names(datCatch) &&
            any(unique(datCatch$Place) %in% c("Vorgod Å", "Omme Å"))
         dat_stat <- if (is_skjern) yearly_stat(datCatch) else yearly_stat_karup(datCatch)
      } else if (all(summary_table_cols %in% names(datCatch))) {
         dat_stat <- datCatch
         is_skjern <- "PlaceK" %in% names(dat_stat)
      } else {
         stop(
            "`datCatch` must contain raw catch data with a `Date` column or a yearly summary table with columns ",
            paste(summary_table_cols, collapse = ", "),
            ".",
            call. = FALSE
         )
      }
   } else {
      if ("Date" %in% names(datCatch)) {
         is_skjern <- "Place" %in% names(datCatch) &&
            any(unique(datCatch$Place) %in% c("Vorgod Å", "Omme Å"))
         table_year <- lubridate::year(max(datCatch$Date, na.rm = TRUE))
         dat_stat <- if (is_skjern) monthly_stat(datCatch, table_year) else monthly_stat_karup(datCatch, table_year)
      } else if (all(summary_table_cols %in% names(datCatch))) {
         dat_stat <- datCatch
         is_skjern <- "PlaceK" %in% names(dat_stat)
      } else {
         stop(
            "`datCatch` must contain raw catch data with a `Date` column or a monthly summary table with columns ",
            paste(summary_table_cols, collapse = ", "),
            ".",
            call. = FALSE
         )
      }
   }

   c_names <- c(
      paste0(fa_icon("calendar-alt", title = if (.row_unit == "Year") "År" else "Måned")),
      paste0("<i class=\"fas\" title=\"Total antal\" ", tooltip_attrs, ">&#x3A3;</i>"),
      paste0(
         fa_icon("mars", title = "Han"), "/",
         fa_icon("venus", title = "Hun"), "/",
         fa_icon("question", title = "Ukendt"), " (%)"
      ),
      if (is_skjern) {
         paste0(
            tooltip_span("N", "Nedre"), "/",
            tooltip_span("M", "Mellem"), "/",
            tooltip_span("Ø", "Øvre"), "/",
            tooltip_span("V", "Vorgod Å"), "/",
            tooltip_span("O", "Omme Å"), "/",
            tooltip_span("U", "Ukendt"), " (%)"
         )
      } else {
         paste0(
            tooltip_span("N", "Nedre"), "/",
            tooltip_span("M", "Mellem"), "/",
            tooltip_span("Ø", "Øvre"), "/",
            tooltip_span("H", "Haderis Å"), "/",
            tooltip_span("U", "Ukendt"), " (%)"
         )
      },
      if (is_skjern) {
         paste0(
            tooltip_span("N", "Nedre"), "/",
            tooltip_span("M", "Mellem"), "/",
            tooltip_span("Ø", "Øvre"), "/",
            tooltip_span("V", "Vorgod Å"), "/",
            tooltip_span("O", "Omme Å"), "/",
            tooltip_span("U", "Ukendt"), " (%)"
         )
      },
      paste0(
         tooltip_span("F", "Flue"), "/",
         tooltip_span("S", "Spin"), "/",
         tooltip_span("O", "Orm"), "/",
         tooltip_span("U", "Ukendt"), " (%)"
      ),
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
         "/<i class=\"fas\" title=\"Total vægt\" ", tooltip_attrs, ">&#x3A3;</i>"
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

   if (.row_unit == "Year") {
      dat_stat <- dplyr::arrange(dat_stat, desc(.data$Year))
   }

   if (type == "kable") {
      return(
         dat_stat %>%
            knitr::kable(col.names = c_names, escape = FALSE, align = "c", format = "html") %>%
            kableExtra::kable_styling(
               fixed_thead = TRUE,
               font_size = 10,
               bootstrap_options = c("striped", "hover", "condensed", "responsive")
            ) %>%
            kableExtra::add_header_above(header_names)
      )
   }

   column_names <- names(dat_stat)
   cell_labels <- c(
      if (.row_unit == "Year") "År" else "Måned",
      "Total",
      "Køn",
      if (is_skjern) c("Område", "Område hjemtaget") else "Område",
      "Metode",
      "Genudsat",
      "Længde",
      "Vægt",
      "Kondition"
   )
   names(cell_labels) <- column_names
   skjern_place_header <- htmltools::tagList(
      tooltip_span_tag("N", "Nedre"), "/",
      tooltip_span_tag("M", "Mellem"), "/",
      tooltip_span_tag("Ø", "Øvre"), "/",
      tooltip_span_tag("V", "Vorgod Å"), "/",
      tooltip_span_tag("O", "Omme Å"), "/",
      tooltip_span_tag("U", "Ukendt"), " (%)"
   )
   karup_place_header <- htmltools::tagList(
      tooltip_span_tag("N", "Nedre"), "/",
      tooltip_span_tag("M", "Mellem"), "/",
      tooltip_span_tag("Ø", "Øvre"), "/",
      tooltip_span_tag("H", "Haderis Å"), "/",
      tooltip_span_tag("U", "Ukendt"), " (%)"
   )
   common_headers <- list(
      htmltools::tagList(
         tooltip_span_tag("F", "Flue"), "/",
         tooltip_span_tag("S", "Spin"), "/",
         tooltip_span_tag("O", "Orm"), "/",
         tooltip_span_tag("U", "Ukendt"), " (%)"
      ),
      htmltools::tagList(
         fa_icon_tag("sync", title = "C&R"), "/",
         fa_icon_tag("times", title = "Hjemtaget"), " (%)"
      ),
      htmltools::tagList(
         fa_icon_tag("ruler-horizontal", title = "Gens længde"), "/",
         fa_icon_tag("ruler", title = "Max længde")
      ),
      htmltools::tagList(
         fa_icon_tag("balance-scale", title = "Gens. vægt"), "/",
         fa_icon_tag("weight-hanging", title = "Max vægt"), "/",
         sigma_icon_tag("Total vægt")
      ),
      htmltools::tagList(
         fa_icon_tag("heart", title = "Gens kondition"), "/",
         fa_icon_tag("gratipay", title = "Max kondition")
      )
   )
   mobile_headers <- c(
      list(
         fa_icon_tag("calendar-alt", title = if (.row_unit == "Year") "År" else "Måned"),
         sigma_icon_tag("Total antal"),
         htmltools::tagList(
            fa_icon_tag("mars", title = "Han"), "/",
            fa_icon_tag("venus", title = "Hun"), "/",
            fa_icon_tag("question", title = "Ukendt"), " (%)"
         ),
         if (is_skjern) skjern_place_header else karup_place_header
      ),
      if (is_skjern) list(skjern_place_header),
      common_headers
   )
   names(mobile_headers) <- column_names
   table_css <- htmltools::tags$style(htmltools::HTML("
.summary-table-container,
.summary-table-container .reactable,
.summary-table-container .Reactable,
.summary-table-container .rt-table {
  height: 100% !important;
}
.summary-table-container .rt-tbody {
  overflow-y: auto;
}
.summary-table .summary-table-label {
  display: none;
}
@media (max-width: 767px) {
  .summary-table-container,
  .summary-table-container .reactable,
  .summary-table-container .Reactable,
  .summary-table-container .rt-table,
  .summary-table-container .rt-tbody {
    height: 100% !important;
  }
  .summary-table-container {
    height: calc(100dvh - 24px) !important;
    max-width: 100% !important;
    overflow-x: hidden !important;
  }
  .summary-table-container .reactable,
  .summary-table-container .Reactable,
  .summary-table {
    max-width: 100% !important;
    overflow-x: hidden !important;
  }
  .summary-table .rt-thead {
    display: none;
  }
  .summary-table .rt-table,
  .summary-table .rt-tbody {
    display: block;
    width: 100% !important;
    max-width: 100% !important;
    min-width: 0 !important;
    overflow-x: hidden !important;
  }
  .summary-table .rt-tr-group {
    display: block;
    width: 100% !important;
    max-width: 100% !important;
    min-width: 0 !important;
    box-sizing: border-box;
    margin: 0 0 8px;
    border: 1px solid #ddd;
    border-radius: 4px;
    background: #fff;
  }
  .summary-table .rt-tr {
    display: block;
    width: 100% !important;
    max-width: 100% !important;
    min-width: 0 !important;
  }
  .summary-table .rt-td {
    display: flex !important;
    justify-content: flex-start;
    align-items: baseline;
    text-align: left !important;
    width: 100% !important;
    max-width: 100% !important;
    min-width: 0 !important;
    box-sizing: border-box;
    padding: 3px 6px !important;
    border-right: 0 !important;
    white-space: normal !important;
  }
  .summary-table .rt-td > .rt-td-inner,
  .summary-table .summary-table-cell {
    display: flex;
    justify-content: flex-start;
    text-align: left !important;
    width: 100%;
    max-width: 100%;
    min-width: 0;
    white-space: normal !important;
  }
  .summary-table .summary-table-label {
    display: inline;
    flex: 0 1 auto;
    margin-right: 4px;
    font-weight: 600;
    min-width: 0;
    overflow-wrap: anywhere;
  }
  .summary-table .summary-table-value {
    min-width: 0;
    overflow-wrap: anywhere;
  }
}
"))
   columns <- stats::setNames(
      lapply(column_names, function(column_name) {
         column_index <- match(column_name, column_names)
         reactable::colDef(
            name = htmltools::HTML(c_names[[column_index]]),
            align = "center",
            html = TRUE,
            cell = function(value) {
               htmltools::tags$span(
                  class = "summary-table-cell",
                  htmltools::tags$span(
                     class = "summary-table-label",
                     cell_labels[[column_name]],
                     " - ",
                     mobile_headers[[column_name]],
                     ":"
                  ),
                  htmltools::tags$span(class = "summary-table-value", as.character(value))
               )
            }
         )
      }),
      column_names
   )
   column_groups <- if (is_skjern) {
      list(
         reactable::colGroup(name = " ", columns = column_names[1:2]),
         reactable::colGroup(name = "Køn", columns = "Sex"),
         reactable::colGroup(name = "Område (alle fangster/hjemtaget)", columns = c("Place", "PlaceK")),
         reactable::colGroup(name = "Metode", columns = "Method"),
         reactable::colGroup(name = "Genudsat", columns = "Released"),
         reactable::colGroup(name = "Længde", columns = "Length"),
         reactable::colGroup(name = "Vægt", columns = "Weight"),
         reactable::colGroup(name = "Kondition", columns = "Fulton")
      )
   } else {
      list(
         reactable::colGroup(name = " ", columns = column_names[1:2]),
         reactable::colGroup(name = "Køn", columns = "Sex"),
         reactable::colGroup(name = "Område", columns = "Place"),
         reactable::colGroup(name = "Metode", columns = "Method"),
         reactable::colGroup(name = "Genudsat", columns = "Released"),
         reactable::colGroup(name = "Længde", columns = "Length"),
         reactable::colGroup(name = "Vægt", columns = "Weight"),
         reactable::colGroup(name = "Kondition", columns = "Fulton")
      )
   }

   htmltools::tagList(
      table_css,
      htmltools::tags$div(
         class = "summary-table-container",
         reactable::reactable(
            dat_stat,
            class = "summary-table",
            columns = columns,
            columnGroups = column_groups,
            pagination = FALSE,
            striped = TRUE,
            highlight = TRUE,
            compact = TRUE,
            fullWidth = TRUE,
            height = "100%",
            wrap = FALSE,
            defaultColDef = reactable::colDef(
               align = "center",
               minWidth = 70
            )
         )
      )
   )
}

#' Catch records table
#'
#' @param datCatch Catch data.
#' @param years Years to show.
#' @param page_length Number of rows shown on each page.
#'
#' @return A formatted DT table.
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- read_data("data_karup_catch_seatrout_", year = 2023)
#' snip_catch_table(dat, years = c(2023, 2022))
#' }
snip_catch_table <- function(datCatch, years, page_length = 25) {
   DT::datatable(
      datCatch %>%
         dplyr::select(Date:Weight, Place, Method, Fulton, Misc, NoWeight) %>%
         dplyr::filter(lubridate::year(.data$Date) %in% years),
      class = "stripe row-border order-column nowrap compact",
      rownames = FALSE,
      filter = "bottom",
      escape = FALSE,
      options = list(
         pageLength = page_length,
         order = list(list(0, "desc")),
         list(visible = FALSE, targets = "NoWeight"),
         columnDefs = list(
            list(className = "dt-center", targets = 0:6),
            list(visible = FALSE, targets = c(7))
         )
      ),
      colnames = c("Dato", "Længde", "Vægt", "Sted", "Metode", "Kondition", "Diverse", "NoWeight"),
      extensions = c("Responsive")
   ) %>%
      DT::formatStyle(
         columns = "Weight",
         valueColumns = "NoWeight",
         color = DT::styleEqual(c(0, 1), c("black", "green"))
      ) %>%
      DT::formatRound(c("Length"), digits = 0) %>%
      DT::formatRound(c("Weight"), digits = 1) %>%
      DT::formatRound(c("Fulton"), digits = 2)
}

#' Build an interactive river map
#'
#' @param prefix File prefix for the map marker and line CSV files.
#' @param show_groups Layer groups shown when the map opens.
#'
#' @return A Leaflet map.
#' @examples
#' \dontrun{
#' snip_leaflet("data/data_karup_", show_groups = "Stednavne")
#' }
snip_leaflet <- function(prefix, show_groups = "Stednavne") {
   datMarkers <- read_csv(paste0(prefix, "mapmarkers.csv"), col_types = "fccddff") %>%
      mutate(Desc = str_c(if_else(!is.na(.data$Desc), str_c("<b>", if_else(is.na(.data$Club), "", str_c(.data$Club, " - ")), .data$Desc, "</b>"), "", ""),
                          if_else(!is.na(.data$Text), str_c("<br/><br/>", .data$Text), "", ""))) %>%
      mutate(Desc = str_replace(.data$Desc, "^(.*?)(http.*)([\\s$]*.*)", "\\1<a href='\\2'>\\2</a>\\3")) %>%
      mutate(Desc = map(.data$Desc, HTML), Icon = str_c("www/", .data$Icon), Id = seq_len(n())) %>%
      select(-all_of("Text"))
   
   datLines <- read_csv(paste0(prefix, "maplines.csv"), col_types = "fccddif")  %>%
      mutate(Desc = str_c("<b>", if_else(is.na(.data$Club), "", str_c(.data$Club, " - ")), if_else(!is.na(.data$Desc), .data$Desc, "", ""), "</b>",
                          if_else(!is.na(.data$Text), str_c("<br/><br/>", .data$Text), "", ""))) %>%
      mutate(Desc = str_replace(.data$Desc, "^(.*?)(http.*)([\\s$]*.*)", "\\1<a href='\\2'>\\2</a>\\3")) %>%
      mutate(Desc = map(.data$Desc, HTML)) %>%
      select(-all_of("Text"))
   
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
               str_detect(.data$Icon, regex(search_term, ignore_case = T)))
      } 
      if (group %in% c("Info")) {
         datIds <- datMarkers %>%
            filter(
               str_detect(.data$Group, regex(search_term, ignore_case = T)) |
                  str_detect(.data$Desc, regex(search_term, ignore_case = T)))
      }
      if (group %in% c("Shelter", "Stednavne", "Bro/spang")) {
         tmpIds <- datIds %>% filter(!is.na(.data$Club)) %>% pull(.data$Id)
         datIds <- datIds %>% filter(is.na(.data$Club))
         datMarkers <- datMarkers %>% filter(!(.data$Id %in% tmpIds))
      }
      Ids <- datIds %>% pull(.data$Id)
      # cat("Adding group ", group, " with ", length(Ids), " markers\n")
      lst <- map_add_markers(maplet, group, datMarkers %>% filter(.data$Id %in% Ids))
      maplet <- lst$map
      grp <- unique(c(grp, lst$groups))
      datMarkers <- datMarkers %>% filter(!(.data$Id %in% Ids))
      # print(datMarkers)
      # print(maplet)
   }
   
   
   # Lines
   groups <- c("Parkering", "fiskevand", "medlem", "dagkort", "gæstekort")
   colors <- c("#eb9834", "#3C8AE6", "#EBE053", "#000000", "#bf5656")
   for (i in 1:length(groups)) {
      group <- groups[i]
      color <- colors[i]
      datLinesGroup <- datLines %>% filter(str_detect(.data$Group, regex(group, ignore_case = T)))
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
