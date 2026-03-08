## Snippets for plotting, tables etc.


#' Plot of catches given current date
#'
#' @param datCatch Catch data.
#' @param .days Number of days to plot.
#' @param .plotly Use plotly otherwise just ggplot.
#'
#' @return The plot object.
#' @export
#'
#' @examples
#' dat <- read_data("data_karup_catch_seatrout_", year = 2023)
#' snip_plot_catch(dat)
snip_plot_catch <- function(datCatch, .days = 30, .plotly = TRUE) {
   dat <- datCatch %>%
      dplyr::filter(Date > now(tzone = "CET") - days(.days)) %>%
      select(Date, .data$Place) %>%
      arrange(desc(Date))
   lastDate <- date(now() + ddays(1))
   if (nrow(dat) == 0) {  # find last days with catches
      dat <- datCatch %>%
         mutate(DayCtr = max(Date) - Date) %>%
         dplyr::filter(.data$DayCtr <= .days) %>%
         arrange(desc(Date))
      lastDate <- max(dat$Date) + ddays(1)
   }
   pt <- ggplot(data = dat, aes(x = Date)) +
      geom_bar(aes(fill = .data$Place), width = 0.75) +
      geom_text(aes(label = after_stat(count), y= after_stat(count) ), stat= "count", position = position_dodge(width = 1)) + #  vjust = -0.5
      scale_x_date(date_labels = "%e. %b", date_breaks = "1 day",
                   limits = c(NA_Date_, lastDate)) + # date(now()+ddays(1))
      labs(fill = "") + xlab("") + ylab("") +
      theme(axis.text.x  = element_text(angle=45, hjust = 1, vjust = 1), legend.position="bottom")
   if (.plotly) pt <- ggplotly(pt, tooltip = NULL) %>%  # , dynamicTicks = TRUE
      layout(
         xaxis = list(domain = c(0, 1), title = NA, tickformat = "%d %b"),
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
   return(pt)
}
