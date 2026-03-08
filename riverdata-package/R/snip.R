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
