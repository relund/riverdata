# Functions for preparing data for map visualizations

map_add_markers <- function(map, group, data) {
  if (nrow(data) == 0) return(list(map=map, groups=NULL))
  data <-  data %>% mutate(Pop = Desc)
  if (group %in% c("Parkering", "Shelter")) {
    data <- data %>%
      mutate(
        MapsUrl = str_c(
          "https://www.google.com/maps/dir/?api=1&destination=",
          lat, ",", long
        ),
        MapsUrlEnc = map_chr(MapsUrl, ~ URLencode(.x, reserved = TRUE)),
        Pop = pmap(
          list(Desc, MapsUrl, MapsUrlEnc),
          function(desc, maps_url, maps_url_enc) {
            HTML(
              str_c(
                "<div style='max-width:180px;'>",
                as.character(desc),
                "<br/><br/>",
                "<a href='", maps_url, "' target='_blank' rel='noopener noreferrer'>Rute i Google Maps</a>",
                "<br/><div style='text-align:center; margin-top:8px;'>",
                "<img src='https://api.qrserver.com/v1/create-qr-code/?size=180x180&data=", maps_url_enc,
                "' alt='QR kode til rute' style='max-width:180px; width:100%; height:auto;'/>",
                "</div></div>"
              )
            )
          }
        )
      ) %>%
      select(-MapsUrl, -MapsUrlEnc)
  }
  
  map <- map %>%
    addMarkers(
      ~long, ~lat,
      label = ~Desc,
      popup = ~Pop,
      popupOptions = popupOptions(
        maxWidth = 180,
        minWidth = 180,
        autoPan = TRUE
      ),
      # labelOptions = labelOptions(
      #   style = list(
      #     "max-width" = "200px",
      #     "width" = "200px",
      #     "white-space" = "normal",
      #     "word-break" = "break-word",
      #     "overflow-wrap" = "break-word",
      #     "font-size" = "13px",
      #     "line-height" = "1.3"
      #   ),
      #   direction = "top"
      # ),
      icon = ~icons(
        Icon,
        iconWidth = 32,
        iconHeight = 32,
        iconAnchorX = 16,
        iconAnchorY = 16
      ),
      group = group,
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE,
        freezeAtZoom = FALSE
      ),
      data = data
    )
  return(list(map = map, groups = group))
}


map_add_lines <- function(map, group, data, color, useClub = TRUE) {
  if (nrow(data) == 0) return(list(map=map, groups=NULL))
  dat <- data %>% group_by(LineGroupId, Desc, Club) %>% nest()
  grp <- map_chr(1:nrow(dat), function(i) {
    g <- if_else(useClub, str_c(dat$Club[i], " (", group, ")"), group)
    map <<- map %>%
      addPolylines(~long, ~lat, label = ~Desc, popup = ~Desc,
                   group = g, color = color, weight = 1.5, opacity = 0.75,
                   data = dat[i,] %>% unnest(col = c(data)))
    return(g)
  })
  grp <- unique(grp)
  return(list(map=map, groups=grp))
}

  