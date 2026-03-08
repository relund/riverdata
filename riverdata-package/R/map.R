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

#' Parse KML map data into marker/line tables
#'
#' @param map_id Google map ID.
#' @param club Optional club label.
#' @param group_name_markers Optional marker group name filter.
#' @param group_name_lines Optional line group name filter.
#' @param start_ctr Counter for line group IDs used up till now (to avoid duplicates when combining multiple maps).
#'
#' @return List with `datMarkers` and `datLines`.
#' @examples
#' \dontrun{
#' map_strip_kml("1XJoAUKY_-kbmhZgovPpLgi82Gn8")
#' }
map_strip_kml <- function(
    map_id,
    club = NA,
    group_name_markers = NULL,
    group_name_lines = NULL,
    start_ctr = 0
) {
  kml <- read_xml(str_c("https://www.google.com/maps/d/u/0/kml?mid=", map_id, "&forcekml=1"))
  xml_ns_strip(kml)
  x <- xml_find_all(kml, "//Folder")

  datMarkers <- NULL
  if (length(xml_find_all(x, ".//Point")) > 0) {
    datMarkers <- bind_rows(map(x, function(n) {  # for each Folder
      folderName <- xml_text(xml_find_all(n, "./name"))  # Folder name
      y <- xml_find_all(n, "./Placemark[Point]")
      res <- tibble(Desc = xml_text(xml_find_first(y, "./name")),
                    Text = xml_text(xml_find_first(y, "./description")),
                    cord = xml_text(xml_find_first(y, ".//coordinates"), trim = TRUE) ) %>%
        mutate(long = as.numeric(str_split_fixed(cord, ",", 3)[,1]), lat = as.numeric(str_split_fixed(cord, ",", 3)[,2])) %>%
        select(-cord)
      tibble(Group = folderName, Point = list(res))
    })) %>%
      filter(map_lgl(Point, function(df) nrow(df) > 0)) %>%
      unnest(Point) %>% mutate(Club = club) %>%
      mutate(Icon = case_when(
        str_detect(Group, fixed("parkering", ignore_case = TRUE)) ~ "park.png",
        str_detect(Group, fixed("standpladser", ignore_case = TRUE)) ~ "fish.png",
        str_detect(Group, fixed("shelter", ignore_case = TRUE)) ~ "shelter.png",
        str_detect(Group, fixed("sten", ignore_case = TRUE)) ~ "rock.png",
        str_detect(Desc, fixed("hytte", ignore_case = TRUE)) ~ "cottage.png",
        str_detect(Desc, fixed("indhegning", ignore_case = TRUE)) ~ "fence.png",
        str_detect(Desc, regex("skarrildhus", ignore_case = TRUE)) ~ "infoplace.png",
        str_detect(Desc, regex("hus", ignore_case = TRUE)) ~ "house.png",
        str_detect(Desc, fixed("fiskekort", ignore_case = TRUE)) ~ "house.png",
        str_detect(Desc, fixed("p-plads", ignore_case = TRUE)) ~ "park.png",
        str_detect(Desc, fixed("parkering", ignore_case = TRUE)) ~ "park.png",
        str_detect(Desc, fixed("spang", ignore_case = TRUE)) ~ "footbridge.png",
        str_detect(Desc, fixed("toilet", ignore_case = TRUE)) ~ "wc.png",
        str_detect(Desc, fixed("wc", ignore_case = TRUE)) ~ "wc.png",
        str_detect(Desc, fixed("info", ignore_case = TRUE)) ~ "infoplace.png",
        str_detect(Desc, fixed("shelter", ignore_case = TRUE)) ~ "shelter.png",
        str_detect(Desc, fixed("bro", ignore_case = TRUE)) ~ "bridge.png",
        str_detect(Desc, fixed("båd", ignore_case = TRUE)) ~ "boat.png",
        str_detect(Desc, fixed("fiskeret", ignore_case = TRUE)) ~ "infoplace.png",
        TRUE ~ NA_character_
      ))
    if (!is.null(group_name_markers)) datMarkers$Group <- group_name_markers
  }

  datLines <- NULL
  if (length(xml_find_all(x, ".//LineString")) > 0) {
    ctr <- start_ctr
    datLines <- bind_rows(map(x, function(n) {  # for each Folder
      folderName <- xml_text(xml_find_all(n, "./name"))  # Folder name
      y <- xml_find_all(n, "./Placemark[LineString]")
      res <- bind_rows(map(y, function(n) {  # for each Placemark
        lineName <- xml_text(xml_find_first(n, "./name"))
        lineText <- xml_text(xml_find_first(n, "./description"))
        txt <- xml_text(xml_find_all(n, "./LineString/coordinates"))
        l <- suppressWarnings(
          read_csv(txt, col_names = c("long", "lat", "h"), col_types = "ddd") %>%
            select(-h) %>%
            filter(!is.na(lat)))
        ctr <<- ctr + 1
        tibble(Desc = lineName, Text = lineText, LineCord = list(l), LineGroupId = ctr)
      }))
      tibble(Group = folderName, Line = list(res))
    })) %>%
      filter(map_lgl(Line, function(df) nrow(df) > 0)) %>%
      unnest(col = c(Line)) %>%
      unnest(col = c(LineCord)) %>%
      mutate(Club = club)
    if (!is.null(group_name_lines)) datLines$Group <- group_name_lines
  }
  return(list(datMarkers = datMarkers, datLines = datLines))
}



  
