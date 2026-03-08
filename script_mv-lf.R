# library(xml2)
# library(rvest)
# library(jsonlite)
# library(tidyverse)
# library(lubridate)
# library(zoo)
# library(forecast)
# library(tsibble)
# library(fs)
# library(rmarkdown)
# library(conflicted)
# conflicts_prefer(
#   dplyr::filter,
#   dplyr::lag,
#   plotly::layout
# )

here::i_am("script_mv-lf.R")
library(riverdata)


#### Catch ####
# To 2023 only HI-LF data
url <- "https://fangstjournalen.dtu.dk/fangst.nsf/xsp/app/v3/catches/club/37E6FB0707BAE0A1C1257F3A003FF013/1/"
prefix <- "data/data_mv-lf"
yr <- year(now())

#### Catch records ####
datCatch <- write_catch(url, prefix, yr, species = "all", club = TRUE)


#### Map ####
datLines <- tibble()
datMarkers <- tibble()
lineId <- 0

# Stednavne Karup
lst <- map_strip_kml("1XJoAUKY_-kbmhZgovPpLgi82Gn8", start_ctr = lineId)  
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)
if (nrow(datLines) != 0) {lineId <- max(datLines$LineGroupId)}

# Stednavne Skjern
lst <- map_strip_kml("135J9l0kVoBKkdIdG_0vc3U9WJeuUPWyJ", start_ctr = lineId)  
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)
if (nrow(datLines) != 0) {lineId <- max(datLines$LineGroupId)}

# Karup
lst <- map_strip_kml("1A1Oi7hPeFbAU2ahS_zWgg6Fqcxa5Gbg", club = "MV-LF", start_ctr = lineId)  
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)
if (nrow(datLines) != 0) {lineId <- max(datLines$LineGroupId)}

## Skjern
lst <- map_strip_kml("1NiY_55Mw_GUULepLXq6Wjt0Gtu2K2c0", club = "MV-LF", start_ctr = lineId)  
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)
if (nrow(datLines) != 0) {lineId <- max(datLines$LineGroupId)}

# Storå
lst <- map_strip_kml("1_xtzzPTUUiecahNG71C_4rOY_izwVmU", club = "MV-LF", start_ctr = lineId)  
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)
if (nrow(datLines) != 0) {lineId <- max(datLines$LineGroupId)}

# Lakes
lst <- map_strip_kml("14vdqfI93frzgB0jmm44GPP3bIu5aVH8", club = "MV-LF", start_ctr = lineId)  
datMarkers <- bind_rows(datMarkers, lst$datMarkers) 
datLines <- bind_rows(datLines, lst$datLines)

# Save map data to csv files (for use in leaflet map in report)
datMarkers <- datMarkers  %>% filter(!is.na(Icon))
write_csv(datMarkers, str_c(prefix, "_mapmarkers.csv"))
write_csv(datLines, str_c(prefix, "_maplines.csv"))


#### Generate reports ####
yr <- year(lubridate::now())
if (month(lubridate::now()) < 5)  yr <- yr - 1
render("reports/mv-lf/mv-lf-kort.Rmd", output_dir = "docs/mv-lf")
render("reports/mv-lf/mv-lf-report.Rmd", output_dir = "docs/mv-lf", output_file = str_c("mv-lf-report-", yr), params = list(yr = yr))
render("reports/relund/dashboard.Rmd", output_dir = "docs/relund")
# render(here::here("reports/mv-lf/mv-lf-report.Rmd"), output_file = here::here(str_c("docs/mv-lf/mv-lf-report-", yr, ".html")), params = list(yr = yr))
