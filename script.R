library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)

## Flow though lock at Hvide Sande
url <- 'http://hyde.dk/Sflow/default_flow.asp'
webpage <- read_html(url)
dat <- html_nodes(webpage, xpath = "/html/body/div[2]/div[3]/script/text()")
dat <- html_text(dat)
dat <- str_replace_all(dat, "[\r\n\t]", "")

dates <- str_replace(dat, "^.*labels:", "{\n'labels' :")
dates <- str_replace(dates, "].*$", "]\n}")
dates <- str_replace_all(dates, "'", '"')
dates <- fromJSON(dates)$labels
# dates

flow <- str_replace(dat, "^.*data:", "{\n'data' :")
flow <- str_replace(flow, "].*$", "]\n}")
flow <- str_replace_all(flow, "'", '"')
flow <- fromJSON(flow)$data
# flow

if (length(dates)==length(flow)) {
  fn <- "data_flow_sluse.csv"
  dat <- tibble(dates = dmy_hm(dates, tz = "CET"), flow = flow)
  if (!file.exists(fn)) {
    write_csv2(dat, fn)
  } else {
    datOld <- read_csv2(fn, col_types = "Ti", locale = locale(tz = "CET"))
    dat <- bind_rows(datOld,dat)
    dat <- distinct(dat)
    write_csv2(dat, fn)
  }
}

# ## Wind, water levet etc at Hvide Sande
# url <- 'http://hyde.dk/'
# webpage <- read_html(url)
# dat <- html_nodes(webpage, xpath = "/html/body/div[4]/div[2]/div[1]/div[4]/p[2]/text()")
# dat <- html_text(dat)
# dat <- str_replace_all(dat, "[\r\n\t]", "")
# 
# dates <- str_replace(dat, "^.*labels:", "{\n'labels' :")
# dates <- str_replace(dates, "].*$", "]\n}")
# dates <- str_replace_all(dates, "'", '"')
# dates <- fromJSON(dates)$labels
# # dates
# 
# flow <- str_replace(dat, "^.*data:", "{\n'data' :")
# flow <- str_replace(flow, "].*$", "]\n}")
# flow <- str_replace_all(flow, "'", '"')
# flow <- fromJSON(flow)$data
# # flow
# 
# if (length(dates)==length(flow)) {
#   fn <- "data_flow_sluse.csv"
#   dat <- tibble(dates = dmy_hm(dates, tz = "CET"), flow = flow)
#   if (!file.exists(fn)) {
#     write_csv2(dat, fn)
#   } else {
#     datOld <- read_csv2(fn, col_types = "Ti", locale = locale(tz = "CET"))
#     dat <- bind_rows(datOld,dat)
#     dat <- distinct(dat)
#     write_csv2(dat, fn)
#   }
# }



