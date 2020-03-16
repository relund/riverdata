#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(zoo) 

prefix <- "https://raw.githubusercontent.com/relund/skjern/master/data/"
readGHCatch <- function(file) {
  colT <- cols(
    Date = col_date(format = ""),
    Length = col_double(),
    Weight = col_double(),
    Name = col_character(),
    Place = col_character(),
    Method = col_character(),
    Cut = col_logical(),
    Foto = col_character(),
    Killed = col_logical(),
    Sex = col_character(),
    Fulton = col_double()
  )
  read_csv(paste0(prefix, file), col_types = colT)
}
datCatch <- readGHCatch("data_karup_catch_seatrout_2003-2019.csv")
tmp <- readGHCatch("data_karup_catch_seatrout_2020-.csv")
datCatch <- bind_rows(datCatch, tmp) 
tmp <- transmute(datCatch, 
                  K = if_else(!Killed, '<img src="c_and_r.gif" alt="C&R">', "", ""),
                  M = if_else(Sex == 'Male', '<img src="boy.gif" alt="Han">', "", ""),
                  F = if_else(Sex == 'Female', '<img src="girl.gif" alt="Hun">', "", ""),
                  C = if_else(!is.na(Foto), paste0('<a href="', Foto, '"><img src="foto.gif" alt="Foto"></a>'), "", "")
                  ) %>% transmute(Misc = paste0('<span>',K,M,F,C,'</span>'))
datCatch <- bind_cols(datCatch, tmp) %>% select(Date, Name, Length, Weight, Method, Place, Fulton, Misc)



# y <- year(now())
# fn <- paste0("https://raw.githubusercontent.com/relund/skjern/master/data/data_karup_waterlevel_", y, ".csv")
# datWLevel <- read_csv(fn) %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# ggplot(data = datWLevel, aes(x = Date, y = Level)) + geom_line(aes(group = Group))




readGHWLevels <- function(years) {
  colT <- cols(
    Date = col_datetime(format = ""),
    'Karup By (054764)' = col_double(),
    'Hagebro (001762)' = col_double(),
    'Nørkærbro (001767)' = col_double()
  )
  dat <- NULL
  for (y in years) {
    fn <- paste0(prefix, "data_karup_waterlevel_", y, ".csv")
    dat <- bind_rows(dat, read_csv(fn, col_types = colT))
  }
  return(dat)
}
dat <- readGHWLevels(2013:2020)
# datL <- dat %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
# datS <- datL %>% dplyr::filter(year(Date)>2013)
# pWRaw <- ggplot(data = datS, aes(x = Date, y = Level)) + geom_line(aes(color = Group), show.legend = T)
rMeans <- read_csv(paste0(prefix,"data_karup_waterlevel_avg90.csv"))
dat1 <- dat %>% mutate(Day = yday(Date)) 
dat1 <- left_join(dat1, rMeans)
datL <- dat1 %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
datS <- datL %>% dplyr::filter(year(Date)>2019)
pWMA <- ggplot(data = datS, aes(x = Date, y = Level)) + 
  geom_line(aes(color = Group), show.legend = T, na.rm = T) + 
  labs(title = "Vandstand i Karup Å", 
       subtitle = "Gennemsnit er beregnet over 90 dage (givet data for alle år)") +
  xlab("Vandstand (m)") + ylab("Dato") +
  theme(legend.position="bottom") + 
  scale_color_discrete(name = "", labels = c("Hagebro", "Hagebro (gens)", 
                                             "Karup By", "Karup By (gens)", 
                                             "Nørkærbro", "Nørkærbro (gens)"))
pWMA

## Normilize to moving avg
dat2 <- dat1 %>% mutate(`Karup By (054764)` = `Karup By (054764)` - `Karup By (054764) rAvg90`, 
                        `Hagebro (001762)` = `Hagebro (001762)` - `Hagebro (001762) rAvg90`,
                        `Nørkærbro (001767)` = `Nørkærbro (001767)` - `Nørkærbro (001767) rAvg90`) %>% 
  select(Date, `Karup By (054764)`, `Hagebro (001762)`, `Nørkærbro (001767)`)
datL <- dat2 %>% pivot_longer(cols = contains(c('K','H')), names_to = 'Group', values_to = 'Level')
datS <- datL %>% dplyr::filter(year(Date)>2019)
pWN <- ggplot(data = datS, aes(x = Date, y = Level)) + 
  geom_hline(yintercept = 0) +
  geom_smooth(aes(group = Group), na.rm = T, method = lm, formula = y ~ splines::bs(x, 40), se = FALSE, color = "grey", lwd=0.5) +
  geom_line(aes(color = Group), show.legend = T, na.rm = T) + 
  labs(title = "Relativ vandstand i Karup Å", 
       subtitle = "Vandstand relativ i forhold til gennemsnit over 90 dage") +
  # xlab("Vandstand (m)") + ylab("Dato") +
  theme(legend.position="bottom") + 
  scale_color_discrete(name = "", labels = c("Hagebro", "Karup By", "Nørkærbro"))
pWN
pWN + facet_grid(rows = vars(Group))



shinyServer(
    function(input, output) {
      
# first tab
output$tabCatch <-
    DT::renderDataTable(
      DT::datatable(
        datCatch,
        colnames = c('Dato', 'Navn', 'Længde', 'Vægt', 'Metode', 'Sted', 'Fulton', 'Diverse'),
        class = 'stripe row-border order-column nowrap', # nowrap display compact
        rownames = FALSE,
        # filter = 'bottom',
        escape = FALSE,
        options = list(pageLength = 15, order = list(list(0, 'desc')), 
                       # autoWidth = TRUE,
                       columnDefs = list(list(className = 'dt-center', targets = 2:7),
                                         list(targets = 7, render = JS(
                                           "function(data, type, row, meta) {",
                                           "return data;",
                                           "}"))
                                         # list(width = '70px', targets = c(2,3,6,7))
                                    ),
                       fixedHeader = TRUE, 
                       rowGroup = list(dataSrc = 0, startRender = JS("function(rows, group) {",
                                                                     "var res = group.split('-');",
                                                                     "res = res[2] + '/' + res[1] + '/' + res[0];",
                                                                     "return res + ' (' + rows.count() + ')';}"))
                  ),
        extensions = c('Responsive','FixedHeader','RowGroup')
      ) %>% 
        formatDate('Date', 'toLocaleDateString') %>% 
        formatCurrency(c('Length','Weight','Fulton'), digits = c(0,1,2), currency = "") 
    )

# second tab
output$plotWLevel <- renderPlot(pWMA)


        # -1 means no pagination; the 2nd element contains menu labels
        # output$ex2 <- renderPlot()
        
      #   # you can also use paging = FALSE to disable pagination
      #   output$ex3 <- DT::renderDataTable(
      #       DT::datatable(iris, options = list(paging = FALSE))
      #   )
      #   
      #   # turn off filtering (no searching boxes)
      #   output$ex4 <- DT::renderDataTable(
      #       DT::datatable(iris, options = list(searching = FALSE))
      #   )
      #   
      #   # write literal JS code in JS()
      #   output$ex5 <- DT::renderDataTable(DT::datatable(
      #       iris,
      #       options = list(rowCallback = DT::JS(
      #           'function(row, data) {
      #   // Bold cells for those >= 5 in the first column
      #   if (parseFloat(data[1]) >= 5.0)
      #     $("td:eq(1)", row).css("font-weight", "bold");
      # }'
      #       ))
      #   ))
    }    
    
    
#     function(input, output) {
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#     })
# 
# }

)
