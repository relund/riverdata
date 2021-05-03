# Need to run this for getting the local right

options(rsconnect.locale = "da_DK.UTF-8")

## Skjern Dashboard
rsconnect::deployApp(
  appDir = "./skjern",
  appPrimaryDoc = "skjern.Rmd",
  appSourceDoc = "./skjern/skjern.Rmd",
  account = "relund",
  server = "shinyapps.io",
  appName = "skjern",
  appId = 2094215,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  forceUpdate = TRUE,
  lint = FALSE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE,
    ignoredFiles = "skjern.html|skjern_data"
  )
) 

## Karup Dashboard
rsconnect::deployApp(
  appDir = "./karup",
  appPrimaryDoc = "karup.Rmd",
  appSourceDoc = "./karup/karup.Rmd",
  account = "relund",
  server = "shinyapps.io",
  appName = "karup",
  appId = 1951623,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  forceUpdate = TRUE,
  lint = FALSE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE,
    ignoredFiles = "karup.html|karup_data/data_chunks_index.txt|karup_data/unnamed-chunk-19.RData|karup_data/unnamed-chunk-2.RData|karup_data/unnamed-chunk-28.RData"
  )
) 