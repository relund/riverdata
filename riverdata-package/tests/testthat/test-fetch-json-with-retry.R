test_that("fetch succeeds without retrying", {
  attempts <- 0
  sleeps <- numeric()
  fetch <- function(url) {
    attempts <<- attempts + 1
    list(PlotRecs = matrix(c("date", 1), nrow = 1))
  }

  result <- .fetch_json_with_retry(
    "example", "Station", 1,
    retry_delays = c(1, 2, 3),
    fetch = fetch,
    sleep = function(seconds) sleeps <<- c(sleeps, seconds)
  )

  expect_equal(attempts, 1)
  expect_length(sleeps, 0)
  expect_true("PlotRecs" %in% names(result))
})

test_that("fetch retries with the configured delays", {
  attempts <- 0
  sleeps <- numeric()
  fetch <- function(url) {
    attempts <<- attempts + 1
    if (attempts < 3) stop("temporary failure")
    list(PlotRecs = matrix(c("date", 1), nrow = 1))
  }

  expect_message(
    result <- .fetch_json_with_retry(
      "example", "Station", 1,
      retry_delays = c(5, 15, 45),
      fetch = fetch,
      sleep = function(seconds) sleeps <<- c(sleeps, seconds)
    ),
    "Retrying in 5 seconds"
  )

  expect_equal(attempts, 3)
  expect_equal(sleeps, c(5, 15))
  expect_true("PlotRecs" %in% names(result))
})

test_that("fetch warns and returns NULL after the final failure", {
  attempts <- 0
  fetch <- function(url) {
    attempts <<- attempts + 1
    stop("service unavailable")
  }

  expect_warning(
    result <- suppressMessages(.fetch_json_with_retry(
      "example", "Station", 468,
      retry_delays = c(0, 0, 0),
      fetch = fetch,
      sleep = function(seconds) NULL
    )),
    "Station \\(468\\) after 4 attempts"
  )

  expect_equal(attempts, 4)
  expect_null(result)
})

test_that("fetch retries malformed responses", {
  attempts <- 0
  fetch <- function(url) {
    attempts <<- attempts + 1
    if (attempts == 1) return(list(other = "value"))
    list(PlotRecs = matrix(numeric(), nrow = 0, ncol = 2))
  }

  result <- suppressMessages(.fetch_json_with_retry(
    "example", "Station", 1,
    retry_delays = 0,
    fetch = fetch,
    sleep = function(seconds) NULL
  ))

  expect_equal(attempts, 2)
  expect_length(result$PlotRecs, 0)
})
