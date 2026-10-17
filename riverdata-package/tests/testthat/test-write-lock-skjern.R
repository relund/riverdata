lock_flow_html <- function(records) {
  json <- jsonlite::toJSON(records, auto_unbox = TRUE, null = "null")
  paste0("<html><script>const other = [];</script><div></div><script>\n",
         "const chartData =\n", json, ";\nconst labels = [];</script></html>")
}

lock_flow_table <- function(times, flows) {
  tibble::tibble(DateTime = as.POSIXct(times, tz = "UTC"), Flow = as.numeric(flows))
}

test_that("flow parser finds JSON and preserves flow values and intervals", {
  html <- lock_flow_html(list(
    list(datetime = "2026-10-17 16:20:00", sflow = "-319.5"),
    list(datetime = "2026-10-17 16:25:00", sflow = 0),
    list(datetime = "2026-10-17 16:35:00", sflow = "12.75"),
    list(datetime = "2026-10-17 16:40:00", sflow = NULL)
  ))
  result <- .parse_lock_flow(html)
  expect_identical(names(result), c("DateTime", "Flow"))
  expect_equal(result$Flow, c(-319.5, 0, 12.75, NA_real_))
  expect_equal(format(result$DateTime, tz = "UTC", usetz = FALSE),
               c("2026-10-17 14:20:00", "2026-10-17 14:25:00",
                 "2026-10-17 14:35:00", "2026-10-17 14:40:00"))
})

test_that("flow parser uses Danish winter and summer time", {
  result <- .parse_lock_flow(lock_flow_html(list(
    list(datetime = "2026-01-15 12:00:00", sflow = "1"),
    list(datetime = "2026-07-15 12:00:00", sflow = "2")
  )))
  expect_equal(format(result$DateTime, tz = "UTC", usetz = FALSE),
               c("2026-01-15 11:00:00", "2026-07-15 10:00:00"))
})

test_that("flow parser rejects ambiguous, nonexistent, and invalid dates", {
  for (time in c("2026-10-25 02:30:00", "2026-03-29 02:30:00",
                 "2026-02-30 12:00:00", "not a date")) {
    expect_error(.parse_lock_flow(lock_flow_html(list(
      list(datetime = time, sflow = "1")
    ))), "timestamp|datetime")
  }
})

test_that("flow parser rejects empty, malformed, or incomplete data", {
  for (html in c("<html><script>const other = [];</script></html>",
                 "<script>const chartData = [];</script>",
                 "<script>const chartData = [broken];</script>",
                 "<script>const chartData = [{\"datetime\":\"2026-10-17 12:00:00\"}];</script>")) {
    expect_error(.parse_lock_flow(html))
  }
  for (value in list("bad", "", "Inf", TRUE, list(1, 2))) {
    expect_error(.parse_lock_flow(lock_flow_html(list(
      list(datetime = "2026-10-17 12:00:00", sflow = value)
    ))), "sflow")
  }
})

test_that("flow fetch retries failures and malformed pages with three total attempts", {
  attempts <- 0
  sleeps <- numeric()
  fetch <- function(url) {
    attempts <<- attempts + 1
    if (attempts == 1) stop("connection failed")
    if (attempts == 2) return("<html>No chart</html>")
    lock_flow_html(list(list(datetime = "2026-10-17 12:00:00", sflow = "12")))
  }
  result <- suppressMessages(.fetch_lock_flow(
    fetch = fetch, sleep = function(seconds) sleeps <<- c(sleeps, seconds)
  ))
  expect_equal(attempts, 3)
  expect_equal(sleeps, c(2, 5))
  expect_equal(result$Flow, 12)
  attempts <- 0
  expect_error(suppressMessages(.fetch_lock_flow(
    fetch = function(url) { attempts <<- attempts + 1; stop("unavailable") },
    sleep = function(seconds) NULL
  )), "failed after 3 attempts")
  expect_equal(attempts, 3)
})

test_that("merge prefers valid new readings and preserves history and missing timestamps", {
  previous <- lock_flow_table(
    c("2026-10-16 10:00:00", "2026-10-17 10:00:00", "2026-10-17 10:05:00", NA),
    c(5, 10, 15, NA)
  )
  fresh <- lock_flow_table(
    c("2026-10-17 10:00:00", "2026-10-17 10:05:00", "2026-10-17 10:10:00",
      "2026-10-17 10:10:00"), c(12, NA, 0, 0)
  )
  result <- .merge_lock_flow(fresh, previous)
  expect_equal(result$Flow, c(0, 15, 12, 5, NA_real_))
  expect_equal(sum(is.na(result$DateTime)), 1)
  expect_equal(.merge_lock_flow(fresh, result), result)
  conflict <- lock_flow_table(rep("2026-10-17 10:00:00", 2), c(1, 2))
  expect_error(.merge_lock_flow(conflict, previous), "conflicting duplicate")
  expect_error(.merge_lock_flow(fresh, conflict), "conflicting duplicate")
})

test_that("writer creates a UTC archive and repeated updates are idempotent", {
  prefix <- tempfile("lock-flow-")
  filename <- paste0(prefix, "_flow_lock.csv")
  on.exit(unlink(filename))
  fresh <- lock_flow_table(c("2026-10-17 10:00:00", "2026-10-17 10:05:00"), c(0, -12.5))
  local_mocked_bindings(.fetch_lock_flow = function() fresh)
  result <- suppressMessages(withVisible(write_lock_skjern(prefix)))
  expect_false(result$visible)
  expect_equal(result$value$Flow, c(-12.5, 0))
  expect_match(readLines(filename)[2], "2026-10-17T10:05:00Z,-12.5", fixed = TRUE)
  before <- readLines(filename)
  expect_equal(suppressMessages(write_lock_skjern(prefix)), result$value)
  expect_identical(readLines(filename), before)
})

test_that("writer reports gaps and preserves untimestamped archived rows", {
  prefix <- tempfile("lock-flow-")
  filename <- paste0(prefix, "_flow_lock.csv")
  on.exit(unlink(filename))
  readr::write_csv(lock_flow_table(c("2026-10-07 02:00:00", NA), c(-251, NA)), filename)
  fresh <- lock_flow_table("2026-10-17 10:00:00", 1)
  local_mocked_bindings(.fetch_lock_flow = function() fresh)
  expect_warning(expect_warning(
    result <- suppressMessages(write_lock_skjern(prefix)), "without a timestamp"
  ), "does not overlap")
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$DateTime[3]))
})

test_that("fetch, archive, and write failures leave the existing file untouched", {
  prefix <- tempfile("lock-flow-")
  filename <- paste0(prefix, "_flow_lock.csv")
  on.exit(unlink(filename))
  old <- lock_flow_table("2026-10-17 10:00:00", -12)
  readr::write_csv(old, filename)
  before <- readLines(filename)
  local_mocked_bindings(.fetch_lock_flow = function() stop("download failed"))
  expect_error(suppressMessages(write_lock_skjern(prefix)), "download failed")
  expect_identical(readLines(filename), before)
  local_mocked_bindings(.fetch_lock_flow = function() old,
                       write_csv = function(...) stop("write failed"))
  expect_error(suppressMessages(write_lock_skjern(prefix)), "write failed")
  expect_identical(readLines(filename), before)
})

test_that("invalid archive values do not get silently dropped", {
  prefix <- tempfile("lock-flow-")
  filename <- paste0(prefix, "_flow_lock.csv")
  on.exit(unlink(filename))
  writeLines(c("DateTime,Flow", "2026-10-17T10:00:00Z,bad"), filename)
  before <- readLines(filename)
  local_mocked_bindings(.fetch_lock_flow = function() lock_flow_table("2026-10-17 10:00:00", 1))
  expect_error(suppressWarnings(suppressMessages(write_lock_skjern(prefix))), "invalid archive")
  expect_identical(readLines(filename), before)
})
