months <- function(n, start = as.Date("2020-01-01")) seq(start, by = "1 month", length.out = n)

test_that("interpolate fills a single gap log-linearly", {
  df <- data.frame(date = months(3), value = c(100, NA, 121))
  out <- interpolate_single_gaps(df)
  expect_equal(out$data$value[2], 110, tolerance = 1e-9)
  expect_equal(out$filled, df$date[2])
})

test_that("two-month gap is not filled", {
  df <- data.frame(date = months(4), value = c(1, NA, NA, 4))
  out <- interpolate_single_gaps(df)
  expect_true(all(is.na(out$data$value[2:3])))
  expect_length(out$filled, 0)
})

test_that("edge NA is not filled", {
  df <- data.frame(date = months(3), value = c(NA, 2, 3))
  out <- interpolate_single_gaps(df)
  expect_true(is.na(out$data$value[1]))
  expect_length(out$filled, 0)
})

test_that("non-positive series is filled linearly", {
  df <- data.frame(date = months(3), value = c(-1, NA, 1))
  expect_equal(interpolate_single_gaps(df)$data$value[2], 0)
})

test_that("missing key gives actionable error", {
  old <- Sys.getenv("FRED_API_KEY", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("FRED_API_KEY") else Sys.setenv(FRED_API_KEY = old))
  Sys.setenv(FRED_API_KEY = "")
  expect_error(require_fred_key(), "R_ENVIRON_USER")
})

test_that("absent row next to an NA is a two-month gap, not filled", {
  df <- data.frame(date = as.Date(c("2020-01-01", "2020-03-01", "2020-04-01")), value = c(100, NA, 121))
  out <- interpolate_single_gaps(df)
  expect_true(is.na(out$data$value[out$data$date == as.Date("2020-03-01")]))
  expect_length(out$filled, 0)
})

test_that("a single absent row is filled on the monthly grid", {
  df <- data.frame(date = as.Date(c("2020-01-01", "2020-03-01")), value = c(100, 121))
  out <- interpolate_single_gaps(df)
  expect_equal(out$data$value[out$data$date == as.Date("2020-02-01")], 110, tolerance = 1e-9)
  expect_equal(out$filled, as.Date("2020-02-01"))
})
