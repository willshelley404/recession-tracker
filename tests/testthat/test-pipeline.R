full_raw <- make_raw(301, usrec_on = c(40:48, 120:128, 200:210, 260:266))
drop_last <- function(raw, k = 1) {
  raw$series <- lapply(raw$series, function(df) utils::head(df[order(df$date), ], nrow(df) - k))
  raw
}
stub <- function(raw) function() raw

run_tmp <- function(fetch, dir, ...) {
  run_pipeline(fetch = fetch, render = FALSE,
               log_path = file.path(dir, "predictions.csv"),
               usrec_path = file.path(dir, "usrec.csv"),
               now = as.POSIXct("2026-10-30 12:00:00", tz = "UTC"), ...)
}

test_that("first run rebuilds", {
  dir <- withr::local_tempdir()
  res <- run_tmp(stub(drop_last(full_raw)), dir)
  expect_equal(res$status, "rebuilt")
  lg <- read_log(file.path(dir, "predictions.csv"))
  expect_true(all(lg$source == "backtest"))
  expect_true(file.exists(file.path(dir, "usrec.csv")))
})

test_that("second run adds nothing", {
  dir <- withr::local_tempdir()
  run_tmp(stub(drop_last(full_raw)), dir)
  before <- tools::md5sum(file.path(dir, "predictions.csv"))
  res <- run_tmp(stub(drop_last(full_raw)), dir)
  expect_equal(res$status, "unchanged")
  expect_equal(res$n_new, 0)
  expect_equal(tools::md5sum(file.path(dir, "predictions.csv")), before)
})

test_that("new month is appended as live", {
  dir <- withr::local_tempdir()
  run_tmp(stub(drop_last(full_raw)), dir)
  n0 <- nrow(read_log(file.path(dir, "predictions.csv")))
  res <- run_tmp(stub(full_raw), dir)
  lg <- read_log(file.path(dir, "predictions.csv"))
  expect_equal(res$status, "appended")
  expect_equal(nrow(lg), n0 + 1)
  expect_equal(sum(lg$source == "live"), 1)
  expect_equal(lg$month[lg$source == "live"], max(full_raw$series$USREC$date))
})

test_that("missed months are backfilled as live rows", {
  dir <- withr::local_tempdir()
  run_tmp(stub(drop_last(full_raw, 3)), dir)
  res <- run_tmp(stub(full_raw), dir)
  expect_equal(res$n_new, 3)
})

test_that("fetch failure leaves log untouched", {
  dir <- withr::local_tempdir()
  run_tmp(stub(drop_last(full_raw)), dir)
  before <- tools::md5sum(file.path(dir, "predictions.csv"))
  expect_error(run_tmp(function() stop("FRED is down"), dir), "FRED is down")
  expect_equal(tools::md5sum(file.path(dir, "predictions.csv")), before)
})

test_that("rebuild flag archives the old log", {
  dir <- withr::local_tempdir()
  run_tmp(stub(drop_last(full_raw)), dir)
  res <- run_tmp(stub(full_raw), dir, rebuild = TRUE)
  expect_equal(res$status, "rebuilt")
  expect_length(list.files(file.path(dir, "archive")), 1)
})

test_that("interpolated months are recorded", {
  dir <- withr::local_tempdir()
  raw <- drop_last(full_raw)
  raw$interpolated <- data.frame(series = "CPIAUCSL", month = as.Date("1995-10-01"))
  run_tmp(stub(raw), dir)
  lg <- read_log(file.path(dir, "predictions.csv"))
  expect_match(lg$interpolated[lg$month == as.Date("1995-10-01")], "CPIAUCSL")
  expect_match(lg$interpolated[lg$month == as.Date("1996-10-01")], "CPIAUCSL") # via the YoY base month
})
