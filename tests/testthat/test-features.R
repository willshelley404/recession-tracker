val_at <- function(raw, id, d) raw$series[[id]]$value[raw$series[[id]]$date == d]

test_that("yoy is date-matched across a missing month", {
  gap <- as.Date("1985-06-01")
  raw <- make_raw(120, missing = list(CPIAUCSL = gap))
  p <- build_panel(raw)
  d <- as.Date("1986-08-01") # 14 months after the gap; a row-offset lag would be off by one
  yoy <- (val_at(raw, "CPIAUCSL", d) / val_at(raw, "CPIAUCSL", as.Date("1985-08-01")) - 1) * 100
  expect_equal(p$real_rate[p$date == d], val_at(raw, "FEDFUNDS", d) - yoy, tolerance = 1e-9)
  expect_true(is.na(p$real_rate[p$date == as.Date("1986-06-01")])) # its base month is missing
})

test_that("rec_next12 looks forward 12 months", {
  raw <- make_raw(60, usrec_on = 30L)
  p <- build_panel(raw)
  expect_equal(which(p$rec_next12 == 1), 18:29)
  expect_true(all(is.na(p$rec_next12[55:60]))) # < 6 future months
  expect_true(all(p$rec_next12[c(1:17, 30:54)] == 0))
})

test_that("u_rise ignores a missing month", {
  gap <- as.Date("1984-03-01")
  raw <- make_raw(120, missing = list(UNRATE = gap))
  p <- build_panel(raw)
  after <- p$date > gap & p$date <= seq(gap, by = "11 months", length.out = 2)[2]
  expect_false(anyNA(p$u_rise[after]))
})

test_that("yield_spread falls back to DGS10-DGS2", {
  d <- as.Date("1983-01-01")
  raw <- make_raw(120, missing = list(T10Y2Y = d))
  p <- build_panel(raw)
  expect_equal(p$yield_spread[p$date == d], val_at(raw, "DGS10", d) - val_at(raw, "DGS2", d))
})

test_that("sent_chg12 is the date-matched 12-month change", {
  raw <- make_raw(120)
  p <- build_panel(raw)
  d <- as.Date("1990-05-01")
  expect_equal(p$sent_chg12[p$date == d],
               val_at(raw, "UMCSENT", d) - val_at(raw, "UMCSENT", as.Date("1989-05-01")))
})

test_that("complete flags rows with all six predictors", {
  raw <- make_raw(120)
  p <- build_panel(raw)
  expect_equal(p$complete, stats::complete.cases(p[, PREDICTORS]))
  expect_false(p$complete[1]) # no 12-month history yet
  expect_true(p$complete[120])
})
