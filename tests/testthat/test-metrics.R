toy_log <- function(months, prob, actual = NA, base = 0.2) {
  rows <- data.frame(
    month = as.Date(months), prob = prob, train_end = as.Date("1980-01-01"), n_train = 360L,
    train_base_rate = base, as.data.frame(as.list(setNames(rep(0, length(PREDICTORS)), PREDICTORS))),
    toy_params()
  )
  lg <- as_log_rows(rows, "backtest", as.POSIXct("2026-10-30", tz = "UTC"))
  lg$rec_next12_actual <- actual
  lg
}
mseq <- function(from, n) seq(as.Date(from), by = "1 month", length.out = n)

test_that("auc is 1 for perfect ranking", {
  lg <- toy_log(mseq("2000-01-01", 4), c(0.1, 0.2, 0.8, 0.9), c(0, 0, 1, 1))
  m <- backtest_metrics(lg)
  expect_equal(m$auc, 1)
  expect_equal(m$hit_rate, 1)
  expect_equal(m$false_alarm_rate, 0)
  expect_equal(m$n, 4)
})

test_that("skill is 0 when prob equals base rate", {
  lg <- toy_log(mseq("2000-01-01", 10), 0.3, c(rep(0, 7), 1, 1, 1), base = 0.3)
  expect_equal(backtest_metrics(lg)$skill, 0)
})

test_that("metrics ignore rows without an outcome", {
  lg <- toy_log(mseq("2000-01-01", 4), c(0.1, 0.9, 0.5, 0.5), c(0, 1, NA, NA))
  expect_equal(backtest_metrics(lg)$n, 2)
})

test_that("recession_spans finds USREC spells after `from`", {
  usrec <- data.frame(date = mseq("1980-01-01", 40), recession = c(rep(0, 10), 1, 1, rep(0, 20), 1, 1, 1, rep(0, 5)))
  sp <- recession_spans(usrec, from = as.Date("1981-01-01"))
  expect_equal(nrow(sp), 1)
  expect_equal(sp$start, as.Date("1982-09-01"))
  expect_equal(sp$end, as.Date("1982-11-01"))
})

test_that("runup and spell windows differ", {
  # recession 2001-03..2001-11; probs low beforehand, high once it starts
  months <- mseq("1999-01-01", 48)
  prob <- ifelse(months >= as.Date("2001-03-01") & months <= as.Date("2001-11-01"), 0.8, 0.2)
  lg <- toy_log(months, prob)
  usrec <- data.frame(date = months, recession = as.numeric(prob == 0.8))
  rt <- runup_table(lg, usrec)
  expect_equal(rt$recession, "2001")
  expect_equal(rt$runup_peak, 0.2) # 2000-03..2001-02
  expect_equal(rt$spell_peak, 0.8) # 2000-03..2001-10 includes the recession
  expect_gt(rt$spell_mean, rt$runup_mean)
})

test_that("latest_reading skips an unscorable month", {
  lg <- toy_log(as.Date(c("2026-08-01", "2026-10-01")), c(0.25, 0.31))
  r <- latest_reading(lg)
  expect_equal(r$month, as.Date("2026-10-01"))
  expect_equal(r$prev_month, as.Date("2026-08-01"))
  expect_equal(r$change_pts, 6, tolerance = 1e-9)
})

drivers_log <- function() {
  panel <- build_panel(make_raw(300, usrec_on = c(40:48, 120:128, 200:210, 260:266)))
  as_log_rows(walk_forward(panel), "backtest", as.POSIXct("2026-10-30", tz = "UTC"))
}

test_that("contributions rebuild the logged probability", {
  lg <- drivers_log()
  ct <- driver_contributions(lg)
  expect_named(ct, c("month", PREDICTORS, "b0"))
  expect_equal(plogis(ct$b0 + rowSums(ct[, PREDICTORS])), lg$prob, tolerance = 1e-4)
})

test_that("a contribution pushes up only on the risky side of the median", {
  lg <- drivers_log()
  ct <- driver_contributions(lg)
  for (p in PREDICTORS) {
    risky <- EXPECTED_SIGN[[p]] * (lg[[p]] - lg[[paste0("med_", p)]])
    expect_true(all(ct[[p]] * risky >= -1e-12), info = p)
  }
})

test_that("driver_change terms sum to the change in probability", {
  lg <- drivers_log()
  for (pair in list(c("1995-11-01", "1996-02-01"), c("1996-03-01", "1996-06-01"))) {
    from <- as.Date(pair[1])
    to <- as.Date(pair[2])
    d <- driver_change(lg, from, to)
    expect_equal(d$driver, c(PREDICTORS, "refit"))
    expect_lt(abs(sum(d$pts) - 100 * (lg$prob[lg$month == to] - lg$prob[lg$month == from])), 0.01)
  }
})

test_that("driver_change has no refit term within one fit", {
  d <- driver_change(drivers_log(), as.Date("1996-03-01"), as.Date("1996-06-01"))
  expect_equal(d$pts[d$driver == "refit"], 0)
})

test_that("driver_change is NULL for a month without parameters", {
  lg <- drivers_log()
  lg$b0[lg$month == as.Date("1996-03-01")] <- NA
  expect_null(driver_change(lg, as.Date("1996-03-01"), as.Date("1996-06-01")))
  expect_null(driver_change(lg, as.Date("1900-01-01"), as.Date("1996-06-01")))
})
