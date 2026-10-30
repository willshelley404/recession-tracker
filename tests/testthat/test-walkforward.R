# 1980-01 .. 2004-12 with four recessions, enough positives to fit from 1987
wf_panel <- function() {
  build_panel(make_raw(300, usrec_on = c(40:48, 120:128, 200:210, 260:266)))
}

test_that("no training row is within the embargo", {
  out <- walk_forward(wf_panel())
  expect_gt(nrow(out), 0)
  jan <- as.Date(paste0(format(out$month, "%Y"), "-01-01"))
  expect_true(all(out$train_end <= jan %m-% n_months(EMBARGO_MONTHS)))
})

test_that("scores the unlabelled tail", {
  p <- wf_panel()
  out <- walk_forward(p)
  tail_months <- p$date[is.na(p$rec_next12) & p$complete]
  expect_gt(length(tail_months), 0)
  expect_true(all(tail_months %in% out$month))
})

test_that("incomplete months are skipped", {
  p <- wf_panel()
  hole <- as.Date("1995-06-01")
  p$sent_chg12[p$date == hole] <- NA
  p$complete <- stats::complete.cases(p[, PREDICTORS])
  out <- walk_forward(p)
  expect_false(hole %in% out$month)
  expect_true(as.Date("1995-07-01") %in% out$month)
})

test_that("skip rule: too few positives means no forecast", {
  p <- wf_panel()
  early <- p$date < as.Date("1990-01-01")
  p$rec_next12[early & !is.na(p$rec_next12)] <- 0
  p$rec_next12[p$date %in% as.Date(c("1983-01-01", "1983-02-01", "1983-03-01"))] <- 1
  out <- walk_forward(p)
  expect_false(any(out$month < as.Date("1990-01-01")))
})

test_that("score_month matches walk_forward in January", {
  p <- wf_panel()
  jan <- as.Date("1998-01-01")
  wf <- walk_forward(p)
  one <- score_month(p, jan)
  expect_equal(nrow(one), 1)
  expect_equal(one$prob, wf$prob[wf$month == jan], tolerance = 1e-12)
})

test_that("score_month returns zero rows for an incomplete month", {
  p <- wf_panel()
  p$complete[p$date == as.Date("1998-03-01")] <- FALSE
  expect_equal(nrow(score_month(p, as.Date("1998-03-01"))), 0)
})

test_that("output columns", {
  out <- walk_forward(wf_panel())
  expect_named(out, c("month", "prob", "train_end", "n_train", "train_base_rate", PREDICTORS, PARAM_COLS))
  expect_true(all(out$n_train <= WINDOW_MONTHS))
})
