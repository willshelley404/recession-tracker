score_rows <- function(months, prob = 0.2) {
  data.frame(
    month = as.Date(months), prob = prob, train_end = as.Date("2020-01-01"), n_train = 360L,
    train_base_rate = 0.15, as.data.frame(as.list(setNames(rep(1, length(PREDICTORS)), PREDICTORS)))
  )
}
t0 <- as.POSIXct("2026-10-30 12:00:00", tz = "UTC")

test_that("as_log_rows produces the log schema", {
  lg <- as_log_rows(score_rows("2026-09-01"), "backtest", t0)
  expect_named(lg, LOG_COLS)
  expect_equal(lg$source, "backtest")
  expect_equal(lg$model_version, MODEL_VERSION)
})

test_that("append is idempotent", {
  lg <- as_log_rows(score_rows("2026-08-01"), "backtest", t0)
  lg <- append_live(lg, score_rows("2026-09-01"), t0)
  lg <- append_live(lg, score_rows("2026-09-01"), t0)
  expect_equal(sum(lg$month == as.Date("2026-09-01")), 1)
  expect_equal(nrow(lg), 2)
})

test_that("live rows are never rewritten", {
  lg <- append_live(as_log_rows(score_rows("2026-08-01"), "backtest", t0), score_rows("2026-09-01", 0.2), t0)
  lg <- append_live(lg, score_rows("2026-09-01", 0.9), t0 + 3600)
  expect_equal(lg$prob[lg$month == as.Date("2026-09-01")], 0.2)
})

test_that("update_actuals touches only the outcome column", {
  lg <- as_log_rows(score_rows(c("2020-01-01", "2020-02-01")), "backtest", t0)
  panel <- data.frame(date = as.Date(c("2020-01-01", "2020-02-01")), rec_next12 = c(1, 0))
  up <- update_actuals(lg, panel)
  expect_equal(up$rec_next12_actual, c(1, 0))
  expect_identical(up[, setdiff(LOG_COLS, "rec_next12_actual")], lg[, setdiff(LOG_COLS, "rec_next12_actual")])
})

test_that("write is atomic and round-trips types", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "predictions.csv")
  lg <- as_log_rows(score_rows(c("2020-01-01", "2020-02-01")), "backtest", t0)
  write_log_atomic(lg, path)
  expect_false(file.exists(paste0(path, ".tmp")))
  back <- read_log(path)
  expect_s3_class(back$month, "Date")
  expect_s3_class(back$train_end, "Date")
  expect_type(back$prob, "double")
  expect_equal(back$prob, lg$prob)
  expect_equal(back$month, lg$month)
})

test_that("read_log returns NULL when absent", {
  expect_null(read_log(file.path(withr::local_tempdir(), "nope.csv")))
})

test_that("version mismatch archives and rebuilds", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "predictions.csv")
  lg <- as_log_rows(score_rows("2020-01-01"), "backtest", t0)
  lg$model_version <- "old"
  write_log_atomic(lg, path)
  expect_true(log_needs_rebuild(read_log(path)))
  expect_false(log_needs_rebuild(as_log_rows(score_rows("2020-01-01"), "backtest", t0)))
  expect_true(log_needs_rebuild(NULL))
  archived <- archive_log(path)
  expect_false(file.exists(path))
  expect_true(file.exists(archived))
  expect_match(basename(archived), "^old_")
  expect_equal(basename(dirname(archived)), "archive")
})
