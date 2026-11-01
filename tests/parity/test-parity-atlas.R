# Dev-only parity check: TerraMetric-Atlas's recession model
# (R/synopsis_tier3.R) is the same method, so on the same FRED fetch it must
# reproduce our inputs, walk-forward history, scores and run-up peaks.
# Needs network + FRED key + a TerraMetric-Atlas checkout. Run from repo root:
#   Rscript -e "testthat::test_file('tests/parity/test-parity-atlas.R')"

ATLAS_DIR <- "C:/code_repos/TerraMetric-Atlas"
# test_file() runs with the working directory at tests/parity
root <- normalizePath(file.path(getwd(), "..", ".."))
stopifnot(file.exists(file.path(root, "R", "config.R")))

test_that("TerraMetric-Atlas's recession model matches ours", {
  skip_if_not(dir.exists(ATLAS_DIR), "TerraMetric-Atlas checkout not found")
  skip_if(Sys.getenv("FRED_API_KEY") == "", "FRED_API_KEY not set")

  # Atlas first (its global.R sources into the global env), then ours on top
  # — ours must win for fetch_fred(), which both define. Atlas's model
  # functions have their own names, so none are redefined by ours.
  old <- setwd(ATLAS_DIR)
  suppressMessages(suppressWarnings(source("global.R")))
  setwd(old)
  for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) source(f)
  expect_equal(.GLM_PREDICTORS, PREDICTORS)

  raw <- fetch_fred()
  atlas_series <- .fetch_glm_series()
  for (id in names(FRED_SERIES)) {
    expect_equal(atlas_series[[id]], raw$series[[id]], ignore_attr = TRUE, label = id)
  }

  panel <- build_panel(raw)
  atlas_panel <- build_glm_panel(atlas_series)
  cols <- c("date", "recession", "rec_next12", PREDICTORS, "complete")
  expect_equal(atlas_panel[, cols], panel[, cols], ignore_attr = TRUE)

  ours <- walk_forward(panel)
  atlas <- backtest_recession_glm(atlas_panel)
  a <- atlas$series
  o <- ours[match(a$date, ours$month), ]
  expect_equal(nrow(a), nrow(ours))
  cat(sprintf("\nmonths compared: %d | max |Δprob| = %.2e\n", nrow(a), max(abs(o$prob - a$prob))))
  expect_lte(max(abs(o$prob - a$prob)), 1e-8)

  # Scored unrounded: the log stores probabilities to 6 decimals
  lg <- update_actuals(o, panel)
  m <- backtest_metrics(lg)
  cat(sprintf("skill  ours %.3f  atlas %.3f\nAUC    ours %.3f  atlas %.3f\n", m$skill, atlas$skill, m$auc, atlas$auc))
  for (k in c("brier", "brier_clim", "skill", "auc", "hit_rate", "false_alarm_rate", "n")) {
    expect_equal(atlas[[k]], m[[k]], tolerance = 1e-8, label = k)
  }

  ru <- runup_table(lg, panel[, c("date", "recession")])
  expect_equal(atlas$episodes$recession, ru$recession)
  expect_equal(atlas$episodes$runup_peak, ru$runup_peak, tolerance = 1e-8)
  expect_equal(atlas$episodes$spell_peak, ru$spell_peak, tolerance = 1e-8)

  # Atlas's headline is the latest month under its January refit
  g <- train_recession_glm()
  expect_equal(g$prob, utils::tail(ours$prob, 1), tolerance = 1e-8)
  cat(sprintf("latest %s: ours %.1f%%  atlas %.1f%%\n", format(utils::tail(ours$month, 1), "%b %Y"),
              100 * utils::tail(ours$prob, 1), 100 * g$prob))
})
