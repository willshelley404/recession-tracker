# Dev-only parity check: the ported model must reproduce TerraMetric-Atlas's
# "Long-history subset" backtest on the same fetched panel (within 0.01).
# Needs network + FRED key + a TerraMetric-Atlas checkout. Run from repo root:
#   Rscript -e "testthat::test_file('tests/parity/test-parity-atlas.R')"

ATLAS_DIR <- "C:/code_repos/TerraMetric-Atlas"
# test_file() runs with the working directory at tests/parity
root <- normalizePath(file.path(getwd(), "..", ".."))
stopifnot(file.exists(file.path(root, "R", "config.R")))

test_that("port matches TerraMetric-Atlas backtest", {
  skip_if_not(dir.exists(ATLAS_DIR), "TerraMetric-Atlas checkout not found")
  skip_if(Sys.getenv("FRED_API_KEY") == "", "FRED_API_KEY not set")

  # Atlas first (its global.R sources into the global env), then ours on top
  # — ours must win for fetch_fred(), which both define. None of the names
  # Atlas's backtest calls are redefined by ours.
  old <- setwd(ATLAS_DIR)
  suppressMessages(suppressWarnings(source("global.R")))
  setwd(old)
  for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) source(f)

  raw <- fetch_fred()
  panel <- build_panel(raw)
  labelled <- panel[panel$complete & !is.na(panel$rec_next12), ]

  atlas <- backtest_recession_glm(labelled, PREDICTORS, FIRST_FORECAST)
  ours <- walk_forward(panel)

  a <- atlas$series
  o <- ours[match(a$date, ours$month), ]
  expect_false(anyNA(o$prob))
  cat(sprintf("\nmonths compared: %d | max |Δprob| = %.2e\n", nrow(a), max(abs(o$prob - a$prob))))
  expect_lte(max(abs(o$prob - a$prob)), 0.01)

  lg <- as_log_rows(o, "backtest", Sys.time())
  lg$rec_next12_actual <- a$rec_next12
  m <- backtest_metrics(lg)
  cat(sprintf("skill  ours %.3f  atlas %.3f\nAUC    ours %.3f  atlas %.3f\n", m$skill, atlas$skill, m$auc, atlas$auc))
  expect_lte(abs(m$skill - atlas$skill), 0.01)
  expect_lte(abs(m$auc - atlas$auc), 0.01)

  # Per-recession peaks over the rec_next12 = 1 spells (Atlas's definition)
  r <- rle(a$rec_next12); e <- cumsum(r$lengths); s <- e - r$lengths + 1
  for (i in which(r$values == 1)) {
    yr <- format(a$date[s[i]] %m+% n_months(12), "%Y")
    po <- max(o$prob[s[i]:e[i]])
    pa <- atlas$episodes$peak_prob[atlas$episodes$recession == yr]
    cat(sprintf("%s peak  ours %.1f%%  atlas %.1f%%\n", yr, 100 * po, 100 * pa))
    expect_lte(abs(po - pa), 0.01)
  }
})
