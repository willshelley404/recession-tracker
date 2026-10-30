# ─────────────────────────────────────────────────────────────────────────────
# R/walkforward.R — out-of-sample predictions, history and monthly
#
# Training uses only labelled rows whose outcome had resolved by the forecast
# date (date ≤ as_of − 12 months). Scoring needs no label, so the history
# runs right up to the latest month with complete predictors.
# ─────────────────────────────────────────────────────────────────────────────

training_rows <- function(panel, as_of) {
  ok <- panel$complete & !is.na(panel$rec_next12) &
    panel$date <= as_of %m-% n_months(EMBARGO_MONTHS)
  tr <- panel[ok, , drop = FALSE]
  utils::tail(tr[order(tr$date), , drop = FALSE], WINDOW_MONTHS)
}

.fit_or_null <- function(train) {
  if (nrow(train) < MIN_TRAIN_ROWS || sum(train$rec_next12) < MIN_POSITIVES) return(NULL)
  fit_glm(train)
}

.as_rows <- function(model, rows) {
  data.frame(
    month = rows$date,
    prob = predict_glm(model, rows),
    train_end = model$train_end,
    n_train = model$n_train,
    train_base_rate = model$base_rate,
    rows[, PREDICTORS, drop = FALSE],
    row.names = NULL
  )
}

.empty_rows <- function() {
  data.frame(
    month = as.Date(character()), prob = numeric(), train_end = as.Date(character()),
    n_train = integer(), train_base_rate = numeric(),
    as.data.frame(sapply(PREDICTORS, function(p) numeric(), simplify = FALSE))
  )
}

#' One out-of-sample prediction for `month` (zero rows if not scorable)
score_month <- function(panel, month) {
  row <- panel[panel$date == month & panel$complete, , drop = FALSE]
  if (nrow(row) == 0) return(.empty_rows())
  model <- .fit_or_null(training_rows(panel, month))
  if (is.null(model)) return(.empty_rows())
  .as_rows(model, row)
}

#' Walk-forward history: refit each January, score every complete month of
#' that year in [first, through]
walk_forward <- function(panel, first = FIRST_FORECAST, through = NULL) {
  scorable <- panel[panel$complete & panel$date >= first, , drop = FALSE]
  if (is.null(through)) through <- max(scorable$date)
  scorable <- scorable[scorable$date <= through, , drop = FALSE]
  years <- sort(unique(as.integer(format(scorable$date, "%Y"))))
  out <- lapply(years, function(yr) {
    rows <- scorable[as.integer(format(scorable$date, "%Y")) == yr, , drop = FALSE]
    model <- .fit_or_null(training_rows(panel, as.Date(sprintf("%d-01-01", yr))))
    if (is.null(model)) return(NULL)
    .as_rows(model, rows)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) .empty_rows() else out
}
