# ─────────────────────────────────────────────────────────────────────────────
# R/metrics.R — backtest scores, past run-ups, latest reading
# ─────────────────────────────────────────────────────────────────────────────

#' Scores over rows whose outcome is known. Climatology forecast = each row's
#' training base rate; threshold 0.5 for hit / false-alarm rates.
backtest_metrics <- function(log) {
  d <- log[!is.na(log$rec_next12_actual), , drop = FALSE]
  y <- d$rec_next12_actual
  p <- d$prob
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  brier <- mean((p - y)^2)
  brier_clim <- mean((d$train_base_rate - y)^2)
  list(
    brier = brier,
    brier_clim = brier_clim,
    skill = 1 - brier / brier_clim,
    auc = if (n1 > 0 && n0 > 0) (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_,
    hit_rate = if (n1 > 0) mean(p[y == 1] > 0.5) else NA_real_,
    false_alarm_rate = if (n0 > 0) mean(p[y == 0] > 0.5) else NA_real_,
    n = nrow(d)
  )
}

#' USREC == 1 spells starting on/after `from`
recession_spans <- function(usrec, from = FIRST_FORECAST) {
  usrec <- usrec[order(usrec$date), ]
  r <- rle(usrec$recession == 1)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  k <- which(r$values)
  sp <- data.frame(start = usrec$date[starts[k]], end = usrec$date[ends[k]])
  sp[sp$start >= from, , drop = FALSE]
}

.window_stats <- function(log, from, to) {
  p <- log$prob[log$month >= from & log$month <= to]
  if (length(p) == 0) return(c(peak = NA_real_, mean = NA_real_))
  c(peak = max(p), mean = mean(p))
}

#' Per recession: the run-up (12 months before the start) and the
#' TerraMetric-Atlas window (12 months before the start through the month
#' before the end — the rec_next12 = 1 spell, which includes the recession)
runup_table <- function(log, usrec) {
  sp <- recession_spans(usrec)
  do.call(rbind, lapply(seq_len(nrow(sp)), function(i) {
    s <- sp$start[i]
    e <- sp$end[i]
    run <- .window_stats(log, s %m-% n_months(12), s %m-% n_months(1))
    spell <- .window_stats(log, s %m-% n_months(12), e %m-% n_months(1))
    data.frame(
      recession = format(s, "%Y"),
      runup_peak = run[["peak"]], runup_mean = run[["mean"]],
      spell_peak = spell[["peak"]], spell_mean = spell[["mean"]],
      stringsAsFactors = FALSE
    )
  }))
}

#' Latest logged month vs the previous logged month (which may be > 1 month
#' back when a month couldn't be scored)
latest_reading <- function(log) {
  lg <- log[order(log$month), , drop = FALSE]
  n <- nrow(lg)
  list(
    month = lg$month[n],
    prob = lg$prob[n],
    source = lg$source[n],
    prev_month = if (n > 1) lg$month[n - 1] else as.Date(NA),
    prev_prob = if (n > 1) lg$prob[n - 1] else NA_real_,
    change_pts = if (n > 1) (lg$prob[n] - lg$prob[n - 1]) * 100 else NA_real_
  )
}
