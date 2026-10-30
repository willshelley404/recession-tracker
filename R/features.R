# ─────────────────────────────────────────────────────────────────────────────
# R/features.R — one row per month: NBER label + the six predictors
#
# Every series is joined onto the contiguous USREC month grid first, so a
# missing month is an NA row and every lag below is by calendar month, never
# by row offset (a dropped month would otherwise turn YoY into a 13-month
# change for everything after it).
# ─────────────────────────────────────────────────────────────────────────────

.on_grid <- function(raw, id, grid) {
  df <- raw$series[[id]]
  if (is.null(df)) return(rep(NA_real_, length(grid)))
  df$value[match(grid, as.Date(df$date))]
}

# Value of x at (date − k months), matched by date on the grid
.lag_months <- function(x, grid, k) {
  x[match(grid %m-% n_months(k), grid)]
}

build_panel <- function(raw) {
  usrec <- raw$series$USREC
  usrec <- usrec[order(usrec$date), ]
  grid <- seq(min(usrec$date), max(usrec$date), by = "1 month")
  rec <- usrec$value[match(grid, usrec$date)]
  n <- length(grid)

  # rec_next12: recession in any of t+1..t+12; NA with < 6 future months
  rec_next12 <- vapply(seq_len(n), function(i) {
    if (n - i < 6) return(NA_real_)
    as.numeric(any(rec[(i + 1):min(i + 12, n)] == 1, na.rm = TRUE))
  }, numeric(1))

  t10y2y <- .on_grid(raw, "T10Y2Y", grid)
  fallback <- .on_grid(raw, "DGS10", grid) - .on_grid(raw, "DGS2", grid)
  yield_spread <- ifelse(is.na(t10y2y), fallback, t10y2y)

  u <- .on_grid(raw, "UNRATE", grid)
  u_trough <- zoo::rollapply(u, 12, function(w) if (all(is.na(w))) NA_real_ else min(w, na.rm = TRUE),
                             fill = NA, align = "right")
  u_rise <- u - u_trough

  pay <- .on_grid(raw, "PAYEMS", grid)
  pay_chg <- pay - .lag_months(pay, grid, 1)
  pay_3m <- zoo::rollmean(pay_chg, 3, fill = NA, align = "right")

  cpi <- .on_grid(raw, "CPIAUCSL", grid)
  cpi_yoy <- (cpi / .lag_months(cpi, grid, 12) - 1) * 100
  real_rate <- .on_grid(raw, "FEDFUNDS", grid) - cpi_yoy

  sent <- .on_grid(raw, "UMCSENT", grid)
  sent_chg12 <- sent - .lag_months(sent, grid, 12)

  panel <- data.frame(
    date = grid,
    recession = rec,
    rec_next12 = rec_next12,
    yield_spread = yield_spread,
    u_rise = u_rise,
    pay_3m = pay_3m,
    real_rate = real_rate,
    prime_lfpr = .on_grid(raw, "LNS11300060", grid),
    sent_chg12 = sent_chg12
  )
  panel$complete <- stats::complete.cases(panel[, PREDICTORS])
  panel
}
