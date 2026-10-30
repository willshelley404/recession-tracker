# ─────────────────────────────────────────────────────────────────────────────
# R/pipeline.R — one monthly run: fetch → panel → log → (render)
#
# Nothing is written until the fetch and all scoring have succeeded.
# ─────────────────────────────────────────────────────────────────────────────

# Series whose 12-month lag feeds a predictor: an interpolated base month also
# touches the month a year later
.LAG12_SERIES <- c("CPIAUCSL", "UMCSENT")

.interpolated_tags <- function(months, interp) {
  if (is.null(interp) || nrow(interp) == 0) return(rep("", length(months)))
  hits <- rbind(
    interp,
    transform(interp[interp$series %in% .LAG12_SERIES, , drop = FALSE], month = month %m+% n_months(12))
  )
  vapply(months, function(m) paste(sort(unique(hits$series[hits$month == m])), collapse = ";"), character(1))
}

run_pipeline <- function(fetch = fetch_fred, rebuild = FALSE, render = TRUE,
                         log_path = LOG_PATH, usrec_path = USREC_PATH, now = Sys.time()) {
  raw <- fetch()
  panel <- build_panel(raw)
  log <- read_log(log_path)

  if (rebuild || log_needs_rebuild(log)) {
    rows <- walk_forward(panel)
    new_log <- as_log_rows(rows, "backtest", now)
    new_log$interpolated <- .interpolated_tags(new_log$month, raw$interpolated)
    status <- "rebuilt"
    n_new <- nrow(new_log)
  } else {
    pending <- panel$date[panel$complete & panel$date > max(log$month)]
    rows <- do.call(rbind, c(list(.empty_rows()), lapply(pending, function(m) score_month(panel, m))))
    new_log <- append_live(log, rows, now)
    added <- !new_log$month %in% log$month
    new_log$interpolated[added] <- .interpolated_tags(new_log$month[added], raw$interpolated)
    n_new <- sum(added)
    status <- if (n_new > 0) "appended" else "unchanged"
  }
  new_log <- update_actuals(new_log, panel)

  # All computation succeeded — now write
  if (status == "rebuilt" && file.exists(log_path)) archive_log(log_path)
  write_log_atomic(new_log, log_path)
  dir.create(dirname(usrec_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(panel[, c("date", "recession")], usrec_path, na = "NA")

  if (render) {
    code <- system2("quarto", c("render", "report/recession_tracker.qmd"))
    if (code != 0) stop("quarto render failed (exit ", code, ")")
  }
  list(status = status, n_new = n_new, log = new_log)
}
