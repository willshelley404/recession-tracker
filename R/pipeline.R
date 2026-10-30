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
                         log_path = LOG_PATH, usrec_path = USREC_PATH, now = Sys.time(),
                         meta_path = file.path(dirname(log_path), "meta.csv")) {
  raw <- fetch()
  panel <- build_panel(raw)
  log <- read_log(log_path)

  if (rebuild || log_needs_rebuild(log)) {
    # Same model version: live rows are never rewritten, so only the backtest
    # before the first live month is regenerated. A version change archives
    # everything and starts over.
    same_version <- !is.null(log) && nrow(log) > 0 && !any(log$model_version != MODEL_VERSION)
    keep_live <- if (same_version && any(log$source == "live")) log[log$source == "live", , drop = FALSE]
    through <- if (!is.null(keep_live)) min(keep_live$month) %m-% n_months(1)
    backtest <- as_log_rows(walk_forward(panel, through = through), "backtest", now)
    backtest$interpolated <- .interpolated_tags(backtest$month, raw$interpolated)

    # A fetch that "succeeds" with missing or truncated series must not
    # replace good history
    if (nrow(backtest) == 0) {
      stop("refusing to rebuild: no month has all six predictors (check the fetched series)", call. = FALSE)
    }
    new_end <- max(c(backtest$month, keep_live$month))
    if (!is.null(log) && nrow(log) > 0 && new_end < max(log$month)) {
      stop(sprintf("refusing to rebuild: new history ends %s, before the existing log's %s",
                   format(new_end), format(max(log$month))), call. = FALSE)
    }
    new_log <- rbind(backtest, keep_live)
    status <- "rebuilt"
    n_new <- nrow(backtest)
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
  write_meta(meta_path, raw$fetched_at, status, n_new)

  if (render) {
    code <- system2("quarto", c("render", "report/recession_tracker.qmd"))
    if (code != 0) stop("quarto render failed (exit ", code, ")")
  }
  list(status = status, n_new = n_new, log = new_log)
}
