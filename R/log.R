# ─────────────────────────────────────────────────────────────────────────────
# R/log.R — the append-only prediction log (single source of truth)
#
# One row per month per model version. Prediction columns are never
# rewritten; only rec_next12_actual (the outcome) is filled in as labels
# resolve. A different model version archives the log and rebuilds history.
# ─────────────────────────────────────────────────────────────────────────────

LOG_COLS <- c(
  "month", "prob", "source", "model_version", "run_at", "train_end", "n_train",
  "train_base_rate", PREDICTORS, "interpolated", "rec_next12_actual"
)

#' Scored rows (from walk_forward/score_month) → log schema. Numbers are
#' rounded to LOG_DIGITS so they survive CSV round trips unchanged (readr
#' re-reads 17-digit doubles a ulp off, which would rewrite every row).
LOG_DIGITS <- 6
as_log_rows <- function(rows, source, run_at) {
  rows[, PREDICTORS] <- lapply(rows[, PREDICTORS, drop = FALSE], round, LOG_DIGITS)
  out <- data.frame(
    month = rows$month,
    prob = round(rows$prob, LOG_DIGITS),
    source = rep(source, nrow(rows)),
    model_version = rep(MODEL_VERSION, nrow(rows)),
    run_at = rep(format(run_at, "%Y-%m-%dT%H:%M:%S%z"), nrow(rows)),
    train_end = rows$train_end,
    n_train = as.integer(rows$n_train),
    train_base_rate = round(rows$train_base_rate, LOG_DIGITS),
    rows[, PREDICTORS, drop = FALSE],
    interpolated = rep("", nrow(rows)),
    rec_next12_actual = rep(NA_real_, nrow(rows)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out[, LOG_COLS]
}

read_log <- function(path = LOG_PATH) {
  tmp <- paste0(path, ".tmp")
  if (!file.exists(path) && file.exists(tmp)) {
    # A write died before its rename. Rebuilding here would silently turn every
    # live row into a backtest row, so stop and let a person decide.
    stop("Found ", tmp, " but no ", basename(path), ": a previous write was interrupted. ",
         "Check the .tmp file and rename it to ", basename(path), " if it is complete.", call. = FALSE)
  }
  if (!file.exists(path)) return(NULL)
  types <- readr::cols(
    month = readr::col_date(), train_end = readr::col_date(), source = readr::col_character(),
    model_version = readr::col_character(), run_at = readr::col_character(),
    n_train = readr::col_integer(), interpolated = readr::col_character(),
    .default = readr::col_double()
  )
  lg <- as.data.frame(readr::read_csv(path, col_types = types, na = c("NA"), progress = FALSE))
  lg$interpolated[is.na(lg$interpolated)] <- ""
  lg[, LOG_COLS]
}

#' Write to <path>.tmp then rename, so an interrupted write can't leave a
#' half-written log in place
write_log_atomic <- function(df, path = LOG_PATH) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  readr::write_csv(df[order(df$month), LOG_COLS], tmp, na = "NA")
  # file.rename replaces an existing target (Windows included); removing the
  # target first would open a window where only the .tmp exists
  if (!file.rename(tmp, path)) stop("Could not move ", tmp, " to ", path)
  invisible(path)
}

log_needs_rebuild <- function(log) {
  is.null(log) || nrow(log) == 0 || any(log$model_version != MODEL_VERSION)
}

archive_log <- function(path = LOG_PATH) {
  old <- read_log(path)
  ver <- if (!is.null(old) && nrow(old)) old$model_version[1] else "unknown"
  dir <- file.path(dirname(path), "archive")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, sprintf("%s_%s.csv", ver, format(Sys.time(), "%Y%m%d-%H%M%S")))
  if (!file.rename(path, dest)) stop("Could not archive ", path)
  dest
}

#' Earlier model versions, from log/archive/<version>_<YYYYmmdd-HHMMSS>.csv
model_history <- function(path = LOG_PATH) {
  files <- list.files(file.path(dirname(path), "archive"), pattern = "\\.csv$")
  m <- regmatches(files, regexec("^(.+)_(\\d{8}-\\d{6})\\.csv$", files))
  m <- m[lengths(m) == 3]
  data.frame(
    version = vapply(m, `[`, "", 2),
    archived_at = as.POSIXct(vapply(m, `[`, "", 3), format = "%Y%m%d-%H%M%S"),
    stringsAsFactors = FALSE
  )
}

#' Run metadata: when the data were fetched (the vintage) and what the run did.
#' Written every run, so the vintage is known even when no row was added.
write_meta <- function(path, fetched_at, status, n_new) {
  readr::write_csv(data.frame(
    fetched_at = format(fetched_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = status, n_new = n_new, model_version = MODEL_VERSION
  ), path)
}

read_meta <- function(path) {
  if (!file.exists(path)) return(NULL)
  m <- utils::read.csv(path, stringsAsFactors = FALSE)
  m$fetched_at <- as.POSIXct(m$fetched_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  as.list(m)
}

#' Add live rows for months not already logged; existing rows are untouched
append_live <- function(log, rows, run_at) {
  new <- rows[!rows$month %in% log$month, , drop = FALSE]
  if (nrow(new) == 0) return(log)
  rbind(log, as_log_rows(new, "live", run_at))
}

#' Fill the outcome column from the panel (the only column that may change)
update_actuals <- function(log, panel) {
  log$rec_next12_actual <- panel$rec_next12[match(log$month, panel$date)]
  log
}
