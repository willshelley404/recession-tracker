# ─────────────────────────────────────────────────────────────────────────────
# R/fetch.R — FRED download + single-month gap filling
# ─────────────────────────────────────────────────────────────────────────────

require_fred_key <- function() {
  if (!nzchar(Sys.getenv("FRED_API_KEY"))) {
    stop(
      "FRED_API_KEY is not set. Point R at your .Renviron first, e.g.\n",
      "  export R_ENVIRON_USER=\"C:/Users/will.shelley/OneDrive - Avalara/Documents/.Renviron\"",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Fill an NA only when both date-neighbours are present: log-linear for a
#' strictly positive series, linear otherwise. Longer gaps and edges stay NA.
interpolate_single_gaps <- function(df) {
  df <- df[order(df$date), , drop = FALSE]
  v <- df$value
  n <- length(v)
  filled <- df$date[0]
  if (n >= 3) {
    positive <- all(v > 0, na.rm = TRUE)
    for (i in 2:(n - 1)) {
      if (is.na(v[i]) && !is.na(v[i - 1]) && !is.na(v[i + 1])) {
        v[i] <- if (positive) sqrt(v[i - 1] * v[i + 1]) else (v[i - 1] + v[i + 1]) / 2
        filled <- c(filled, df$date[i])
      }
    }
  }
  df$value <- v
  list(data = df, filled = filled)
}

#' Download every series monthly. Any error propagates — callers must not
#' write anything from a partial fetch.
fetch_fred <- function(start = FETCH_START) {
  require_fred_key()
  fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))
  series <- list()
  interp <- data.frame(series = character(), month = as.Date(character()))
  for (id in names(FRED_SERIES)) {
    raw <- fredr::fredr(FRED_SERIES[[id]], observation_start = start, frequency = "m")
    df <- data.frame(date = as.Date(raw$date), value = as.numeric(raw$value))
    if (!id %in% NO_INTERPOLATE) {
      out <- interpolate_single_gaps(df)
      df <- out$data
      if (length(out$filled)) {
        interp <- rbind(interp, data.frame(series = id, month = out$filled))
      }
    }
    series[[id]] <- df
  }
  list(series = series, interpolated = interp, fetched_at = Sys.time())
}
