# Synthetic stand-in for fetch_fred() output: monthly series from 1980-01.
# `missing` = list(SERIES_ID = <Date vector>) drops those rows entirely, the
# way a filtered feed would; `usrec_on` = row indices where USREC = 1.
make_raw <- function(n_months = 120, missing = list(), usrec_on = integer(), seed = 1) {
  set.seed(seed)
  dates <- seq(as.Date("1980-01-01"), by = "1 month", length.out = n_months)
  walk <- function(start, sd) start + cumsum(rnorm(n_months, 0, sd))
  usrec <- integer(n_months)
  usrec[usrec_on] <- 1L
  vals <- list(
    USREC       = usrec,
    T10Y2Y      = walk(1, 0.1),
    DGS10       = walk(5, 0.1),
    DGS2        = walk(4, 0.1),
    UNRATE      = pmax(2, walk(6, 0.1)),
    PAYEMS      = 100000 + cumsum(rnorm(n_months, 150, 50)),
    FEDFUNDS    = pmax(0, walk(4, 0.1)),
    CPIAUCSL    = 100 * cumprod(1 + rnorm(n_months, 0.002, 0.001)),
    LNS11300060 = walk(82, 0.05),
    UMCSENT     = pmax(40, walk(85, 2))
  )
  series <- lapply(names(vals), function(id) {
    df <- data.frame(date = dates, value = as.numeric(vals[[id]]))
    if (!is.null(missing[[id]])) df <- df[!df$date %in% missing[[id]], , drop = FALSE]
    df
  })
  names(series) <- names(vals)
  list(
    series = series,
    interpolated = data.frame(series = character(), month = as.Date(character())),
    fetched_at = as.POSIXct("2026-10-30 12:00:00", tz = "UTC")
  )
}
