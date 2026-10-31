# ─────────────────────────────────────────────────────────────────────────────
# R/config.R — model specification and paths
#
# Ported from TerraMetric-Atlas R/synopsis_tier3.R (2026-10-30), "Long-history
# subset" spec, plus the U-6 − U-3 gap (v2). Changing anything in the model
# block means bumping MODEL_VERSION: the log is archived and history rebuilt
# so one line on the chart is always one model.
# ─────────────────────────────────────────────────────────────────────────────

# Month arithmetic used throughout (bound here so lubridate needn't be attached)
`%m+%` <- lubridate::`%m+%`
`%m-%` <- lubridate::`%m-%`
n_months <- function(k) lubridate::period(k, "month")

# ── Model ────────────────────────────────────────────────────────────────────
MODEL_VERSION  <- "glm-long-v2"
PREDICTORS     <- c("yield_spread", "u_rise", "gap_rise", "pay_3m", "real_rate", "prime_lfpr", "sent_chg12")

# Predictors whose source series starts after FETCH_START (U6RATE: 1994).
# Before their first value they are NA, which scores as the training median
# (z = 0), so they add nothing until data exist; after it, NA blocks a month.
LATE_START     <- "gap_rise"

# Sign of β (effect of a higher value on recession risk) that macro theory
# expects; each fitted β is constrained to it (0 allowed)
EXPECTED_SIGN  <- c(
  yield_spread = -1, # steeper curve → less risk
  u_rise       = +1,
  gap_rise     = +1, # more involuntary part-time / marginally attached → more risk
  pay_3m       = -1,
  real_rate    = +1, # tighter policy → more risk
  prime_lfpr   = -1,
  sent_chg12   = -1  # falling sentiment → more risk
)

# Fitted parameters logged with every scored month (intercept, β at
# lambda.1se, scaler median/MAD), so each reading can be split into drivers
PARAM_COLS <- c("b0", paste0("b_", PREDICTORS), paste0("med_", PREDICTORS), paste0("mad_", PREDICTORS))

WINDOW_MONTHS  <- 360L # rolling window of labelled months
EMBARGO_MONTHS <- 12L  # rec_next12 at month t resolves at t + 12
FIRST_FORECAST <- as.Date("1985-01-01")
CV_SEED        <- 20260101L
MIN_TRAIN_ROWS <- 60L
MIN_POSITIVES  <- 6L

# ── Data ─────────────────────────────────────────────────────────────────────
FRED_SERIES <- c(
  USREC = "USREC", T10Y2Y = "T10Y2Y", DGS10 = "DGS10", DGS2 = "DGS2",
  UNRATE = "UNRATE", U6RATE = "U6RATE", PAYEMS = "PAYEMS", FEDFUNDS = "FEDFUNDS",
  CPIAUCSL = "CPIAUCSL", LNS11300060 = "LNS11300060", UMCSENT = "UMCSENT"
)
# Indicators and rate series aren't gap-filled; levels with a single missing
# month (e.g. the Oct-2025 shutdown) are
NO_INTERPOLATE <- c("USREC", "T10Y2Y", "DGS10", "DGS2")
FETCH_START    <- as.Date("1959-01-01")

# ── Paths (relative to the repo root) ────────────────────────────────────────
LOG_PATH   <- "log/predictions.csv"
USREC_PATH <- "log/usrec.csv"
