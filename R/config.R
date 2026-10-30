# ─────────────────────────────────────────────────────────────────────────────
# R/config.R — model specification and paths
#
# Ported from TerraMetric-Atlas R/synopsis_tier3.R (2026-10-30), "Long-history
# subset" spec. Changing anything in the model block means bumping
# MODEL_VERSION: the log is archived and history rebuilt so one line on the
# chart is always one model.
# ─────────────────────────────────────────────────────────────────────────────

# ── Model ────────────────────────────────────────────────────────────────────
MODEL_VERSION  <- "glm-long-v1"
PREDICTORS     <- c("yield_spread", "u_rise", "pay_3m", "real_rate", "prime_lfpr", "sent_chg12")

# Sign of β (effect of a higher value on recession risk) that macro theory
# expects; each fitted β is constrained to it (0 allowed)
EXPECTED_SIGN  <- c(
  yield_spread = -1, # steeper curve → less risk
  u_rise       = +1,
  pay_3m       = -1,
  real_rate    = +1, # tighter policy → more risk
  prime_lfpr   = -1,
  sent_chg12   = -1  # falling sentiment → more risk
)

WINDOW_MONTHS  <- 360L # rolling window of labelled months
EMBARGO_MONTHS <- 12L  # rec_next12 at month t resolves at t + 12
FIRST_FORECAST <- as.Date("1985-01-01")
CV_SEED        <- 20260101L
MIN_TRAIN_ROWS <- 60L
MIN_POSITIVES  <- 6L

# ── Data ─────────────────────────────────────────────────────────────────────
FRED_SERIES <- c(
  USREC = "USREC", T10Y2Y = "T10Y2Y", DGS10 = "DGS10", DGS2 = "DGS2",
  UNRATE = "UNRATE", PAYEMS = "PAYEMS", FEDFUNDS = "FEDFUNDS",
  CPIAUCSL = "CPIAUCSL", LNS11300060 = "LNS11300060", UMCSENT = "UMCSENT"
)
# Indicators and rate series aren't gap-filled; levels with a single missing
# month (e.g. the Oct-2025 shutdown) are
NO_INTERPOLATE <- c("USREC", "T10Y2Y", "DGS10", "DGS2")
FETCH_START    <- as.Date("1959-01-01")

# ── Paths (relative to the repo root) ────────────────────────────────────────
LOG_PATH   <- "log/predictions.csv"
USREC_PATH <- "log/usrec.csv"
