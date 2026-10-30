# Recession Tracker Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A standalone R project that logs a walk-forward GLM recession probability for every month from 1985 on, appends one live row per new month, and renders a self-contained HTML report with NBER recessions shaded.

**Architecture:** Small sourced R modules (`R/*.R`) for fetch → features → model → walk-forward → log → metrics, driven by one entry script `run_monthly.R`. The CSV log is the single source of truth. A Quarto (knitr) report reads only the log plus a saved USREC file — it never fetches.

**Tech Stack:** R 4.6, fredr, glmnet, dplyr, tidyr, lubridate, zoo, readr, testthat, plotly, gt, htmltools; Quarto 1.9 (engine: knitr).

**Spec:** `docs/superpowers/specs/2026-10-30-recession-tracker-design.md`

## Global Constraints

- Repo root: `C:\code_repos\recession-tracker\`. No runtime dependency on TerraMetric-Atlas (only the dev-only parity test reads it).
- Predictors (exactly six): `yield_spread`, `u_rise`, `pay_3m`, `real_rate`, `prime_lfpr`, `sent_chg12`.
- Theory signs: yield_spread −, u_rise +, pay_3m −, real_rate +, prime_lfpr −, sent_chg12 −.
- Ridge: `glmnet::cv.glmnet(family="binomial", alpha=0, type.measure="deviance")`, `s="lambda.1se"`, sign limits, `foldid` from class-stratified 5 folds, seed `20260101`, caller RNG restored.
- Scaling: median/MAD from training rows only (MAD floor 1e-6), z clamped to [−3, 3], NA → 0 after scaling.
- Window 360 labelled months; embargo 12 months (date-based); first forecast `1985-01-01`; skip a fit if < 60 rows or < 6 positives.
- `rec_next12` = 1 if any USREC==1 in t+1..t+12; NA when fewer than 6 future USREC months exist.
- All lag-12 / YoY calculations are **date-matched**, never row offsets.
- Single-month NA gaps in level series are log-linearly interpolated; each interpolated (series, month) is recorded.
- `MODEL_VERSION <- "glm-long-v1"`. One row per month per version; live rows never rewritten.
- FRED key from `R_ENVIRON_USER="C:/Users/will.shelley/OneDrive - Avalara/Documents/.Renviron"`; never commit keys.
- Report follows the owner's `.qmd` house style (global CLAUDE.md "Quarto (.qmd) document formatting"), with `engine: knitr` in place of `jupyter: python3` and `gt` for tables.

## Review Focus

1. **FRED fetch fails partway** (network, rate limit, bad key) → the run aborts before touching `log/predictions.csv`; the existing log is byte-identical. *(Task 7 test `fetch failure leaves log untouched`.)*
2. **Run twice in the same month / no new data** → no new rows, no duplicate months, exit 0 with a "nothing new" message. *(Task 5 test `append is idempotent`, Task 7 test `second run adds nothing`.)*
3. **A gap longer than one month** in any input → not interpolated; that month is simply unscorable (no row), no crash. *(Task 1 test `two-month gap is not filled`, Task 4 test `incomplete months are skipped`.)*
4. **Log written by a different model version, or interrupted mid-write** → archived and rebuilt, never mixed; writes go to a temp file then rename. *(Task 5 tests `version mismatch archives and rebuilds`, `write is atomic`.)*
5. **`FRED_API_KEY` missing** → fails immediately with a message naming `R_ENVIRON_USER`, before any fetch. *(Task 1 test `missing key gives actionable error`.)*

---

### Task 1: Project scaffold, config, FRED fetch with gap interpolation

**Files:**
- Create: `R/config.R`, `R/fetch.R`, `tests/testthat.R`, `tests/testthat/helper-source.R`, `tests/testthat/test-fetch.R`, `README.md`

**Interfaces:**
- Produces:
  - `config.R` constants: `PREDICTORS` (chr, the six names in the order above), `EXPECTED_SIGN` (named num), `WINDOW_MONTHS = 360L`, `EMBARGO_MONTHS = 12L`, `FIRST_FORECAST = as.Date("1985-01-01")`, `MODEL_VERSION = "glm-long-v1"`, `CV_SEED = 20260101L`, `LOG_PATH = "log/predictions.csv"`, `USREC_PATH = "log/usrec.csv"`, `FRED_SERIES = c(USREC="USREC", T10Y2Y="T10Y2Y", DGS10="DGS10", DGS2="DGS2", UNRATE="UNRATE", PAYEMS="PAYEMS", FEDFUNDS="FEDFUNDS", CPIAUCSL="CPIAUCSL", LNS11300060="LNS11300060", UMCSENT="UMCSENT")`.
  - `require_fred_key() -> invisible(TRUE)`; errors with a message containing `"R_ENVIRON_USER"` when `Sys.getenv("FRED_API_KEY") == ""`.
  - `interpolate_single_gaps(df: data.frame(date, value)) -> list(data = df_filled, filled = Date vector)`; fills an NA only when both neighbours (by date) are non-NA; log-linear for positive series, linear otherwise.
  - `fetch_fred(start = as.Date("1959-01-01")) -> list(series = named list of data.frame(date, value), interpolated = data.frame(series, month))`. Uses `fredr::fredr(id, observation_start=start, frequency="m")`; level series (all except USREC, T10Y2Y, DGS10, DGS2) pass through `interpolate_single_gaps`. Any fetch error propagates (no partial result).
- `tests/testthat/helper-source.R` sources every `R/*.R` relative to the repo root.

- [ ] **Step 1: Write failing tests** in `test-fetch.R`:
  - `interpolate fills a single gap log-linearly`: values `c(100, NA, 121)` monthly → filled middle `== 110` (tolerance 1e-9), `filled` has length 1 with that date.
  - `two-month gap is not filled`: `c(1, NA, NA, 4)` → both stay NA, `filled` empty.
  - `edge NA is not filled`: `c(NA, 2, 3)` → first stays NA.
  - `missing key gives actionable error`: with `withr::local_envvar(FRED_API_KEY = "")` (or `Sys.setenv` + `on.exit`), `expect_error(require_fred_key(), "R_ENVIRON_USER")`.
- [ ] **Step 2: Run** `Rscript -e "testthat::test_dir('tests/testthat')"` — expect FAIL (functions not defined).
- [ ] **Step 3: Implement** `R/config.R` and `R/fetch.R` per Interfaces. `README.md`: purpose, `R_ENVIRON_USER` export line, `Rscript run_monthly.R [--rebuild]`, where outputs land.
- [ ] **Step 4: Run tests** — expect PASS.
- [ ] **Step 5: Commit** `feat: config, FRED fetch, single-gap interpolation`.

### Task 2: Feature panel

**Files:** Create `R/features.R`, `tests/testthat/test-features.R`

**Interfaces:**
- Consumes: `fetch_fred()` output shape.
- Produces: `build_panel(raw: list from fetch_fred) -> data.frame` with one row per month (contiguous, every month from first USREC month), columns `date, recession, rec_next12, yield_spread, u_rise, pay_3m, real_rate, prime_lfpr, sent_chg12, complete` (`complete` = all six predictors non-NA).
  - `yield_spread` = T10Y2Y, falling back to DGS10 − DGS2 for months where T10Y2Y is NA.
  - `u_rise` = UNRATE − rolling 12-month min with `na.rm = TRUE` (right-aligned).
  - `pay_3m` = mean of the last 3 monthly changes in PAYEMS.
  - `real_rate` = FEDFUNDS − CPI YoY, CPI YoY date-matched (`value / value at date − 12 months − 1) × 100`).
  - `sent_chg12` = UMCSENT − UMCSENT at date − 12 months (date-matched).
  - `rec_next12` per Global Constraints.

- [ ] **Step 1: Write failing tests** using a synthetic `raw` list (helper `make_raw(n_months, missing = list())`):
  - `yoy is date-matched across a missing month`: drop one CPI month mid-series; the YoY 13 months after it equals `(cpi[t]/cpi[t-12]-1)*100` by date, not by row.
  - `rec_next12 looks forward 12 months`: USREC = 1 only at month 30 → `rec_next12` is 1 for months 18..29, 0 elsewhere where defined, NA for the last 5 months.
  - `u_rise ignores a missing month`: one NA UNRATE → `u_rise` is non-NA for the following 11 months.
  - `yield_spread falls back to DGS10-DGS2` when T10Y2Y is NA.
  - `complete flags rows with all six predictors`.
- [ ] **Step 2: Run** tests — expect FAIL.
- [ ] **Step 3: Implement** `build_panel()`.
- [ ] **Step 4: Run** tests — expect PASS.
- [ ] **Step 5: Commit** `feat: monthly feature panel with date-matched lags`.

### Task 3: Model primitives

**Files:** Create `R/model.R`, `tests/testthat/test-model.R`

**Interfaces:**
- Produces:
  - `build_scaler(df, cols) -> list(center, scale, cols)`; `apply_scaler(df, scaler) -> df` (z clamp [−3,3], NA→0).
  - `sign_limits(preds) -> list(lower, upper)` from `EXPECTED_SIGN`.
  - `cv_folds(y, k = 5L) -> integer` — class-stratified, `set.seed(CV_SEED)`, restores `.Random.seed` (or removes it if absent before).
  - `fit_glm(train: data.frame with rec_next12 + PREDICTORS) -> list(fit = cv.glmnet, scaler, base_rate = mean(y), n_train = nrow, train_end = max(train$date))`.
  - `predict_glm(model, newdata) -> numeric` probabilities at `lambda.1se`.

- [ ] **Step 1: Write failing tests:**
  - `scaler clamps and imputes`: an extreme value maps to 3; NA maps to 0.
  - `fit respects theory signs`: synthetic data where `prime_lfpr` truly *raises* risk (wrong sign) → its fitted coef `== 0`; and for every predictor, coef `<= 0` when `EXPECTED_SIGN < 0` and `>= 0` when `> 0`.
  - `fit is deterministic`: two `fit_glm()` calls on the same data give identical `lambda.1se` and predictions.
  - `cv_folds restores RNG`: `set.seed(1); a <- runif(1); set.seed(1); cv_folds(rep(0:1, 50)); b <- runif(1); expect_equal(a, b)`.
  - `every fold has a positive`: `cv_folds(c(rep(0,95), rep(1,5)))` → each of 1..5 appears among positives.
- [ ] **Step 2: Run** — FAIL. **Step 3: Implement.** **Step 4: Run** — PASS.
- [ ] **Step 5: Commit** `feat: sign-constrained ridge with fixed folds`.

### Task 4: Walk-forward history and single-month scoring

**Files:** Create `R/walkforward.R`, `tests/testthat/test-walkforward.R`

**Interfaces:**
- Consumes: `build_panel()` columns; `fit_glm()`, `predict_glm()`.
- Produces:
  - `training_rows(panel, as_of: Date) -> data.frame` — rows with `complete`, non-NA `rec_next12`, `date <= as_of %m-% months(EMBARGO_MONTHS)`; the last `WINDOW_MONTHS` of them.
  - `score_month(panel, month: Date) -> data.frame` (one row or zero rows) with columns `month, prob, train_end, n_train, train_base_rate` plus the six predictor values; zero rows if the month is not `complete` or training fails the skip rule (< 60 rows or < 6 positives).
  - `walk_forward(panel, first = FIRST_FORECAST, through = NULL) -> data.frame` (same columns) — for each year, one fit on `training_rows(panel, as.Date(paste0(year,"-01-01")))`, scoring every `complete` month of that year from `first` up to `through` (default: last complete month), **including months with NA `rec_next12`**.

- [ ] **Step 1: Write failing tests** (synthetic panel with a few recessions):
  - `no training row is within the embargo`: for every January fit, `max(train$date) <= jan - 12 months`.
  - `scores the unlabelled tail`: last 5 months have NA `rec_next12` yet appear in `walk_forward()` output.
  - `incomplete months are skipped`: a month with NA `sent_chg12` has no row.
  - `skip rule`: a panel with only 3 positive labels before 1990 produces no rows for 1985–1989.
  - `score_month matches walk_forward in January`: `score_month(panel, jan)` prob equals the `walk_forward` prob for that January.
- [ ] **Step 2: Run** — FAIL. **Step 3: Implement.** **Step 4: Run** — PASS.
- [ ] **Step 5: Commit** `feat: walk-forward history and monthly scoring`.

### Task 5: Prediction log

**Files:** Create `R/log.R`, `tests/testthat/test-log.R`

**Interfaces:**
- Produces (all take `path = LOG_PATH`):
  - Log columns, in order: `month, prob, source, model_version, run_at, train_end, n_train, train_base_rate, yield_spread, u_rise, pay_3m, real_rate, prime_lfpr, sent_chg12, interpolated, rec_next12_actual`. `interpolated` is a `;`-joined list of series interpolated for that month (or empty).
  - `read_log(path) -> data.frame | NULL` (NULL if absent).
  - `write_log_atomic(df, path)` — write to `paste0(path, ".tmp")` then `file.rename`.
  - `log_needs_rebuild(log) -> logical` — TRUE if NULL, empty, or any `model_version != MODEL_VERSION`.
  - `archive_log(path) -> archived_path` — moves to `log/archive/<old_version>_<YYYYmmdd-HHMMSS>.csv`.
  - `append_live(log, rows, run_at) -> data.frame` — adds rows whose `month` isn't already present; never modifies existing rows' prediction columns.
  - `update_actuals(log, panel) -> data.frame` — sets `rec_next12_actual` from `panel` by month (only this column may change).

- [ ] **Step 1: Write failing tests:**
  - `append is idempotent`: appending the same month twice leaves one row.
  - `live rows are never rewritten`: appending a row for an existing month with a different `prob` leaves the original `prob`.
  - `update_actuals touches only the outcome column`: all other columns identical before/after.
  - `version mismatch archives and rebuilds`: a log with `model_version = "old"` → `log_needs_rebuild` TRUE; `archive_log` leaves no file at `path` and one under `log/archive/` whose name starts with `old_`.
  - `write is atomic`: after `write_log_atomic`, no `.tmp` file remains and contents round-trip via `read_log` (Date and numeric types preserved).
- [ ] **Step 2: Run** — FAIL. **Step 3: Implement.** **Step 4: Run** — PASS.
- [ ] **Step 5: Commit** `feat: append-only prediction log with archive`.

### Task 6: Metrics and run-up table

**Files:** Create `R/metrics.R`, `tests/testthat/test-metrics.R`

**Interfaces:**
- Consumes: log columns; USREC data.frame(date, recession).
- Produces:
  - `backtest_metrics(log) -> list(brier, brier_clim, skill, auc, hit_rate, false_alarm_rate, n)` over rows with non-NA `rec_next12_actual`; climatology forecast = `train_base_rate`; threshold 0.5.
  - `recession_spans(usrec, from = FIRST_FORECAST) -> data.frame(start, end)` — USREC==1 spells starting on/after `from`.
  - `runup_table(log, usrec) -> data.frame(recession, runup_peak, runup_mean, spell_peak, spell_mean)`:
    - `recession` = `format(start, "%Y")`;
    - run-up window = the 12 months `start − 12 … start − 1`;
    - spell window = months `start − 12 … end − 1` (the `rec_next12 = 1` spell, TerraMetric-Atlas definition).
  - `latest_reading(log) -> list(month, prob, prev_month, prev_prob, change_pts)`; `prev_*` = the previous logged month (may be > 1 month back if a month was unscorable).

- [ ] **Step 1: Write failing tests** with a toy log and USREC:
  - `auc is 1 for perfect ranking` and `skill is 0 when prob equals base rate`.
  - `runup and spell windows differ`: a toy where probs rise only during the recession → `spell_peak > runup_peak`.
  - `latest_reading skips an unscorable month`: log has Aug and Oct only → `prev_month` is Aug, `change_pts` = (Oct − Aug) × 100.
- [ ] **Step 2: Run** — FAIL. **Step 3: Implement.** **Step 4: Run** — PASS.
- [ ] **Step 5: Commit** `feat: backtest metrics and run-up table`.

### Task 7: Monthly entry script

**Files:** Create `run_monthly.R`, `R/pipeline.R`, `tests/testthat/test-pipeline.R`

**Interfaces:**
- Consumes: everything above.
- Produces: `run_pipeline(fetch = fetch_fred, rebuild = FALSE, render = TRUE, log_path = LOG_PATH, usrec_path = USREC_PATH, now = Sys.time()) -> list(status = "rebuilt"|"appended"|"unchanged", n_new)`. Order:
  1. `require_fred_key()` (skip when `fetch` is not the default, for tests);
  2. `raw <- fetch()` — any error stops here, before any file is written;
  3. `panel <- build_panel(raw)`;
  4. if `rebuild || log_needs_rebuild(read_log())`: archive the existing log if present, `walk_forward()` → rows with `source = "backtest"`, status `"rebuilt"`;
     else: score every complete month after the last logged month through the last complete month with `score_month()`, `source = "live"`; status `"appended"` or `"unchanged"`;
  5. fill `interpolated` from `raw$interpolated`, `update_actuals()`, `write_log_atomic()`, write `usrec.csv`;
  6. if `render`: `system2("quarto", c("render", "report/recession_tracker.qmd"))`, stop on non-zero exit.
- `run_monthly.R`: parses `--rebuild` and `--no-render`, sources `R/*.R`, calls `run_pipeline()`, prints status and the latest reading.

- [ ] **Step 1: Write failing tests** with a stub `fetch` returning synthetic `raw` (from Task 2's helper) and a temp dir:
  - `first run rebuilds`: status `"rebuilt"`, every row `source == "backtest"`.
  - `second run adds nothing`: same stub → status `"unchanged"`, log identical.
  - `new month is appended as live`: stub with one extra month → exactly one new `live` row.
  - `fetch failure leaves log untouched`: stub that `stop()`s → error, log file bytes unchanged (`tools::md5sum`).
- [ ] **Step 2: Run** — FAIL. **Step 3: Implement.** **Step 4: Run** — PASS.
- [ ] **Step 5: Commit** `feat: monthly pipeline and entry script`.

### Task 8: Parity check against TerraMetric-Atlas, then first real run

**Files:** Create `tests/parity/test-parity-atlas.R` (not under `tests/testthat`, so it never runs by default)

**Interfaces:**
- Consumes: `fetch_fred()`, `build_panel()`, `walk_forward()`, `backtest_metrics()`; TerraMetric-Atlas `R/synopsis_tier3.R::backtest_recession_glm` (sourced via `setwd("C:/code_repos/TerraMetric-Atlas"); source("global.R")` inside a `local()` with the working directory restored).

- [ ] **Step 1: Write the parity test:** fetch once; build our panel; run Atlas's `backtest_recession_glm(panel_labelled, PREDICTORS, FIRST_FORECAST)` where `panel_labelled` = our panel filtered to `complete & !is.na(rec_next12)`; run our `walk_forward(panel)`; restrict ours to the months Atlas scored. Assert `|skill_ours − skill_atlas| <= 0.01`, `|auc_ours − auc_atlas| <= 0.01`, and for each episode `|peak_ours − peak_atlas| <= 0.01`. Print both sets so the owner can compare against the reference (skill ≈ 0.32, AUC ≈ 0.84, peaks ≈ 67/60/68/74%).
- [ ] **Step 2: Run** with `export R_ENVIRON_USER=...` then `Rscript -e "testthat::test_file('tests/parity/test-parity-atlas.R')"` — expect PASS. If it fails, fix the port, not the tolerance.
- [ ] **Step 3: First real run:** `Rscript run_monthly.R --rebuild --no-render` → status `rebuilt`; `log/predictions.csv` starts 1985-01 and ends at the last month with all six predictors; `interpolated` names CPIAUCSL/UNRATE/LNS11300060 for 2025-10 if those were filled.
- [ ] **Step 4: Commit** the parity test and the generated `log/` files: `feat: parity check and initial backtest log`.

### Task 9: Quarto report

**Files:** Create `report/recession_tracker.qmd`, `report/_helpers.R`

**Interfaces:**
- Consumes: `read_log()`, `backtest_metrics()`, `runup_table()`, `latest_reading()`, `log/usrec.csv`. Param `log_path` (default `"../log/predictions.csv"`), `usrec_path` (default `"../log/usrec.csv"`).
- Produces: `report/recession_tracker.html` (self-contained).

- [ ] **Step 1: Write the report** in house style: frontmatter shape from the global CLAUDE.md with `title: "Recession Tracker"`, `subtitle: "GLM 12-month recession probability · walk-forward since 1985"`, `engine: knitr`; leading HTML comment (purpose, params, render command `quarto render report/recession_tracker.qmd`, prerequisite `Rscript run_monthly.R`); params cell; setup cell (palette navy `#1B2A4A`, orange `#FC6600`, teal `#2EC4B6`, signals `#6c757d/#198754/#dc3545/#d97706`; `kpi_card()`, `callout()`; one Plotly layout helper). Sections:
  - `# 1 — Latest reading`: KPI row — probability, change vs previous logged month (pts, coloured), as-of month, data fetched date.
  - `# 2 — History`: Plotly line of `prob × 100` vs `month`; NBER recessions as `layout(shapes = rect, xref="x", yref="paper", fillcolor grey, opacity 0.25)`; backtest as a navy line, live rows as orange markers; dashed 50% line; hover shows month, prob, source.
  - `# 3 — Compared with past run-ups`: `gt` table from `runup_table()` with `tab_spanner("Run-up · 12 months before", c(runup_peak, runup_mean))` and `tab_spanner("TerraMetric-Atlas window · incl. recession", c(spell_peak, spell_mean))`, percentages, plus a bold **Today** row; note below explaining why the second spanner is higher.
  - `# 4 — Model notes`: spec in one callout; `backtest_metrics()` KPIs; caveats callout (revised data, NBER lag ignored, interpolated months listed from the log, GLM alone ≠ TerraMetric-Atlas headline).
  - Glossary `<details>`: Brier skill, AUC, walk-forward, embargo, rec_next12, NBER.
- [ ] **Step 2: Render:** `quarto render report/recession_tracker.qmd` — expect exit 0 and `report/recession_tracker.html` present.
- [ ] **Step 3: Verify in a browser** (Playwright over `python -m http.server`): the chart shows four shaded bands after 1985, the latest point is labelled, the table has five rows (four recessions + Today).
- [ ] **Step 4: Full pipeline:** `Rscript run_monthly.R` → status `unchanged` (no new month since Task 8), report re-rendered.
- [ ] **Step 5: Commit** `feat: Quarto recession tracker report`, including `report/recession_tracker.html` (it is the deliverable); `report/*_files/` stays ignored.
