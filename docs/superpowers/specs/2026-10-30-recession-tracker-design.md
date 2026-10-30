# Recession Tracker — Design

**Date:** 2026-10-30
**Location:** `C:\code_repos\recession-tracker\` (standalone; sibling of `TerraMetric-Atlas`)

## Intent

A monthly record of one recession model's 12-month-ahead recession probability, for
history and going forward, so the owner can answer two questions each month:

1. **Did the latest reading move?** — this month's value vs. last month's.
2. **How does today compare with the run-ups to past recessions?** — today's value
   alongside what the same model said in the year before 1990, 2001, 2008 and 2020.

It is not an app. It is a script that appends to a log and renders one self-contained
HTML report. "Deployable someday" means the same script can later run on a schedule
(Task Scheduler, Posit Connect scheduled render, or GitHub Actions) with no server.

## Goals

- A **line chart** of monthly recession probability from 1985 to the latest month, with
  **NBER recessions shaded** (USREC = 1).
- A **CSV log** that grows by one row per month and is the single source of truth for
  the chart.
- **Every point comes from the same model**, trained only on information available at
  the time (walk-forward), so history and live readings are comparable.
- A **latest-reading header** (value + change vs. prior month) and a **past run-ups
  table** (peak and mean probability in the 12 months before each recession start, with
  today's value alongside).
- Runs standalone: no dependency on TerraMetric-Atlas code at runtime.

## Non-goals (for now)

- No Shiny app, no server, no dashboard hosting.
- No hand-tuned prior, Markov model or anomaly adjustment — GLM only.
- No real-time data vintages (ALFRED). History uses today's revised FRED data; the report
  says so. True real-time reconstruction is a possible later project.
- No automatic scheduling in v1 — the owner runs it by hand; the design keeps it
  schedulable.
- No column for TerraMetric-Atlas's blended headline in v1 *(builder's call — see
  "Decided by the designer")*. The owner wants to see later "how close it approximates";
  that comparison is a follow-up.

## Anti-goals (would be a failure even if it runs)

- **History that silently changes.** Once a `live` row is logged it is never rewritten
  by a later run. (Backtest rows may be regenerated only by an explicit rebuild.)
- **Mixing models on one line.** Backtest and live points must come from the identical
  specification; if the spec changes, history is rebuilt and the change is noted.
- **Hindsight in the history.** No backtest prediction may use a label that wasn't
  resolved at its forecast date.
- **Non-reproducible numbers.** Same data in → same numbers out (fixed CV folds).

## The model (decided)

Ported from `TerraMetric-Atlas/R/synopsis_tier3.R` as of 2026-10-30. Behavior must match
the "Long-history subset" row of TerraMetric-Atlas's Rolling Backtest panel (Brier skill
≈ 0.32, AUC ≈ 0.84, 5% false alarms; peaks 1990 ≈ 67%, 2001 ≈ 60%, 2008 ≈ 68%,
2020 ≈ 74%) when run on the same data — that is the acceptance check for the port.

- **Target:** `rec_next12` = 1 if any USREC month falls in months t+1..t+12 (NA when fewer
  than 6 future months exist).
- **Predictors (6):** `yield_spread` (T10Y2Y, fallback DGS10−DGS2), `u_rise` (UNRATE minus
  its trailing-12-month min, **na.rm** so the Oct-2025 gap doesn't blank 12 months),
  `pay_3m` (3-month mean of PAYEMS monthly change), `real_rate` (FEDFUNDS − CPIAUCSL YoY),
  `prime_lfpr` (LNS11300060), `sent_chg12` (UMCSENT minus its value 12 months earlier,
  **date-matched**).
- **All YoY/lag-12 calculations are date-matched**, never row-offset (FRED returns the
  missing Oct-2025 month as an NA row via fredr, but any filtered series must not shift).
- **Scaling:** robust median/MAD from the training rows only, clamp to [−3, 3], NA → 0
  after scaling.
- **Fit:** `glmnet::cv.glmnet`, binomial, alpha = 0 (Ridge), `lambda.1se`, each β
  constrained to its theory sign (yield_spread −, u_rise +, pay_3m −, real_rate +,
  prime_lfpr −, sent_chg12 −), fixed class-stratified 5-fold `foldid` with seed
  20260101 that restores the caller's RNG.
- **Window:** rolling 360 months of resolved labels.
- **Walk-forward (backtest):** forecasts from 1985-01. Each January, refit on rows whose
  label had resolved by then (row index ≤ i0 − 12), predict that January through December.
  Skip a refit if the training window has fewer than 60 rows or 6 positive labels.
- **Predictions don't need labels** *(designer's call)*. TerraMetric-Atlas drops rows with
  NA `rec_next12` before scoring; here, **training** uses only labelled rows but
  **scoring** covers every month with complete predictors. That way the backtest runs
  right up to the first live month, with no gap.
- **"Resolved" ignores NBER's announcement lag** *(designer's call, for parity with
  TerraMetric-Atlas)*. A label counts as known 12 months after its row, using NBER dates
  as finalised today. In reality NBER dates turning points 6–12+ months late, so the
  history is somewhat flattered. The report states this next to the revised-data caveat.
  A strict real-time embargo is a possible later project.
- **CPI gap** *(designer's call)*. CPIAUCSL has no Oct-2025 value (shutdown). Fill that one
  level by log-linear interpolation between Sep and Nov 2025 before computing YoY, so
  `real_rate` exists for Oct 2025 and Oct 2026. The same rule applies to any future
  single-month gap in a level series; the report lists each interpolated month.
- **Acceptance check** *(designer's call)*:
  - **What it compares:** a dev-only test that sources TerraMetric-Atlas's
    `backtest_recession_glm` and runs it, and the port, on the **same fetched panel**.
  - **Pass condition:** Brier skill, AUC and per-recession peaks (using TerraMetric-Atlas's
    spell definition) agree to within 0.01. The only allowed differences are the months
    that TerraMetric-Atlas never scores (its unlabelled tail and the interpolated CPI
    months).
  - **The quoted figures:** the numbers above are a sanity reference only, since they come
    from an earlier FRED pull.

## Monthly run (decided in conversation; details marked)

- **First run / rebuild:** compute the full walk-forward history and write it to the log
  with `source = backtest`.
- **Each later run:**
  - **Which months:** fetch FRED and find the **latest month for which all six predictors
    are available** *(designer's call)*. Every month after the last logged one, up to that
    month, gets one `source = live` row.
  - **How each is scored:** refit on labels resolved as of that month (same 360-month
    window, same 12-month embargo).
  - **Missed runs** *(designer's call)*: missed runs and multi-month data drops are
    backfilled this way, with `run_at` showing they were logged late.
  - **Months already logged** stay untouched.
- **One row per month per model version** *(designer's call)*. Rows are never rewritten
  within a version.
  - **Spec change:** the current log moves to `log/archive/<model_version>.csv`, history
    is rebuilt under the new version, and the report notes the change and its date.
  - **Old live rows:** they are kept in the archive and not shown on the main line, so one
    line is always one model.
  - **Outcome column:** `rec_next12_actual` is an outcome, not a prediction, so it may be
    filled in as labels resolve.
- **Rendering:** after updating the log, render the Quarto report to a single
  self-contained HTML file.

## Log (CSV) — designer's call on exact columns

One row per forecast month. Suggested columns: `month` (first of month), `prob`,
`source` (`backtest`|`live`), `run_at` (timestamp), `model_version`, `train_end`
(last resolved label month used), the six predictor values used, and
`rec_next12_actual` (filled in later as labels resolve; may be updated — it's the
outcome, not the prediction).

## Report (Quarto HTML)

Follows the owner's house style for `.qmd` documents (frontmatter shape, leading HTML
comment with purpose/render command, params cell, setup cell with palette + helpers,
`# N — Title` sections, collapsible glossary, navy/orange/teal palette, KPI cards,
callouts, `gt` tables). Engine is R/knitr *(designer's call: the model code is R)*.

Contents, at minimum:

1. KPI row: latest probability, change vs. prior month, as-of month, data vintage date.
2. Line chart (Plotly): probability 1985→latest; NBER recessions as shaded vertical
   bands; backtest vs. live points visually distinct; 50% reference line.
3. Past run-ups table (owner decided: **both windows, side by side**), with one row each
   for 1990, 2001, 2008 and 2020, and today's value alongside:
   - **Run-up (12 months before the recession):** peak and mean probability in the
     12 months before the recession's first month. This is what the model said while
     there was still time to act.
   - **TerraMetric-Atlas window:** peak and mean over the `rec_next12 = 1` spell, which
     runs from 12 months before the start through the recession itself. These match the
     figures in TerraMetric-Atlas's Rolling Backtest panel.

   A one-line note under the table explains why the second column's numbers are higher.
4. Model notes: spec, backtest skill/AUC/false-alarm rate, the revised-data caveat,
   and that this is the GLM alone (not TerraMetric-Atlas's blended headline).

## Constraints

- R 4.6, packages already installed: glmnet, fredr, dplyr, tidyr, lubridate, zoo,
  plotly, gt. Quarto 1.9.
- FRED API key comes from the user's `.Renviron` at
  `C:\Users\will.shelley\OneDrive - Avalara\Documents\.Renviron`; non-interactive
  `Rscript` needs `R_ENVIRON_USER` pointed at it. Never commit the key.
- Windows; Git Bash for shell.

## Decided by the designer (owner may override)

- Folder name `recession-tracker`; initialise it as its own git repo.
- Quarto with the R engine rather than Python.
- "Latest month" = latest month with all six predictors available; missed months are
  backfilled as live rows.
- Backtest scores every month with complete predictors, so it runs right up to the first
  live month.
- One row per month per model version; spec changes archive the old log and rebuild.
- Single-month gaps in level series (Oct-2025 CPI) are log-linearly interpolated.
- NBER announcement lag ignored, for parity with TerraMetric-Atlas; caveat in report.
- Acceptance = matches TerraMetric-Atlas's backtest on the same fetched panel within 0.01.
- No TerraMetric-Atlas comparison column in v1.
- Exact CSV columns and file layout.

## Left to the builder

- File/module layout, function names, tests, how the rebuild is triggered
  (flag/argument), chart styling details within the house palette.
