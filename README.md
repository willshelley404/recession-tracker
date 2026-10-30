# Recession Tracker

A monthly record of one model's 12-month-ahead US recession probability, from 1985 to
the latest month, with NBER recessions shaded.

- **Model:** a sign-constrained Ridge logistic regression (GLM). It is the "Long-history
  subset" spec ported from TerraMetric-Atlas, and uses six predictors:
  - yield curve
  - unemployment rise
  - payroll momentum
  - real fed funds rate
  - prime-age participation
  - 12-month change in consumer sentiment
- **History:** a walk-forward backtest. Each month's value comes from a model trained only
  on outcomes known at the time.
- **Going forward:** each run appends one row per new month. Logged rows are never
  rewritten.

Design: `docs/superpowers/specs/2026-10-30-recession-tracker-design.md`.

## Run

The FRED API key is read from your `.Renviron`. Non-interactive `Rscript` needs to be
pointed at it:

```bash
export R_ENVIRON_USER="C:/Users/will.shelley/OneDrive - Avalara/Documents/.Renviron"
Rscript run_monthly.R             # fetch, append new months, render the report
Rscript run_monthly.R --rebuild   # regenerate the backtest (live rows are kept; old log archived)
Rscript run_monthly.R --no-render # update the log only
```

## Outputs

| File | What |
|---|---|
| `log/predictions.csv` | One row per month: probability, source (`backtest`/`live`), inputs, outcome once known |
| `log/usrec.csv` | NBER recession indicator, for shading |
| `log/meta.csv` | Last run: FRED fetch time (the data vintage), status, rows added |
| `log/archive/` | Logs from earlier model versions |
| `report/recession_tracker.html` | The self-contained report |

## Tests

```bash
Rscript tests/testthat.R                                    # unit tests, no network
Rscript -e "testthat::test_file('tests/parity/test-parity-atlas.R')"  # dev-only: vs TerraMetric-Atlas
```
