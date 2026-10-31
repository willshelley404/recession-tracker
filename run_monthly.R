# ─────────────────────────────────────────────────────────────────────────────
# run_monthly.R — fetch FRED, log new months, render the report
#
#   Rscript run_monthly.R             # append new months + render
#   Rscript run_monthly.R --rebuild   # archive the log, rebuild history
#   Rscript run_monthly.R --no-render # log only
#
# Needs FRED_API_KEY: export R_ENVIRON_USER=".../Documents/.Renviron" first.
# ─────────────────────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
setwd(root)
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

res <- run_pipeline(rebuild = "--rebuild" %in% args, render = !"--no-render" %in% args)
if (!"--no-render" %in% args) {
  plot_static()
  plot_static(out = "report/recession_tracker_24m.png", months = 24)
}
r <- latest_reading(res$log)
cat(sprintf(
  "%s: %d new row(s). Latest %s: %.1f%% (%s)%s\n",
  res$status, res$n_new, format(r$month, "%b %Y"), r$prob * 100, r$source,
  if (is.na(r$change_pts)) "" else sprintf(", %+.1f pts vs %s", r$change_pts, format(r$prev_month, "%b %Y"))
))
