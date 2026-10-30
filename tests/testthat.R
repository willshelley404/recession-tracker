# Run from the repo root: Rscript tests/testthat.R
suppressWarnings(suppressPackageStartupMessages(library(testthat)))
res <- as.data.frame(test_dir("tests/testthat", stop_on_failure = FALSE, reporter = "summary"))
n_fail <- sum(res$failed) + sum(res$error)
cat(sprintf("\n%d passed, %d failed\n", sum(res$passed), n_fail))
if (n_fail > 0) quit(status = 1)
