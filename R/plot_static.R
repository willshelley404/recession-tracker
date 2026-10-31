# ─────────────────────────────────────────────────────────────────────────────
# R/plot_static.R — static PNGs of the probability line, for the email
# ─────────────────────────────────────────────────────────────────────────────

#' Full history by default; `months` = N plots only the latest N months, with
#' a point and a tick per month
plot_static <- function(log_path = LOG_PATH, usrec_path = USREC_PATH,
                        out = "report/recession_tracker.png", months = NULL) {
  log <- readr::read_csv(log_path, show_col_types = FALSE)
  if (!is.null(months)) log <- log[log$month > max(log$month) %m-% n_months(months), ]
  rec <- readr::read_csv(usrec_path, show_col_types = FALSE)
  rec <- rec[!is.na(rec$recession) & rec$date >= min(log$month) & rec$date <= max(log$month), ]

  navy <- "#1B2A4A"; orange <- "#FC6600"
  last <- log[which.max(log$month), ]

  grDevices::png(out, width = 1100, height = 480, res = 110)
  on.exit(grDevices::dev.off())
  graphics::par(mar = c(3, 4, 4, 1), col.axis = navy, col.lab = navy, fg = navy)
  graphics::plot(log$month, log$prob * 100, type = "n", ylim = c(0, 100),
                 xlab = "", ylab = "12-month recession probability (%)", las = 1,
                 xaxt = if (is.null(months)) "s" else "n")

  # NBER recession months as bars, one per month
  usr <- graphics::par("usr")
  m <- rec$date[rec$recession == 1]
  if (length(m)) graphics::rect(m, usr[3], m %m+% n_months(1), usr[4], col = "#d9dde5", border = NA)
  graphics::abline(h = seq(0, 100, 20), col = "#e9ecef")
  graphics::lines(log$month, log$prob * 100, col = orange, lwd = 2)
  if (!is.null(months)) {
    graphics::points(log$month, log$prob * 100, pch = 19, cex = 0.7, col = orange)
    graphics::axis(1, at = log$month, labels = FALSE, tcl = -0.25)
    q <- log$month[as.integer(format(log$month, "%m")) %in% c(1, 4, 7, 10)]
    graphics::axis(1, at = q, labels = format(q, "%b %Y"))
  }
  graphics::points(last$month, last$prob * 100, pch = 19, col = navy)
  graphics::title(
    main = sprintf("Recession Tracker%s: %.1f%% (%s)",
                   if (is.null(months)) "" else sprintf(", last %d months", months),
                   last$prob * 100, format(last$month, "%b %Y")),
    adj = 0, col.main = navy
  )
  k <- if (length(m)) 1:2 else 1
  graphics::legend("topleft", bty = "n", legend = c("GLM probability", "NBER recession")[k],
                   col = c(orange, "#d9dde5")[k], lwd = c(2, NA)[k], pch = c(NA, 15)[k], pt.cex = 2)
  invisible(out)
}
