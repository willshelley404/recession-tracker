# ─────────────────────────────────────────────────────────────────────────────
# R/plot_static.R — static PNG of the probability line, for the email
# ─────────────────────────────────────────────────────────────────────────────

plot_static <- function(log_path = LOG_PATH, usrec_path = USREC_PATH,
                        out = "report/recession_tracker.png") {
  log <- readr::read_csv(log_path, show_col_types = FALSE)
  rec <- readr::read_csv(usrec_path, show_col_types = FALSE)
  rec <- rec[!is.na(rec$recession) & rec$date >= min(log$month), ]

  navy <- "#1B2A4A"; orange <- "#FC6600"
  last <- log[which.max(log$month), ]

  grDevices::png(out, width = 1100, height = 480, res = 110)
  on.exit(grDevices::dev.off())
  graphics::par(mar = c(3, 4, 4, 1), col.axis = navy, col.lab = navy, fg = navy)
  graphics::plot(log$month, log$prob * 100, type = "n", ylim = c(0, 100),
                 xlab = "", ylab = "12-month recession probability (%)", las = 1)

  # NBER recession months as bars, one per month
  usr <- graphics::par("usr")
  m <- rec$date[rec$recession == 1]
  graphics::rect(m, usr[3], m %m+% n_months(1), usr[4], col = "#d9dde5", border = NA)
  graphics::abline(h = seq(0, 100, 20), col = "#e9ecef")
  graphics::lines(log$month, log$prob * 100, col = orange, lwd = 2)
  graphics::points(last$month, last$prob * 100, pch = 19, col = navy)
  graphics::title(
    main = sprintf("Recession Tracker: %.1f%% (%s)", last$prob * 100, format(last$month, "%b %Y")),
    adj = 0, col.main = navy
  )
  graphics::legend("topleft", bty = "n", legend = c("GLM probability", "NBER recession"),
                   col = c(orange, "#d9dde5"), lwd = c(2, NA), pch = c(NA, 15), pt.cex = 2)
  invisible(out)
}
