# ─────────────────────────────────────────────────────────────────────────────
# report/_helpers.R — palette + small HTML/Plotly helpers for the report
# ─────────────────────────────────────────────────────────────────────────────

PAL <- list(
  navy = "#1B2A4A", orange = "#FC6600", teal = "#2EC4B6",
  neutral = "#6c757d", positive = "#198754", negative = "#dc3545", warning = "#d97706",
  band = "rgba(108,117,125,0.22)"
)
FONT <- "Inter, DejaVu Sans, Arial, sans-serif"

fmt_pct <- function(x, d = 1) ifelse(is.na(x), "—", sprintf(paste0("%.", d, "f%%"), 100 * x))
fmt_pts <- function(x, d = 1) ifelse(is.na(x), "—", sprintf(paste0("%+.", d, "f pts"), x))

kpi_card <- function(label, value, sub = NULL, accent = PAL$navy) {
  sprintf(
    "<div style='flex:1;min-width:180px;border-left:4px solid %s;background:rgba(27,42,74,0.04);padding:10px 14px;border-radius:4px;'>
       <div style='font-size:11px;text-transform:uppercase;letter-spacing:.06em;color:%s;'>%s</div>
       <div style='font-size:26px;font-weight:700;color:%s;line-height:1.2;'>%s</div>%s</div>",
    accent, PAL$neutral, label, accent, value,
    if (is.null(sub)) "" else sprintf("<div style='font-size:12px;color:%s;'>%s</div>", PAL$neutral, sub)
  )
}

kpi_row <- function(...) {
  htmltools::HTML(paste0("<div style='display:flex;gap:12px;flex-wrap:wrap;margin:8px 0 18px;'>",
                         paste(c(...), collapse = ""), "</div>"))
}

callout <- function(kind = c("note", "inputs", "logic", "output", "warn"), title, body) {
  kind <- match.arg(kind)
  col <- switch(kind, note = PAL$neutral, inputs = PAL$teal, logic = PAL$navy,
                output = PAL$positive, warn = PAL$warning)
  htmltools::HTML(sprintf(
    "<div style='border-left:4px solid %s;background:rgba(0,0,0,0.025);padding:10px 14px;margin:10px 0;border-radius:4px;'>
       <div style='font-weight:700;color:%s;margin-bottom:4px;'>%s</div><div>%s</div></div>",
    col, col, title, body
  ))
}

#' Shared Plotly look: white, house font, navy text, horizontal legend below
tm_layout <- function(p, yaxis = list(), ...) {
  plotly::layout(
    p,
    font = list(family = FONT, color = PAL$navy),
    paper_bgcolor = "white", plot_bgcolor = "white",
    legend = list(orientation = "h", x = 0, y = -0.15),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, title = ""),
    yaxis = utils::modifyList(list(gridcolor = "#e9ecef", zeroline = FALSE), yaxis),
    margin = list(t = 30, r = 20, b = 60, l = 60),
    hovermode = "x unified",
    ...
  ) |> plotly::config(displaylogo = FALSE)
}

# ── Drivers ──────────────────────────────────────────────────────────────────
DRIVER_LABEL <- c(
  yield_spread = "Yield curve", u_rise = "Unemployment rise", gap_rise = "Underemployment gap rise",
  pay_3m = "Payroll momentum", real_rate = "Real fed funds rate", prime_lfpr = "Prime-age participation", sent_chg12 = "Sentiment change"
)
# Fixed categorical order, checked with the dataviz palette validator on white
# (adjacent stacked pairs separate under CVD). Not the brand hues: navy and
# orange already mean backtest and live on the chart above.
DRIVER_COLOR <- c(
  yield_spread = "#6250d6", u_rise = "#eda100", gap_rise = "#e34948", pay_3m = "#2a78d6",
  real_rate = "#e87ba4", prime_lfpr = "#008300", sent_chg12 = "#1baf7a"
)
DRIVER_FMT <- c(
  yield_spread = "%.2f pp", u_rise = "%.1f pp", gap_rise = "%.1f pp", pay_3m = "%+.0fk jobs/mo",
  real_rate = "%.2f pp", prime_lfpr = "%.1f%%", sent_chg12 = "%+.1f pts"
)
