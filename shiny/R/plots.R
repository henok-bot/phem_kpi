# R/plots.R
# Plotly charts. Each function takes already-filtered data and returns a plotly
# object. Keeping them here (out of server.R) makes the server easy to read and
# the plots easy to test on their own.

# horizontal bar: one indicator value per row, red->green by value, 80% target line
.bar_ryg <- function(df, xcol, ycol, xtitle, target = 80) {
  df <- df[order(df[[xcol]]), ]
  df[[ycol]] <- factor(df[[ycol]], levels = df[[ycol]])
  p <- plotly::plot_ly(
    df, x = df[[xcol]], y = df[[ycol]], type = "bar", orientation = "h",
    marker = list(color = df[[xcol]], colorscale = RYG_SCALE,
                  cmin = 0, cmax = 100),
    hovertemplate = "%{y}<br>%{x:.0f}%<extra></extra>")
  p <- plotly::layout(
    p, xaxis = list(title = xtitle, range = c(0, 105)),
    yaxis = list(title = ""), margin = list(l = 320))
  if (!is.na(target))
    p <- plotly::layout(p, shapes = list(list(
      type = "line", x0 = target, x1 = target,
      y0 = -0.5, y1 = nrow(df) - 0.5,
      line = list(color = "grey40", dash = "dash"))))
  p
}

# National KPI performance bar (proportions only)
plot_national_bar <- function(nat_slice) {
  d <- nat_slice |>
    dplyr::filter(value_type == "proportion") |>
    dplyr::mutate(label = stringr::str_trunc(label, 52)) |>
    dplyr::select(label, value_pct_capped)
  if (!nrow(d)) return(plotly::plotly_empty())
  .bar_ryg(d, "value_pct_capped", "label", "% (capped at 100)")
}

# % of indicators at target, by region
plot_region_targets <- function(reg_slice) {
  d <- reg_slice |>
    dplyr::filter(value_type == "proportion") |>
    dplyr::group_by(region) |>
    dplyr::summarise(pct = round(100 * mean(value_pct_capped >= 80, na.rm = TRUE)),
                     .groups = "drop")
  if (!nrow(d)) return(plotly::plotly_empty())
  d <- d[order(d$pct), ]; d$region <- factor(d$region, levels = d$region)
  plotly::plot_ly(d, x = ~pct, y = ~region, type = "bar", orientation = "h",
                  marker = list(color = EPHI_GREEN),
                  hovertemplate = "%{y}<br>%{x}% of indicators ≥ 80%<extra></extra>") |>
    plotly::layout(xaxis = list(title = "% of indicators ≥ 80%", range = c(0, 100)),
                   yaxis = list(title = ""))
}

# Region x indicator heatmap (+ NATIONAL column) for one period
plot_region_heatmap <- function(reg_slice, nat_slice) {
  mat <- reg_slice |>
    dplyr::filter(value_type == "proportion") |>
    dplyr::mutate(label = stringr::str_trunc(label, 46)) |>
    dplyr::select(region, label, value_pct_capped)
  natcol <- nat_slice |>
    dplyr::filter(value_type == "proportion") |>
    dplyr::mutate(label = stringr::str_trunc(label, 46), region = "NATIONAL") |>
    dplyr::select(region, label, value_pct_capped)
  if (!nrow(mat)) return(plotly::plotly_empty())

  hm  <- dplyr::bind_rows(mat, natcol) |>
    tidyr::pivot_wider(names_from = region, values_from = value_pct_capped)
  ord <- natcol[order(-natcol$value_pct_capped), ]$label
  hm  <- hm[match(ord, hm$label), ]
  labs <- hm$label
  M    <- as.matrix(hm[, -1])
  cols <- c(setdiff(colnames(M), "NATIONAL"), "NATIONAL")
  M    <- M[, cols, drop = FALSE]

  plotly::plot_ly(x = colnames(M), y = labs, z = M, type = "heatmap",
                  colorscale = RYG_SCALE, zmin = 0, zmax = 100,
                  hovertemplate = "%{x}<br>%{y}<br>%{z:.0f}%<extra></extra>") |>
    plotly::layout(xaxis = list(title = "", tickangle = -45),
                   yaxis = list(title = "", automargin = TRUE),
                   margin = list(l = 300, b = 120))
}

# National trajectory across periods, for a set of indicators (by label)
plot_trend_national <- function(trend_nat, labels = NULL) {
  d <- trend_nat
  if (!is.null(labels) && length(labels)) d <- dplyr::filter(d, label %in% labels)
  if (!nrow(d)) return(plotly::plotly_empty())
  d$period_lab <- factor(d$period, levels = PERIOD_LEVELS,
                         labels = unname(PERIOD_LABELS[PERIOD_LEVELS]))
  d$label <- stringr::str_trunc(d$label, 40)
  plotly::plot_ly(d, x = ~period_lab, y = ~value_pct, color = ~label,
                  type = "scatter", mode = "lines+markers",
                  hovertemplate = "%{fullData.name}<br>%{x}: %{y:.0f}%<extra></extra>") |>
    plotly::layout(xaxis = list(title = ""),
                   yaxis = list(title = "%", range = c(0, 105)),
                   shapes = list(list(type = "line", x0 = -0.2, x1 = 2.2,
                                      y0 = 80, y1 = 80,
                                      line = list(color = "grey40", dash = "dash"))),
                   legend = list(font = list(size = 9)))
}

# Regional composite change, earliest -> latest period (dumbbell). Clearer than
# 11 overlapping lines: grey dot = earliest, coloured dot = latest (green up / red down).
plot_trend_composite <- function(trend_comp) {
  po <- stats::setNames(seq_along(PERIOD_LEVELS), PERIOD_LEVELS)
  dc <- trend_comp |>
    dplyr::mutate(o = po[period]) |>
    dplyr::group_by(region) |>
    dplyr::summarise(first = composite[which.min(o)], last = composite[which.max(o)],
                     .groups = "drop") |>
    dplyr::mutate(dir = ifelse(last >= first, "improved", "declined")) |>
    dplyr::arrange(last)
  dc$region <- factor(dc$region, levels = dc$region)
  plotly::plot_ly(dc) |>
    plotly::add_segments(x = ~first, xend = ~last, y = ~region, yend = ~region,
                         line = list(color = "grey75", width = 3),
                         showlegend = FALSE, hoverinfo = "skip") |>
    plotly::add_markers(x = ~first, y = ~region, name = "earliest",
                        marker = list(color = "grey55", size = 9),
                        hovertemplate = "earliest: %{x:.0f}%<extra></extra>") |>
    plotly::add_markers(x = ~last, y = ~region, name = "latest",
                        marker = list(color = ~ifelse(dir == "improved", "#1a9850", "#d73027"),
                                      size = 12),
                        hovertemplate = "latest: %{x:.0f}%<extra></extra>") |>
    plotly::layout(xaxis = list(title = "composite % (earliest → latest period reported)",
                                range = c(0, 100)),
                   yaxis = list(title = ""),
                   legend = list(orientation = "h", y = -0.18))
}

# National mean performance by PHEM core function for the selected period.
plot_corefunction_bar <- function(nat_slice) {
  d <- nat_slice |>
    dplyr::filter(value_type == "proportion") |>
    dplyr::group_by(group) |>
    dplyr::summarise(v = round(mean(value_pct_capped, na.rm = TRUE)),
                     n = sum(!is.na(value_pct_capped)), .groups = "drop") |>
    dplyr::arrange(v)
  if (!nrow(d)) return(plotly::plotly_empty())
  d$group <- factor(d$group, levels = d$group)
  plotly::plot_ly(d, x = ~v, y = ~group, type = "bar", orientation = "h",
                  marker = list(color = ~v, colorscale = RYG_SCALE, cmin = 0, cmax = 100),
                  text = ~paste0(n, " indicators"),
                  hovertemplate = "%{y}<br>%{x:.0f}% (mean of %{text})<extra></extra>") |>
    plotly::layout(xaxis = list(title = "mean % of indicators (capped at 100)",
                                range = c(0, 105)),
                   yaxis = list(title = ""))
}

# One indicator, value by region (bar), for the selected period.
# Proportions use the 0-100 / 80%-target styling; rates (CFR, attack rate) use
# a plain bar with no target line and an auto x-range.
plot_indicator_by_region <- function(reg_slice, indicator_code) {
  d <- reg_slice |>
    dplyr::filter(indicator == indicator_code) |>
    dplyr::select(region, value_type, value_pct, value_pct_capped)
  if (!nrow(d)) return(plotly::plotly_empty())
  is_rate <- isTRUE(d$value_type[1] == "rate")
  d$v <- if (is_rate) d$value_pct else d$value_pct_capped
  d <- d[order(d$v), ]; d$region <- factor(d$region, levels = d$region)
  if (is_rate) {
    plotly::plot_ly(d, x = ~v, y = ~region, type = "bar", orientation = "h",
                    marker = list(color = EPHI_GREEN),
                    hovertemplate = "%{y}<br>%{x:.2f}<extra></extra>") |>
      plotly::layout(xaxis = list(title = "rate"), yaxis = list(title = ""))
  } else {
    .bar_ryg(d[, c("v", "region")], "v", "region", "%")
  }
}
