# R/value_boxes.R
# Summary statistics for the Overview value boxes.
# Inputs are already filtered to the selected period + regions by the server.
#   reg_slice  : rows from region.rds  (one row per region x indicator)
#   wide_slice : rows from wide.rds    (one row per woreda)
# Returns a plain list the server turns into value boxes.

kpi_value_boxes <- function(reg_slice, wide_slice) {
  prop <- dplyr::filter(reg_slice, value_type == "proportion")

  n_reg <- dplyr::n_distinct(reg_slice$region)
  n_wor <- nrow(dplyr::distinct(wide_slice, region, woreda))

  vals    <- prop$value_pct_capped
  vals    <- vals[!is.na(vals)]
  pct_met   <- if (length(vals)) round(100 * mean(vals >= 80)) else NA_real_
  composite <- if (length(vals)) round(mean(vals)) else NA_real_

  list(
    n_reg     = n_reg,
    n_wor     = n_wor,
    pct_met   = pct_met,
    composite = composite
  )
}
