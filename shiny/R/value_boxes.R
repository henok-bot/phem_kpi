# R/value_boxes.R
# Reporting-first summary for the Overview value boxes. We deliberately lead with
# how MUCH was reported (regions, woredas, coverage, entries filled) rather than a
# single performance score, because every KPI % is computed only among woredas
# that reported. Inputs are already filtered to the selected period + regions.
#   reg_slice  : rows from region.rds   (one row per region x indicator)
#   wide_slice : rows from wide.rds     (one row per woreda)
#   pct_filled : the period's "% of indicator entries filled" (from trend_dq)

kpi_value_boxes <- function(reg_slice, wide_slice, pct_filled = NA_real_) {
  prop <- dplyr::filter(reg_slice, value_type == "proportion")

  n_reg <- dplyr::n_distinct(reg_slice$region)
  n_wor <- nrow(dplyr::distinct(wide_slice, region, woreda))
  coverage <- round(100 * n_wor / N_WOREDAS_REF)

  list(
    n_reg          = n_reg,
    n_wor          = n_wor,
    coverage       = coverage,
    entries_filled = if (length(pct_filled)) round(pct_filled) else NA_real_
  )
}

# colour a 0-100 figure: green high, amber middle, red low
cov_col <- function(x) if (isTRUE(x >= 66)) "success" else
                       if (isTRUE(x >= 33)) "warning" else "danger"
