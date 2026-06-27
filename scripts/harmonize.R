#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# harmonize.R  —  build the tidy KPI master from 12 heterogeneous workbooks
#
# Strategy:
#   - read each region's data sheet raw (config in catalog.R)
#   - locate the geo block (region/zone/woreda/date [+population])
#   - walk indicator columns left->right, grouping each value column
#     (Proportion / Attack rate / Case-fatality) with its nearest preceding
#     numerator ("Number of...") and denominator ("Total number of..." /
#     "Number of expected...")
#   - map each value column to a canonical indicator (keyword, then order)
#   - recompute proportion = 100*num/denom for cross-region comparability
#
# Outputs (to output/):
#   master_woreda.csv      long: one row per woreda x indicator
#   kpi_region.csv         region x indicator rollup (summed num/denom)
#   kpi_national.csv       national x indicator rollup
#   indicator_catalog.csv  the canonical catalog
#   harmonize_log.txt      data-quality log
# ----------------------------------------------------------------------------

suppressMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(purrr); library(tibble); library(readr)
})

here <- function(...) file.path(getwd(), ...)
source(here("scripts", "catalog.R"))
dir.create(here("output"), showWarnings = FALSE)
LOG <- c()
log_msg <- function(...) { m <- paste0(...); message(m); LOG[[length(LOG)+1]] <<- m }

# ---- helpers ---------------------------------------------------------------

# classify a header into geo role / numerator / denominator / value / other
classify <- function(h) {
  hl <- str_squish(tolower(h %||% ""))
  if (hl == "" || hl == "na") return("blank")
  if (str_detect(hl, "^s\\.?n$|^column1$|^sn$"))                  return("sn")
  if (str_detect(hl, "region or city|^region$"))                 return("region")
  if (str_detect(hl, "zone or sub|^zone"))                       return("zone")
  if (str_detect(hl, "name of the (district|woreda|region)|^district$|^woreda$")) return("woreda")
  if (str_detect(hl, "date of reporting|^date"))                 return("date")
  if (str_detect(hl, "^population$"))                            return("population")
  if (str_detect(hl, "^proportion|attack rate|case-?fatality rate")) return("value")
  if (str_detect(hl, "^total number|^number of expected|^total +number")) return("denom")
  if (str_detect(hl, "^number of|^district +for which|attack|^total")) return("num")
  "other"
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# map a value header to a catalog code (keyword first)
match_code <- function(h) {
  hl <- str_squish(tolower(h))
  hit <- KPI_CATALOG$code[map_lgl(KPI_CATALOG$pattern,
              ~ str_detect(hl, regex(.x, ignore_case = TRUE)))]
  if (length(hit) >= 1) hit[1] else NA_character_
}

# ---- per-region reader -----------------------------------------------------

read_region <- function(cfg) {
  log_msg("\n== ", cfg$region, "  (", cfg$file, " :: ", cfg$sheet, ") ==")
  # n_max guards against phantom sheets (one Addis workbook declares 1,048,576
  # rows); real KPI sheets hold at most a few hundred woredas, so 20000 is safe
  # and keeps every run fast. Trailing blank rows are dropped by the keep filter.
  raw <- read_excel(file.path(cfg$dir, cfg$file), sheet = cfg$sheet, col_names = FALSE,
                    .name_repair = "minimal", n_max = 20000L)
  hdr <- as.character(unlist(raw[cfg$header_row, ]))
  dat <- raw[(cfg$header_row + 1):nrow(raw), , drop = FALSE]
  names(dat) <- paste0("X", seq_along(hdr))

  roles <- vapply(hdr, classify, character(1))
  col_idx <- function(role) which(roles == role)

  # geo columns (take first match of each)
  g_region <- col_idx("region")[1]; g_zone <- col_idx("zone")[1]
  g_woreda <- col_idx("woreda")[1]; g_date <- col_idx("date")[1]
  g_pop    <- col_idx("population")[1]

  geo <- tibble(
    region_raw = if (!is.na(g_region)) as.character(dat[[g_region]]) else NA,
    zone       = if (!is.na(g_zone))   as.character(dat[[g_zone]])   else NA,
    woreda     = if (!is.na(g_woreda)) as.character(dat[[g_woreda]]) else NA,
    report_date_raw = if (!is.na(g_date)) as.character(dat[[g_date]]) else NA,
    population = if (!is.na(g_pop)) suppressWarnings(as.numeric(dat[[g_pop]])) else NA_real_
  )
  # facility-level regions (e.g. Dire Dawa) put the reporting unit's name in the
  # region column, leaving woreda blank — recover it as the unit name.
  if (identical(cfg$level, "facility")) {
    blank <- is.na(geo$woreda) | str_squish(geo$woreda) %in% c("", "NA")
    geo$woreda[blank] <- geo$region_raw[blank]
  }
  geo$row_id <- seq_len(nrow(geo))

  # keep rows with a real unit name; drop blanks and leaked total/summary rows.
  # Aggregate rows surface in two places: the woreda/unit cell ("Total",
  # "Regional cumulative", ...) and, for Addis sub-city reporting, the ZONE cell
  # holding the region code ("AA"/"aa") on a regional-total line. Check both.
  is_aggregate <- function(x) {
    xl <- str_squish(tolower(ifelse(is.na(x), "", x)))
    xl %in% c("total","regional cumulative","region","grand total","sum",
              "cumulative","aa","a.a","addis ababa") |
      str_detect(xl, "cumu|grand total|^total$|^sum$|^region$")  # 'cumu' = cumulative/cumuative typo
  }
  keep <- !(is.na(geo$woreda) | str_squish(geo$woreda) %in% c("", "NA")) &
          !is_aggregate(geo$woreda) & !is_aggregate(geo$zone)
  geo <- geo[keep, ]; dat <- dat[keep, ]; geo$row_id <- seq_len(nrow(geo))
  log_msg("  rows kept: ", nrow(geo), "  (level: ", cfg$level, ")")

  # walk indicator columns after the date column
  start <- max(c(g_date, g_pop, g_region, g_zone, g_woreda), na.rm = TRUE) + 1
  triplets <- list(); last_num <- NA; last_denom <- NA
  for (j in start:length(hdr)) {
    r <- roles[j]
    if (r == "num")        last_num <- j
    else if (r == "denom") last_denom <- j
    else if (r == "value") {
      triplets[[length(triplets)+1]] <- list(
        value_col = j, num_col = last_num, denom_col = last_denom,
        header = hdr[j])
    }
  }
  log_msg("  value columns found: ", length(triplets))

  # assign codes: keyword match; fall back to catalog order for unmatched
  codes <- map_chr(triplets, ~ match_code(.x$header))

  # rate indicators (attack rate, CFR) have two "Number of..." columns as
  # numerator/denominator, so the role walk mis-grabs the denominator.
  # Pair them positionally: numerator = value-2, denominator = value-1.
  rate_codes <- KPI_CATALOG$code[KPI_CATALOG$value_type == "rate"]
  for (k in seq_along(triplets)) {
    if (!is.na(codes[k]) && codes[k] %in% rate_codes) {
      vc <- triplets[[k]]$value_col
      triplets[[k]]$num_col   <- if (vc - 2 >= 1) vc - 2 else NA
      triplets[[k]]$denom_col <- if (vc - 1 >= 1) vc - 1 else NA
    }
  }

  # de-duplicate: if a code maps to >1 value column, keep the first
  dup <- which(duplicated(codes) & !is.na(codes))
  if (length(dup)) {
    log_msg("  note: dropped ", length(dup),
            " duplicate value col(s) for codes: ",
            paste(unique(codes[dup]), collapse = ", "))
    codes[dup] <- NA
  }

  unmatched <- which(is.na(codes))
  if (length(unmatched) && length(triplets) == nrow(KPI_CATALOG)) {
    codes[unmatched] <- KPI_CATALOG$order[unmatched] |>
      (\(o) KPI_CATALOG$code[o])()
    log_msg("  ", length(unmatched), " value col(s) matched by ORDER fallback")
  } else if (length(unmatched)) {
    log_msg("  WARNING: ", length(unmatched),
            " unmatched value col(s), no clean order fallback: ",
            paste(map_chr(triplets[unmatched], "header") |> substr(1, 40),
                  collapse = " | "))
  }

  # build long records
  num_at <- function(col, i) if (is.na(col)) NA_real_ else
              suppressWarnings(as.numeric(dat[[col]][i]))
  recs <- map2_dfr(triplets, codes, function(t, code) {
    if (is.na(code)) return(NULL)
    tibble(
      row_id      = geo$row_id,
      indicator   = code,
      numerator   = vapply(geo$row_id, function(i) num_at(t$num_col, i),   numeric(1)),
      denominator = vapply(geo$row_id, function(i) num_at(t$denom_col, i), numeric(1)),
      value_reported = vapply(geo$row_id, function(i) num_at(t$value_col, i), numeric(1)),
      src_header  = t$header
    )
  })

  out <- recs |>
    left_join(geo, by = "row_id") |>
    mutate(region = cfg$region, period = cfg$period, report_level = cfg$level) |>
    select(region, period, report_level, zone, woreda, report_date_raw, population,
           indicator, numerator, denominator, value_reported, src_header)
  out
}

# ---- run all regions -------------------------------------------------------

master <- pmap_dfr(REGION_CONFIG, function(...) {
  cfg <- tibble(...)
  tryCatch(read_region(cfg),
           error = function(e) { log_msg("  ERROR: ", e$message); NULL })
})

# attach catalog metadata + recompute comparable value
master <- master |>
  left_join(select(KPI_CATALOG, indicator = code, group, value_type, label),
            by = "indicator") |>
  mutate(
    value_pct = case_when(
      value_type == "proportion" & !is.na(denominator) & denominator > 0 ~
        100 * numerator / denominator,
      value_type == "proportion" & is.na(denominator) & !is.na(value_reported) &
        value_reported <= 1.5 ~ value_reported * 100,        # 0-1 scale -> %
      value_type == "proportion" ~ value_reported,
      TRUE ~ NA_real_),
    value_pct_capped = if_else(value_type == "proportion" & !is.na(value_pct),
                               pmin(value_pct, 100), value_pct),  # 0-100 for plots
    # did this woreda actually report this indicator? (blank cells read as NA;
    # genuine zeros read as 0, so NR is distinguishable from a real 0)
    reported = !is.na(numerator) | !is.na(denominator) | !is.na(value_reported),
    # a woreda only contributes to an aggregate ratio when it has BOTH a
    # numerator and a valid (>0) denominator. For a proportion the numerator
    # cannot exceed the denominator — such rows are data-entry errors (e.g. a
    # leaked total row) and are excluded so aggregates can't exceed 100%.
    valid_pair = !is.na(numerator) & !is.na(denominator) & denominator > 0 &
                 (value_type == "rate" | numerator <= denominator)
  ) |>
  relocate(group, label, value_type, .after = indicator) |>
  arrange(region, factor(indicator, levels = KPI_CATALOG$code), zone, woreda)

write_csv(master, here("output", "master_woreda.csv"))
log_msg("\nMaster: ", nrow(master), " rows x ", ncol(master), " cols, ",
        n_distinct(master$region), " regions, ",
        n_distinct(master$woreda), " woredas")
impossible <- master |>
  filter(value_type == "proportion", !is.na(numerator), !is.na(denominator),
         denominator > 0, numerator > denominator)
log_msg("Excluded ", nrow(impossible),
        " proportion rows with numerator > denominator (data-entry errors).")

# ---- rollups (summed num/denom, per CLAUDE.md totals rule) ------------------

roll <- function(df, ...) {
  g <- enquos(...)
  df |>
    group_by(!!!g, indicator, group, label, value_type) |>
    summarise(
      n_woreda    = n(),                       # woredas with this indicator column
      n_reported  = sum(reported),             # woredas that filled it in
      n_pairs     = sum(valid_pair),           # woredas with a usable num+denom
      num_sum     = sum(numerator[valid_pair]),   # complete pairs only
      denom_sum   = sum(denominator[valid_pair]),
      .groups = "drop") |>
    mutate(
      pct_reported = round(100 * n_reported / n_woreda, 1),
      value_pct = case_when(
        value_type == "proportion" & denom_sum > 0 ~ 100 * num_sum / denom_sum,
        value_type == "rate"       & denom_sum > 0 ~ num_sum / denom_sum,
        TRUE ~ NA_real_),
      value_pct_capped = if_else(value_type == "proportion" & !is.na(value_pct),
                                 pmin(value_pct, 100), value_pct)) |>
    arrange(factor(indicator, levels = KPI_CATALOG$code))
}

kpi_region   <- roll(master, region, period)
kpi_national <- roll(master, period) |> mutate(region = "NATIONAL", .before = 1)

write_csv(kpi_region,   here("output", "kpi_region.csv"))
write_csv(kpi_national, here("output", "kpi_national.csv"))
write_csv(KPI_CATALOG,  here("output", "indicator_catalog.csv"))

# ---- data-quality log ------------------------------------------------------
cov <- master |> group_by(region) |>
  summarise(woredas = n_distinct(woreda),
            indicators = n_distinct(indicator),
            has_pop = any(!is.na(population)), .groups = "drop")
LOG[[length(LOG)+1]] <- "\n=== coverage ==="
LOG[[length(LOG)+1]] <- paste(capture.output(print(as.data.frame(cov))),
                              collapse = "\n")
writeLines(unlist(LOG), here("output", "harmonize_log.txt"))
message("\nDone -> output/  (master_woreda.csv, kpi_region.csv, kpi_national.csv)")
