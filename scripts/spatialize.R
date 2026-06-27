#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# spatialize.R  —  attach ADM3_PCODE to the master and roll up to woreda level
#
# Joins output/master_woreda.csv to output/crosswalk_woreda.csv so every record
# carries a clean shapefile ADM3_PCODE. Then aggregates to the canonical 1082
# woreda level (Addis -> sub-city, facilities -> their woreda) for spatial
# analysis and woreda choropleths.
#
# Output (output/):
#   master_woreda_geo.csv   master + adm3_pcode/adm3_en + ref region/zone
#   kpi_woreda_pcode.csv     adm3_pcode x indicator rollup (summed num/denom)
# ----------------------------------------------------------------------------

suppressMessages({ library(dplyr); library(readr); library(tidyr); library(stringr); library(readxl) })
here <- function(...) file.path(getwd(), ...)
norm <- function(x) x |> tolower() |> str_replace_all("[^a-z0-9 ]", " ") |> str_squish()

master <- read_csv(here("output", "master_woreda.csv"),    show_col_types = FALSE)
xwalk  <- read_csv(here("output", "crosswalk_woreda.csv"), show_col_types = FALSE) |>
  select(region, zone, woreda, adm3_pcode, adm3_en,
         ref_region = adm1_en, ref_zone = adm2_en, xwalk_method = method)

geo <- master |>
  left_join(xwalk, by = c("region", "zone", "woreda"))

# ---- population backfill ----------------------------------------------------
# Regions often leave Population blank, which blocks population-based metrics
# (attack rate, incidence). "Population by GIS woreda best.xlsx" maps a clean
# shapefile woreda name (Gnames) -> population. Join it on the crosswalked
# reference woreda name (adm3_en) and coalesce: reported value first, else the
# reference population. population_best is what AR/incidence should use.
pop_file <- here("Population by GIS woreda best.xlsx")
if (file.exists(pop_file)) {
  popref <- read_excel(pop_file, .name_repair = "minimal") |>
    transmute(g_norm = norm(Gnames), population_ref = suppressWarnings(as.numeric(population))) |>
    filter(g_norm != "", !is.na(population_ref)) |>
    group_by(g_norm) |> summarise(population_ref = max(population_ref), .groups = "drop")
  geo <- geo |>
    mutate(g_norm = norm(adm3_en)) |>
    left_join(popref, by = "g_norm") |>
    mutate(population_best = coalesce(population, population_ref)) |>
    select(-g_norm)
  n_popfill <- sum(is.na(geo$population) & !is.na(geo$population_ref))
  cat("population: reference file matched",
      sum(!is.na(geo$population_ref)), "records; backfilled",
      n_popfill, "that lacked a reported population\n")
} else {
  geo <- geo |> mutate(population_ref = NA_real_, population_best = population)
  cat("population reference file not found - population_best = reported only\n")
}

n_tot <- nrow(geo); n_mapped <- sum(!is.na(geo$adm3_pcode))
write_csv(geo, here("output", "master_woreda_geo.csv"))

# compact woreda-population lookup (one row per mapped woreda, newest source)
geo |> filter(!is.na(adm3_pcode)) |>
  group_by(adm3_pcode, adm3_en, ref_region) |>
  summarise(population_best = suppressWarnings(max(population_best, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(population_best = ifelse(is.infinite(population_best), NA_real_, population_best)) |>
  write_csv(here("output", "woreda_population.csv"))

# woreda (pcode) x indicator rollup — summed numerator/denominator
pcode <- geo |>
  filter(!is.na(adm3_pcode)) |>
  mutate(valid_pair = !is.na(numerator) & !is.na(denominator) & denominator > 0 &
                       (value_type == "rate" | numerator <= denominator)) |>
  group_by(adm3_pcode, adm3_en, ref_region, ref_zone,
           indicator, group, label, value_type, period) |>
  summarise(num_sum   = sum(numerator[valid_pair]),   # complete pairs only
            denom_sum = sum(denominator[valid_pair]),
            n_records = n(), .groups = "drop") |>
  mutate(value_pct = case_when(
           value_type == "proportion" & denom_sum > 0 ~ 100 * num_sum / denom_sum,
           value_type == "rate"       & denom_sum > 0 ~ num_sum / denom_sum,
           TRUE ~ NA_real_),
         value_pct_capped = if_else(value_type == "proportion" & !is.na(value_pct),
                                    pmin(value_pct, 100), value_pct))
write_csv(pcode, here("output", "kpi_woreda_pcode.csv"))

# wide combined master: one row per woreda, one column per indicator (value %)
catalog <- read_csv(here("output","indicator_catalog.csv"), show_col_types = FALSE)
wide <- geo |>
  select(region, zone, woreda, adm3_pcode, population, population_best, period, indicator, value_pct) |>
  mutate(indicator = factor(indicator, levels = catalog$code)) |>
  tidyr::pivot_wider(names_from = indicator, values_from = value_pct,
                     values_fn = function(x) round(mean(x, na.rm = TRUE), 1)) |>
  arrange(region, zone, woreda)
write_csv(wide, here("output", "master_woreda_wide.csv"))

cat("records mapped to a P-code:", n_mapped, "/", n_tot,
    " (", round(100*n_mapped/n_tot), "% )\n")
cat("-> output/master_woreda_wide.csv  (one row per woreda, indicators as columns)\n")
cat("distinct woredas (P-codes) with data:", n_distinct(pcode$adm3_pcode), "\n")
cat("-> output/master_woreda_geo.csv\n-> output/kpi_woreda_pcode.csv\n")
