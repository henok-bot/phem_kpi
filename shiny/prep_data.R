#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# prep_data.R  —  build the lean data the Shiny app reads.
#
# WHY THIS EXISTS
#   The Shiny app should NOT read the raw workbooks or the full pipeline. It
#   reads a handful of small .rds files. This script turns the pipeline's CSV
#   output (output/, exports/tables/) + the GIS geometry into those .rds files.
#
#   shinyapps.io only needs what is inside shiny/ . By keeping the data small
#   and self-contained here, deployment stays fast and carries no raw data.
#
# WHEN TO RUN IT
#   After every pipeline refresh (e.g. a new quarter):
#     Rscript scripts/run_all.R --no-render     # refresh output/ + exports/
#     Rscript shiny/prep_data.R                 # refresh shiny/data/*.rds
#   then redeploy (see shiny/DEPLOY/README or dashboard_and_hosting.md).
#
# Run from the PROJECT ROOT:  Rscript shiny/prep_data.R
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(readr) })

root     <- normalizePath(".")
stopifnot(file.exists(file.path(root, "scripts", "catalog.R")))
op       <- function(...) file.path(root, "output", ...)
tb       <- function(...) file.path(root, "exports", "tables", ...)
data_dir <- file.path(root, "shiny", "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

source(file.path(root, "scripts", "catalog.R"))   # PERIOD_LEVELS/LABELS, FOCUS_PERIOD

message("Reading pipeline output ...")
national <- read_csv(op("kpi_national.csv"),       show_col_types = FALSE)
region   <- read_csv(op("kpi_region.csv"),         show_col_types = FALSE)
wide     <- read_csv(op("master_woreda_wide.csv"), show_col_types = FALSE)
wpcode   <- read_csv(op("kpi_woreda_pcode.csv"),   show_col_types = FALSE)
catalog  <- read_csv(op("indicator_catalog.csv"),  show_col_types = FALSE)

trend_nat  <- read_csv(tb("trend_national_by_period.csv"), show_col_types = FALSE)
trend_comp <- read_csv(tb("trend_region_composite.csv"),  show_col_types = FALSE)
trend_dq   <- read_csv(tb("trend_data_quality.csv"),      show_col_types = FALSE)
trend_cf   <- read_csv(tb("trend_corefunction.csv"),      show_col_types = FALSE)

# Geometry lives in assets/ (copied once from the GIS shapefile). 1,082 woredas
# keyed by adm3_pcode -> joins directly to wpcode.
geo3 <- readRDS(file.path(root, "assets", "eth_adm3_geo.rds"))
geo1 <- readRDS(file.path(root, "assets", "eth_adm1_geo.rds"))

# --- guardrail: proportions must never exceed 100% --------------------------
bad <- bind_rows(national, region) |>
  filter(value_type == "proportion", !is.na(value_pct), value_pct > 100.0001)
if (nrow(bad) > 0) stop("Guardrail tripped: ", nrow(bad), " proportion(s) > 100%.")

# --- small metadata bundle the UI uses to build its menus -------------------
meta <- list(
  period_levels = PERIOD_LEVELS,
  period_labels = PERIOD_LABELS,
  focus_period  = FOCUS_PERIOD,
  regions       = sort(unique(region$region)),
  # indicator code -> label (proportions + rates), in catalog order
  indicators    = setNames(catalog$code, catalog$label),
  groups        = sort(unique(catalog$group)),
  catalog       = catalog
)

message("Writing shiny/data/*.rds ...")
saveRDS(national,   file.path(data_dir, "national.rds"))
saveRDS(region,     file.path(data_dir, "region.rds"))
saveRDS(wide,       file.path(data_dir, "wide.rds"))
saveRDS(wpcode,     file.path(data_dir, "wpcode.rds"))
saveRDS(trend_nat,  file.path(data_dir, "trend_nat.rds"))
saveRDS(trend_comp, file.path(data_dir, "trend_comp.rds"))
saveRDS(trend_dq,   file.path(data_dir, "trend_dq.rds"))
saveRDS(trend_cf,   file.path(data_dir, "trend_cf.rds"))
saveRDS(geo3,       file.path(data_dir, "geo3.rds"))
saveRDS(geo1,       file.path(data_dir, "geo1.rds"))
saveRDS(meta,       file.path(data_dir, "meta.rds"))

# the app needs its own copy of the logo under www/
www_dir <- file.path(root, "shiny", "www")
dir.create(www_dir, showWarnings = FALSE, recursive = TRUE)
file.copy(file.path(root, "assets", "ephi_logo.png"),
          file.path(www_dir, "ephi_logo.png"), overwrite = TRUE)

message("Done. shiny/data/ now holds ", length(list.files(data_dir)), " files.")
