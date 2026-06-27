#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# run_all.R  —  run the whole KPI pipeline end to end.
# Run from the project root:  Rscript scripts/run_all.R
#
# Most steps are cheap and re-runnable. The two slow ones are made cheap so a
# re-run does not repeat work that has not changed:
#   - convert.R is INCREMENTAL: it only re-converts workbooks whose readable
#     copy is missing or older than the source (timestamp check). Untouched
#     files are skipped, so a re-run with no new raw data is near-instant.
#   - harmonize.R caps its Excel read (n_max) so a phantom 1M-row sheet no
#     longer costs minutes.
#
# Skip flags (pass on the command line) for when you only need part of it:
#   --no-convert    skip the readable-copy conversion entirely
#   --no-template   skip rebuilding the blank data-collection templates
#   --no-render     skip the Quarto report renders (the slowest stage)
#   --only-render   render reports only (assumes output/ + exports/ are current)
# Examples:
#   Rscript scripts/run_all.R                 # full build (incremental)
#   Rscript scripts/run_all.R --no-render     # refresh data + figures only
#   Rscript scripts/run_all.R --only-render   # re-render reports only
# ----------------------------------------------------------------------------

stopifnot(file.exists("scripts/harmonize.R"))   # guard: must be project root
args <- commandArgs(trailingOnly = TRUE)
has  <- function(flag) flag %in% args

steps <- c(
  if (!has("--no-convert")) "scripts/convert.R", # incremental readable copies
  "scripts/harmonize.R",      # -> output/master_woreda.csv + rollups (ALL periods)
  "scripts/crosswalk_build.R",# -> output/crosswalk_woreda.csv (name -> P-code)
  "scripts/spatialize.R",     # -> master_woreda_geo.csv + kpi_woreda_pcode.csv (+ population backfill)
  "scripts/analyze.R",        # -> exports/figures + exports/tables (focus period)
  "scripts/analyze_depth.R",  # -> gap/scorecard/best-worst + per-indicator PNGs (focus period)
  "scripts/analyze_trends.R", # -> exports/figures/T0* + trend_*.csv (cross-period)
  if (!has("--no-template")) "scripts/make_template.R"  # blank fill-in templates
)
if (!has("--only-render")) {
  for (s in steps) {
    message("\n========== ", s, " ==========")
    system2("Rscript", s)
  }
} else message("\n--only-render: skipping data/figure steps")

# the focus (newest) period drives the single-period report's archive name
source("scripts/catalog.R")

message("\n========== render reports ==========")
if (has("--no-render")) {
  message("--no-render: skipped report render")
} else if (nzchar(Sys.which("quarto"))) {
  system2("quarto", c("render", "reports/kpi_report.qmd", "--to", "html,docx"))
  system2("quarto", c("render", "reports/kpi_trends.qmd", "--to", "html,docx"))
  system2("quarto", c("render", "reports/pipeline_documentation.qmd", "--to", "html,docx"))
  system2("quarto", c("render", "reports/kpi_dashboard.qmd"))  # interactive dashboard (html)

  # Archive the single-period report under its period so previous quarters'
  # reports are preserved when FOCUS_PERIOD advances to a new quarter.
  arch <- file.path("reports", "archive")
  dir.create(arch, showWarnings = FALSE, recursive = TRUE)
  for (ext in c("html", "docx")) {
    src <- file.path("reports", paste0("kpi_report.", ext))
    if (file.exists(src))
      file.copy(src, file.path(arch, sprintf("kpi_report_%s.%s", FOCUS_PERIOD, ext)),
                overwrite = TRUE)
  }
  message("archived single-period report as reports/archive/kpi_report_", FOCUS_PERIOD, ".{html,docx}")
} else message("quarto not found; skipped report render")

message("\nPipeline complete.")
