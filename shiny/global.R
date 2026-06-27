# global.R
# ----------------------------------------------------------------------------
# Runs ONCE when the app starts (before any user connects). Three jobs:
#   1. load packages,
#   2. read the prepared .rds data (built by prep_data.R),
#   3. define constants + login credentials, and source the R/ helper files.
# Everything created here is visible to BOTH ui.R and server.R.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)            # the web framework
  library(bslib)            # Bootstrap 5 layout + theming (page_navbar, cards)
  library(shinycssloaders)  # the spinning loader while a plot renders
  library(plotly)           # interactive charts
  library(leaflet)          # interactive maps
  library(DT)               # interactive tables
  library(dplyr)            # data wrangling
  library(tidyr)            # pivot/reshape
  library(stringr)          # string helpers
  library(scales)           # number formatting (comma, percent)
  library(sf)               # spatial data for the map
  library(shinymanager)     # the username / password login gate
  library(shinyWidgets)     # nicer inputs (pickerInput)
})

# ── 1. Login credentials ────────────────────────────────────────────────────
# Add or change users here. For a real deployment, move these out of source
# (see the note in dashboard_and_hosting.md on env vars / keyring).
APP_CREDENTIALS <- data.frame(
  user     = c("ephi",    "phem"),
  password = c("kpi2026", "phem2026"),
  comment  = c("EPHI",    "PHEM team"),
  stringsAsFactors = FALSE
)

# ── 2. Load prepared data ────────────────────────────────────────────────────
DATA_DIR <- "data"
national  <- readRDS(file.path(DATA_DIR, "national.rds"))
region    <- readRDS(file.path(DATA_DIR, "region.rds"))
wide      <- readRDS(file.path(DATA_DIR, "wide.rds"))
wpcode    <- readRDS(file.path(DATA_DIR, "wpcode.rds"))
trend_nat <- readRDS(file.path(DATA_DIR, "trend_nat.rds"))
trend_comp<- readRDS(file.path(DATA_DIR, "trend_comp.rds"))
trend_dq  <- readRDS(file.path(DATA_DIR, "trend_dq.rds"))
trend_cf  <- readRDS(file.path(DATA_DIR, "trend_cf.rds"))
geo3      <- readRDS(file.path(DATA_DIR, "geo3.rds"))
geo1      <- readRDS(file.path(DATA_DIR, "geo1.rds"))
meta      <- readRDS(file.path(DATA_DIR, "meta.rds"))

# ── 3. Constants used to build the UI menus ──────────────────────────────────
PERIOD_LEVELS <- meta$period_levels
PERIOD_LABELS <- meta$period_labels
FOCUS_PERIOD  <- meta$focus_period
ALL_REGIONS   <- meta$regions
INDICATORS    <- meta$indicators          # named vector: label -> code
GROUPS        <- meta$groups
CATALOG       <- meta$catalog
EPHI_GREEN    <- "#006600"

# friendly period dropdown choices: label shown, period code stored
PERIOD_CHOICES <- setNames(PERIOD_LEVELS, unname(PERIOD_LABELS[PERIOD_LEVELS]))

# performance colour palette: red (poor) -> green (good)
perf_pal <- function(domain = c(0, 100))
  leaflet::colorNumeric("RdYlGn", domain = domain, na.color = "#e6e6e6")

# plotly red->amber->green colorscale for bars/heatmaps
RYG_SCALE <- list(c(0, "#d73027"), c(0.5, "#fee08b"), c(1, "#1a9850"))

# ── 4. Source helper functions ───────────────────────────────────────────────
source("R/value_boxes.R", local = TRUE)
source("R/plots.R",       local = TRUE)
source("R/map.R",         local = TRUE)
