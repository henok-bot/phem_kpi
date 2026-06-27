#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# catalog.R  —  canonical PHEM KPI indicator catalog + region source config
#
# Sourced by harmonize.R. Two objects:
#   KPI_CATALOG   : the canonical indicators (code, group, label, type, pattern)
#   REGION_CONFIG : per-workbook reading config (file, sheet, region, period...)
# ----------------------------------------------------------------------------

# --- canonical indicators ---------------------------------------------------
# `pattern` is a perl regex matched (case-insensitive) against the VALUE column
# header in each region's data sheet. district/facility wording is tolerated.
# value_type: "proportion" (0-100 %) or "rate" (kept as numerator/denominator).
KPI_CATALOG <- tibble::tribble(
  ~order, ~code,            ~group,          ~value_type,  ~label, ~pattern,
   1, "EBS_PPV",        "Identifying",   "proportion", "Confirmed outbreaks among suspected events (EBS sensitivity)",            "confirmed outbreaks among suspected",
   2, "EBS_DETECT",     "Identifying",   "proportion", "Outbreaks detected through event-based surveillance",                     "outbreaks detected through event-based",
   3, "DETECT_7D",      "Identifying",   "proportion", "Outbreaks detected within 7 days",                                        "outbreaks detected within 7 days",
   4, "RPT_TIMELY",     "Reporting",     "proportion", "Surveillance reports submitted on time (timeliness)",                     "submitted surveillance reports(?!.*complet)(?!.*through dhis)",
   5, "RPT_COMPLETE",   "Reporting",     "proportion", "Surveillance reports submitted completely (completeness)",                "submitted surveillance reports +complet|submitted.*surveillance reports.*complet",
   6, "CB_ELIM",        "Reporting",     "proportion", "Case-based reporting: elimination/eradication diseases",                  "cases of diseases targeted for elimination",
   7, "CB_EVENTS",      "Reporting",     "proportion", "Case-based reporting: selected events/conditions",                        "events or conditions selected for case-based",
   8, "NOTIFY_24H",     "Reporting",     "proportion", "Suspected outbreaks notified within 24 hours",                            "within 24 hours of surpassing",
   9, "NOTIFY_30M",     "Reporting",     "proportion", "Immediately reportable events notified within 30 minutes",                "within 30 minutes of surpassing",
  10, "LINEGRAPH",      "Analysis",      "proportion", "Current line graph available for priority diseases",                      "line graph is available",
  11, "OBR_ANALYZED",   "Analysis",      "proportion", "Outbreak reports with analyzed case-based data",                          "investigated outbreaks that include analyzed",
  12, "LAB_7D",         "Laboratory",    "proportion", "Outbreaks with laboratory results within 7 days",                         "laboratory results within 7 days",
  13, "RESP_2448",      "Response",      "proportion", "Confirmed outbreaks with response within 24-48 hours",                    "public health response within 24 to 48",
  14, "ATTACK_RATE",    "Response",      "rate",       "Attack rate per outbreak (per population at risk)",                       "attack rate for each outbreak",
  15, "CFR",            "Response",      "rate",       "Case-fatality rate per epidemic-prone disease",                           "case-?fatality rate",
  16, "EPI_DETECT_REG", "Analysis",      "proportion", "Epidemics detected at regional level from district data",                 "epidemics detected by the regional",
  17, "EPRP_PLAN",      "Preparedness",  "proportion", "Districts with EPRP (preparedness & response) plans",                     "eprp|epr plan",
  18, "RISK_MAP",       "Preparedness",  "proportion", "Districts conducting risk & resource mapping",                            "resources mapping|risk.*mapping",
  19, "EPR_FUNDS",      "Preparedness",  "proportion", "Districts with EPR funds / budget line",                                  "funds for emergency preparedness|budget",
  20, "CONTINGENCY",    "Preparedness",  "proportion", "Districts with contingency stocks (3-6 months)",                          "contingency stocks",
  21, "PHEM_OFFICER",   "Preparedness",  "proportion", "Health facilities with PHEM officer",                                     "with phem officer",
  22, "DHIS2_RPT",      "Preparedness",  "proportion", "Districts reporting weekly via DHIS2",                                    "through dhis ?-?2|dhis2",
  23, "COORD_PLATFORM", "Preparedness",  "proportion", "Districts with functional coordination platform / RRT",                   "coordination platform"
)

# Performance target = 80% benchmark for all proportion indicators (editable
# here if needed). Rates have no target.
KPI_CATALOG$target <- ifelse(KPI_CATALOG$value_type == "proportion", 80, NA_real_)

# --- per-region source config (MULTI-PERIOD) --------------------------------
# header_row: 1-based row holding the column names.
# region: canonical region name written into the master.
# dir/period: raw/<period> folder + canonical epi-week period label.
# notes: free text -> data-quality log.
#
# One block per reporting period. harmonize.R loops over EVERY row (all periods)
# and stacks them into the master by `period`; roll()/spatialize already group
# by period. To add a future quarter: drop its workbooks in raw/<new-period>/
# and append a block here.
#
# Period coverage note: Wk28-52 spans TWO quarters (weeks 28-52); Wk03-15 and
# Wk16-27 are each a single quarter. Proportion KPIs are ratios so they stay
# comparable across periods; only raw counts scale with period length.

# Period 1 — weeks 3-15, 2025 (Q1; 13 regions incl. Tigray & Central Ethiopia)
cfg_wk0315 <- tibble::tribble(
  ~file,                                                          ~sheet,                  ~header_row, ~region,             ~notes,
  "Addis Ababa Regional KPI_Week  3- Week 15, 2025.xlsx",         "Sheet1",                 1L,        "Addis Ababa",       "reported at SUB-CITY level (11 rows); 'District' col = woreda count, not a name; total rows (zone AA/aa) dropped",
  "Afar_Regional_KPI Week 3- Week 15, 2025.xlsx",                 "Sheet4",                 1L,        "Afar",              "",
  "Amhara_Regional_KPI week 3-week  15, 2025.xlsx",               "Sheet4",                 1L,        "Amhara",            "has Population col",
  "Benishangul_Gumuz_ KPI week_3_15 , of_2025.xlsx",              "Data Collection form",   1L,        "Benishangul Gumuz", "",
  "CER_Regional_KPI_Week 3- week 15, 2025.xlsx",                  "Sheet4",                 2L,        "Central Ethiopia",  "title row offset (row1 = 'Source of Information'); did NOT report in Wk28-52",
  "Dire Diwa  KPI , week 3- week 15, 2025.xlsx",                  "data collection tool",   1L,        "Dire Dawa",         "facility/PHCU level; no title row this period",
  "Gambella_Regional_KPI_EPI_Week_13_15_2025.xlsx",              "Gambella KPI Q3 Report",  1L,        "Gambella",          "",
  "Harari_KPI Week 3- 15, 2025.xlsx",                            "Harari KPI",             1L,        "Harari",            "",
  "Oromia_Region KPI_Indicator Week 3 - Week 15,  updated.xlsx", "Data collection form",   1L,        "Oromia",            "geo cols reordered; row2 is a sub-header (dropped)",
  "SER KPI week 3- week, 15 2025.xlsx",                          "Q3 PHEM KPI REPORT",     1L,        "South Ethiopia",    "use Q3 sheet",
  "Sidama Region  week 3-week 15, 2025.xlsx",                    "Sheet4",                 1L,        "Sidama",            "Region/Zone merged-down (NA on continuation rows)",
  "Somali_R_KPI_2025_Week 3-Week 15, 2025.xlsx",                "Sheet4",                 1L,        "Somali",            "",
  "Tigray KPI week -week 15, 2025.xlsx",                        "Woreda Level KPI",       1L,        "Tigray",            "did NOT report in Wk28-52"
) |> dplyr::mutate(dir = "raw/2025-Wk03-15", period = "2025-Wk03-15")

# Period 2 — weeks 16-27, 2025 (Q2; 11 regions)
cfg_wk1627 <- tibble::tribble(
  ~file,                                                          ~sheet,                  ~header_row, ~region,             ~notes,
  "Addis ababa KPI  week 16-week 27, 2025.xlsx",                 "Sheet1",                 1L,        "Addis Ababa",       "woreda level within sub-city; phantom rows to row 1,048,576 (blank rows dropped)",
  "Afar_Regional  week 16-Week 27, 2025.xlsx",                   "Sheet4",                 1L,        "Afar",              "",
  "Amhara_Regional_week 16-week 27, 2025.xlsx",                  "Sheet4",                 1L,        "Amhara",            "has Population col",
  "Benishangul_Gumuz_Week 16-Week 27, 2025.xlsx",               "Data Collection form",   1L,        "Benishangul Gumuz", "a duplicate '4th_quarter' copy was removed (byte-identical)",
  "Dire diwa week 16-week 27, 2025.xlsx",                       "data collection tool",   2L,        "Dire Dawa",         "title row offset; facility/PHCU level",
  "Gambella Week 16- week 27, 2025.xlsx",                       "Gambella KPI Q3 Report",  1L,        "Gambella",          "",
  "Harari_ Week 16-week, 27 , 2025.xlsx",                       "Harari KPI",             1L,        "Harari",            "",
  "Oromia_Region Week 16-week 27, 2025.xlsx",                   "Data collection form",   1L,        "Oromia",            "geo cols reordered; row2 is a sub-header (dropped)",
  "SER Week 16-week 27, 2025.xlsx",                             "Q4 PHEM KPI REPORT",     1L,        "South Ethiopia",    "use Q4 sheet",
  "Sidama Region WEEK 16-WEEK 27, 2025.xlsx",                   "Sheet4",                 1L,        "Sidama",            "Region/Zone merged-down (NA on continuation rows)",
  "Somali_R_KPI_july_25,2025_Week 16-week 27, 2025.xlsx",       "Sheet4",                 1L,        "Somali",            "row2 'Regional cumulative' dropped"
) |> dplyr::mutate(dir = "raw/2025-Wk16-27", period = "2025-Wk16-27")

# Period 3 — weeks 28-52, 2025 (Q3+Q4, the original analysis period; 11 regions)
cfg_wk2852 <- tibble::tribble(
  ~file,                                                                          ~sheet,                  ~header_row, ~region,            ~notes,
  "Afar_Regional_KPI_EPI_Week_28_52_2025_indictors_and_data_collection.xlsx",     "Gambella KPI Q3 Report", 1L,        "Afar",             "sheet mislabeled 'Gambella'; data is Afar",
  "Amhara_Regional_KPI_Jan 27_2026_indictors_and_data.xlsx",                       "Sheet4",                 1L,        "Amhara",           "has Population col; filename says Jan 2026",
  "Benishangul Gumuz week 28-52 of 2025  KPI report.xlsx",                         "Data Collection form",   1L,        "Benishangul Gumuz","reference template",
  "DDAHB, week 28-52, 2025 KPI report.xlsx",                                       "data collection tool",   2L,        "Dire Dawa",        "title row offset; facility/PHCU level",
  "Gambella_Regional_KPI_EPI_Week_28_52_2025_indictors_and_data_collection.xlsx", "Gambella KPI Q3 Report",  1L,       "Gambella",         "",
  "Harari_2nd_Qrtr_RHB_Surveillance_KPI_2018.xlsx",                               "Harari KPI",             1L,        "Harari",           "filename uses EC 2018 = Q2",
  "kpi new addis ababa1 (2).xlsx",                                                 "Sheet1",                 1L,        "Addis Ababa",      "woreda level within sub-city; phantom rows",
  "Oromia_Region_KPI_Indicators_2nd_quarter_2018From_Week_28_52,2025.xlsx",       "Oromia Region 2nd Q KPI ", 1L,      "Oromia",           "geo columns reordered",
  "SER Regional PHEM- KPI_week-28-52,2025.xlsx",                                   "Q4 PHEM KPI REPORT",     1L,        "South Ethiopia",   "use Q4 sheet, not raw facility Sheet1",
  "Sidama Region KPI performance of  week 27-52,2025.xlsx",                        "Sheet4",                 1L,        "Sidama",           "filename says Wk27-52",
  "Somali_R_KPI_february,2026_indictors_and_data_collection_formas.xlsx",         "Sheet4",                 1L,        "Somali",           "filename says Feb 2026"
) |> dplyr::mutate(dir = "raw/2025-Wk28-52", period = "2025-Wk28-52")

REGION_CONFIG <- dplyr::bind_rows(cfg_wk0315, cfg_wk1627, cfg_wk2852)

# reporting level: woreda everywhere except Dire Dawa, which reported per health
# facility (PHCU/hospital) with the facility name in the region column.
REGION_CONFIG$level <- ifelse(REGION_CONFIG$region == "Dire Dawa", "facility", "woreda")

# Period ordering (oldest -> newest) + the period the single-period report and
# figures focus on. analyze.R / analyze_depth.R filter to FOCUS_PERIOD so the
# main report stays pinned to ONE period; analyze_trends.R uses all of them.
#
# TO ADD THE NEXT QUARTER (reproducible workflow):
#   1. drop its workbooks in raw/<new-period>/ (e.g. raw/2025-Wk53-65/),
#   2. add a cfg_<period> tribble block above and bind it into REGION_CONFIG,
#   3. append the new period label to PERIOD_LEVELS (+ PERIOD_LABELS),
#   4. rerun scripts/run_all.R.
# FOCUS_PERIOD auto-advances to the newest period, so the single-period report
# tracks the latest quarter; run_all archives each render under its period so
# previous reports are preserved. The trends report extends automatically.
PERIOD_LEVELS <- c("2025-Wk03-15", "2025-Wk16-27", "2025-Wk28-52")
PERIOD_LABELS <- c("2025-Wk03-15" = "Q1 · Wk 3-15",
                   "2025-Wk16-27" = "Q2 · Wk 16-27",
                   "2025-Wk28-52" = "Q3-Q4 · Wk 28-52")
FOCUS_PERIOD  <- tail(PERIOD_LEVELS, 1)   # newest period = the single-period report
