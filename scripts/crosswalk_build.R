#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# crosswalk_build.R  —  map KPI reported names -> shapefile ADM3_PCODE
#
# Uses the user's cleaning key "All GIS names.xlsx" (Woreda = messy reported
# name, Gname = standard shapefile name) + reference_geo_names.csv (authoritative
# region/zone/woreda/ADM3_PCODE, 1082 rows). Chain:
#     reported woreda --key--> Gname --reference (region-scoped)--> ADM3_PCODE
#
# Special handling:
#   - Addis Ababa: data is per sub-city, so we map the ZONE (sub-city) to the
#     11 Addis ADM3 units (sub-city IS the adm3 unit there).
#   - ambiguous key entries (one reported name -> several Gname): disambiguate
#     by region, then by zone similarity.
#   - aggregate rows ("Total", "Regional cumulative", region names): dropped.
#   - residual: fuzzy match within region; flagged needs_review.
#
# Output (output/):
#   crosswalk_woreda.csv     region/zone/woreda + adm3_pcode + method + review
#   crosswalk_unmatched.csv  rows still needing a manual P-code
# ----------------------------------------------------------------------------

suppressMessages({
  library(sf); library(dplyr); library(stringr)
  library(readr); library(tidyr); library(purrr); library(readxl)
})

KEY_XLSX <- "All GIS names.xlsx"
REF_CSV  <- "reference_geo_names.csv"
here <- function(...) file.path(getwd(), ...)

# KPI region -> reference_geo_names region name(s)
REGION_XWALK <- list(
  "Afar"="Afar","Amhara"="Amhara","Benishangul Gumuz"="Benishangul Gumz",
  "Dire Dawa"="Dire Dawa","Gambella"="Gambela","Harari"="Harari",
  "Addis Ababa"="Addis Ababa","Oromia"="Oromia","Sidama"="Sidama","Somali"="Somali",
  # SER reported only the South Ethiopia Region (2021 "Southern Ethiopi").
  "South Ethiopia"="Southern Ethiopi",
  # Central Ethiopia & Tigray reported only in the earliest period (Wk03-15);
  # both match the 2021 reference region names exactly.
  "Central Ethiopia"="Central Ethiopia","Tigray"="Tigray")

# manual overrides for woredas missing/spelled differently in the key.
# reported woreda (as in the KPI data) -> reference woreda name (resolved to a
# pcode within the same region). Edit this as new residuals appear.
MANUAL_OVERRIDE <- tibble::tribble(
  ~region,              ~woreda,                        ~ref_woreda,
  "Benishangul Gumuz",  "Wonbera",                      "Wembera",
  "Sidama",             "Boricha  WoHO",                "Boricha",
  "South Ethiopia",     "Oyda",                         "O'yida",
  "Gambella",           "Gambella Town Adminstration",  "Gambela town",
  "Gambella",           "Gambella Woreda",              "Gambela town",
  "Gambella",           "Itang Special woreda",         "Itang"
)
# NOTE: both "Gambella Town Adminstration" and "Gambella Woreda" map to the same
# woreda (Gambela town) per user instruction, so they combine in the P-code rollup.

norm <- function(x) x |> tolower() |> str_replace_all("[^a-z0-9 ]"," ") |> str_squish()
sim  <- function(a, b) 1 - as.numeric(adist(a, b)) / pmax(nchar(a), nchar(b), 1)
is_aggregate <- function(x) {
  xl <- norm(x)
  xl %in% c("total","regional cumulative","region","harari","grand total","sum") |
    str_detect(xl, "cumu|grand total|^total")  # 'cumu' = cumulative/cumuative typo
}
`%||%` <- function(a, b)
  if (is.null(a) || length(a)==0 || (length(a)==1 && is.na(a))) b else a

# ---- authoritative reference: region/zone/woreda/pcode (1082) ---------------
ref <- read_csv(REF_CSV, show_col_types = FALSE) |>
  transmute(adm3_pcode = ADM3_PCODE, adm3_en = woreda,
            adm1_en = region, adm2_en = zone, g_norm = norm(woreda))

# ---- cleaning key: reported -> Gname ---------------------------------------
key <- read_excel(KEY_XLSX, .name_repair = "minimal") |>
  transmute(w_norm = norm(Woreda), g_norm = norm(Gname)) |>
  filter(w_norm != "", g_norm != "") |> distinct()

# ---- KPI names -------------------------------------------------------------
m   <- read_csv(here("output", "master_woreda.csv"), show_col_types = FALSE)
kpi <- m |> distinct(region, zone, woreda) |> filter(!is.na(woreda))

pick_in_region <- function(cands, region, zone) {
  # cands: data frame of candidate pcode rows; choose best by region then zone
  adm1ok <- REGION_XWALK[[region]] %||% region
  inreg  <- cands |> filter(adm1_en %in% adm1ok)
  use    <- if (nrow(inreg) > 0) inreg else cands
  if (nrow(use) == 1) return(use)
  zn <- norm(zone %||% "")
  use |> mutate(zs = if (zn == "") 0 else sim(zn, norm(adm2_en))) |>
    slice_max(zs, n = 1, with_ties = FALSE) |> select(-zs)
}

match_one <- function(region, zone, woreda) {
  blank <- tibble(adm3_pcode=NA_character_, adm3_en=NA_character_,
                  adm1_en=NA_character_, adm2_en=NA_character_,
                  score=NA_real_, method="unmatched")
  if (is_aggregate(woreda)) return(mutate(blank, method = "aggregate_drop"))

  # 0) manual override
  ov <- MANUAL_OVERRIDE |> filter(region == !!region, woreda == !!woreda)
  if (nrow(ov)) {
    cands <- ref |> filter(g_norm == norm(ov$ref_woreda[1]))
    best  <- pick_in_region(cands, region, zone)
    if (nrow(best))
      return(tibble(adm3_pcode=best$adm3_pcode, adm3_en=best$adm3_en,
                    adm1_en=best$adm1_en, adm2_en=best$adm2_en,
                    score=1, method="override"))
  }

  # ADDIS: map sub-city (zone) -> Addis reference woreda (= sub-city)
  if (region == "Addis Ababa") {
    pool <- ref |> filter(adm1_en == "Addis Ababa") |>
      mutate(s = sim(norm(zone %||% woreda), g_norm)) |>
      slice_max(s, n = 1, with_ties = FALSE)
    # only 11 known sub-cities -> the closest match is reliable; accept it
    return(tibble(adm3_pcode=pool$adm3_pcode, adm3_en=pool$adm3_en,
                  adm1_en=pool$adm1_en, adm2_en=pool$adm2_en,
                  score=round(pool$s,3), method="addis_subcity"))
  }

  wn <- norm(woreda)
  # 1) via cleaning key
  gn <- key$g_norm[key$w_norm == wn]
  if (length(gn)) {
    cands <- ref |> filter(g_norm %in% gn)
    if (nrow(cands)) {
      best <- pick_in_region(cands, region, zone)
      return(tibble(adm3_pcode=best$adm3_pcode, adm3_en=best$adm3_en,
                    adm1_en=best$adm1_en, adm2_en=best$adm2_en,
                    score=1, method="key"))
    }
  }
  # 2) direct exact on reference woreda (in region)
  adm1ok <- REGION_XWALK[[region]] %||% region
  pool <- ref |> filter(adm1_en %in% adm1ok)
  if (nrow(pool) == 0) pool <- ref
  exact <- pool |> filter(g_norm == wn)
  if (nrow(exact)) {
    best <- pick_in_region(exact, region, zone)
    return(tibble(adm3_pcode=best$adm3_pcode, adm3_en=best$adm3_en,
                  adm1_en=best$adm1_en, adm2_en=best$adm2_en,
                  score=1, method="ref_exact"))
  }
  # 3) fuzzy on shapefile (in region)
  fz <- pool |> mutate(s = sim(wn, g_norm)) |> slice_max(s, n=1, with_ties=FALSE)
  meth <- if (fz$s >= 0.85) "fuzzy_high" else "unmatched"
  tibble(adm3_pcode=if (meth=="unmatched") NA else fz$adm3_pcode,
         adm3_en=if (meth=="unmatched") NA else fz$adm3_en,
         adm1_en=fz$adm1_en, adm2_en=fz$adm2_en,
         score=round(fz$s,3), method=meth)
}

# ---- apply human-approved overrides from crosswalk_overrides.csv ------------
# This file (project root) is YOURS to edit: fill the `approved_ref_woreda`
# column for any reported name and re-run. Approved rows are merged into
# MANUAL_OVERRIDE here, before matching, so they take effect immediately.
OVR_CSV <- "crosswalk_overrides.csv"
if (file.exists(OVR_CSV)) {
  appr <- suppressMessages(read_csv(OVR_CSV, show_col_types = FALSE)) |>
    filter(!is.na(approved_ref_woreda), str_squish(approved_ref_woreda) != "") |>
    transmute(region, woreda = reported_woreda, ref_woreda = approved_ref_woreda)
  if (nrow(appr)) {
    MANUAL_OVERRIDE <- bind_rows(MANUAL_OVERRIDE, appr) |>
      distinct(region, woreda, .keep_all = TRUE)
    cat("applied", nrow(appr), "approved override(s) from", OVR_CSV, "\n")
  }
}

xwalk <- kpi |>
  mutate(res = pmap(list(region, zone, woreda), match_one)) |>
  unnest(res) |>
  mutate(needs_review = !method %in%
           c("key","ref_exact","fuzzy_high","addis_subcity","override","aggregate_drop")) |>
  arrange(needs_review, region, zone, woreda)

write_csv(xwalk, here("output", "crosswalk_woreda.csv"))
write_csv(filter(xwalk, needs_review), here("output", "crosswalk_unmatched.csv"))

# ---- refresh the editable override to-do list ------------------------------
# Keep every row you have already approved (so a resolved name never reverts),
# and add any name still needing review, with a fuzzy suggestion to start from.
suggest1 <- function(rg, wd) {
  tgt  <- REGION_XWALK[[rg]] %||% rg
  pool <- ref |> filter(adm1_en %in% tgt)
  if (!nrow(pool)) return(tibble(suggested_ref_woreda = NA_character_, score = NA_real_))
  s <- sim(norm(wd), pool$g_norm)
  tibble(suggested_ref_woreda = pool$adm3_en[which.max(s)], score = round(max(s), 2))
}
todo <- xwalk |> filter(needs_review) |> distinct(region, zone, woreda) |>
  mutate(s = pmap(list(region, woreda), suggest1)) |> unnest(s) |>
  transmute(region, zone, reported_woreda = woreda,
            suggested_ref_woreda, score, approved_ref_woreda = "")
prev_appr <- tibble(region = character(), zone = character(), reported_woreda = character(),
                    suggested_ref_woreda = character(), score = numeric(),
                    approved_ref_woreda = character())
if (file.exists(OVR_CSV)) {
  prev_appr <- suppressMessages(read_csv(OVR_CSV, show_col_types = FALSE)) |>
    filter(!is.na(approved_ref_woreda), str_squish(approved_ref_woreda) != "")
  todo <- anti_join(todo, prev_appr, by = c("region", "reported_woreda"))
}
bind_rows(prev_appr, todo) |>
  arrange(desc(approved_ref_woreda == ""), region, reported_woreda) |>
  write_csv(OVR_CSV)
cat("override to-do refreshed:", OVR_CSV, "—",
    sum(todo$approved_ref_woreda == ""), "still to resolve,",
    nrow(prev_appr), "approved\n")

# ---- summary ---------------------------------------------------------------
cat("\n=== crosswalk summary ===\n")
print(xwalk |> count(method) |> arrange(desc(n)))
auto <- sum(!xwalk$needs_review & xwalk$method != "aggregate_drop")
cat("\nmapped to a P-code:", auto, "/",
    sum(xwalk$method != "aggregate_drop"), " reportable rows\n")
cat("aggregate rows dropped:", sum(xwalk$method=="aggregate_drop"), "\n")
cat("needs manual review   :", sum(xwalk$needs_review), "\n")
cat("\nby region:\n")
print(xwalk |> group_by(region) |>
        summarise(rows=n(), mapped=sum(!needs_review & method!="aggregate_drop"),
                  review=sum(needs_review), .groups="drop"))
cat("\n-> output/crosswalk_woreda.csv\n-> output/crosswalk_unmatched.csv\n")
