#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# make_template.R  —  build a clean, fill-in KPI data-collection template
#
# Produces an Excel workbook that:
#   - pre-fills Region / Zone / Woreda / ADM3_PCODE from reference_geo_names.csv
#     (so next round's names already match the shapefile — no crosswalk needed),
#   - has a Population column,
#   - gives each indicator a Numerator + Denominator column plus a live % formula,
#   - includes an "Indicators" guide sheet (definitions + targets) and
#     an "Instructions" sheet.
#
# Output:
#   template/KPI_template_MASTER.xlsx        (all woredas, all 14 regions)
#   template/by_region/<Region>.xlsx         (one workbook per region)
# ----------------------------------------------------------------------------

suppressMessages({
  library(openxlsx); library(dplyr); library(readr); library(stringr); library(readxl)
})
here <- function(...) file.path(getwd(), ...)
source(here("scripts", "catalog.R"))
dir.create(here("template", "by_region"), recursive = TRUE, showWarnings = FALSE)

cat <- KPI_CATALOG |> arrange(order)
ref <- read_csv(here("reference_geo_names.csv"), show_col_types = FALSE)

# --- pull official numerator/denominator wording from a reference workbook ---
# (re-uses the same classify+walk as harmonize.R on the clean Benishangul sheet)
classify <- function(h) {
  hl <- str_squish(tolower(ifelse(is.na(h), "", h)))
  if (str_detect(hl, "^proportion|attack rate|case-?fatality rate")) return("value")
  if (str_detect(hl, "^total number|^number of expected"))            return("denom")
  if (str_detect(hl, "^number of|^district +for which|^total"))       return("num")
  "other"
}
defs <- tibble(code = cat$code, num_def = NA_character_, denom_def = NA_character_)
try({
  raw <- read_excel(here("raw", "2025-Wk28-52",
                         "Benishangul Gumuz week 28-52 of 2025  KPI report.xlsx"),
                    sheet = "Data Collection form", col_names = FALSE,
                    n_max = 1, .name_repair = "minimal")
  hdr <- as.character(unlist(raw[1, ])); roles <- vapply(hdr, classify, character(1))
  last_num <- NA; last_den <- NA
  for (j in seq_along(hdr)) {
    r <- roles[j]
    if (r == "num") last_num <- j else if (r == "denom") last_den <- j
    else if (r == "value") {
      hit <- cat$code[vapply(cat$pattern, function(p)
        str_detect(tolower(hdr[j]), regex(p, ignore_case = TRUE)), logical(1))]
      if (length(hit)) {
        k <- match(hit[1], defs$code)
        if (is.na(defs$num_def[k]))   defs$num_def[k]   <- if (!is.na(last_num)) hdr[last_num] else ""
        if (is.na(defs$denom_def[k])) defs$denom_def[k] <- if (!is.na(last_den)) hdr[last_den] else ""
      }
    }
  }
}, silent = TRUE)
cat <- cat |> left_join(defs, by = "code")

# --- column headers for the Data sheet --------------------------------------
geo_cols <- c("S.N","Region","Zone","Woreda","ADM3_PCODE","Population","Reporting_period")
ind_headers <- unlist(lapply(seq_len(nrow(cat)), function(i) {
  o <- cat$order[i]; L <- cat$label[i]
  c(sprintf("%d. %s — Numerator",   o, L),
    sprintf("%d. %s — Denominator", o, L),
    sprintf("%d. %s — %%",          o, L))
}))
all_headers <- c(geo_cols, ind_headers)

# --- styles -----------------------------------------------------------------
hdr_geo  <- createStyle(fgFill = "#1f4e79", fontColour = "white", textDecoration = "bold",
                        halign = "center", valign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
hdr_num  <- createStyle(fgFill = "#2e75b6", fontColour = "white", textDecoration = "bold",
                        halign = "center", valign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
hdr_pct  <- createStyle(fgFill = "#548235", fontColour = "white", textDecoration = "bold",
                        halign = "center", valign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
fill_lock<- createStyle(fgFill = "#f2f2f2")            # pre-filled geo (don't edit)
pct_sty  <- createStyle(numFmt = "0.0", fgFill = "#eaf1e3")

build_one <- function(geo, path, title) {
  wb <- createWorkbook()

  ## ---- Instructions ----
  addWorksheet(wb, "Instructions")
  instr <- c(
    title, "",
    "HOW TO FILL THIS TEMPLATE",
    "1. Use the 'Data' sheet. Each row is one woreda (pre-filled, grey columns A-E).",
    "2. Do NOT edit Region/Zone/Woreda/ADM3_PCODE — they are the standard names",
    "   that match the national shapefile, so your data maps automatically.",
    "3. Enter Population (column F) and the Reporting_period (e.g. 2025-W28-52).",
    "4. For each indicator enter the Numerator and Denominator counts.",
    "   The '%' column is calculated automatically — leave it alone.",
    "5. If a woreda did not report an indicator, leave its cells BLANK.",
    "   Enter 0 only when the true value is zero (e.g. zero outbreaks).",
    "6. See the 'Indicators' sheet for what each numerator/denominator means.",
    "7. A woreda missing from the list (new or split since 2021)? Add a row and",
    "   type its Region/Zone/Woreda; leave ADM3_PCODE blank for us to fill.",
    "", "Generated by make_template.R")
  writeData(wb, "Instructions", instr)

  ## ---- Indicators guide ----
  addWorksheet(wb, "Indicators")
  guide <- cat |> transmute(`#` = order, Code = code, `Core function` = group,
                            Indicator = label, Type = value_type, `Target %` = target,
                            `Numerator (count)` = num_def, `Denominator (count)` = denom_def)
  writeData(wb, "Indicators", guide, headerStyle = hdr_geo)
  setColWidths(wb, "Indicators", 1:8, c(4,16,14,46,11,9,52,52))
  freezePane(wb, "Indicators", firstActiveRow = 2)

  ## ---- Data ----
  addWorksheet(wb, "Data")
  dat <- geo |> transmute(S.N = row_number(), Region = region, Zone = zone,
                          Woreda = woreda, ADM3_PCODE = adm3_pcode,
                          Population = NA_real_, Reporting_period = NA_character_)
  # add empty indicator columns
  for (h in ind_headers) dat[[h]] <- NA_real_
  writeData(wb, "Data", dat, startRow = 1, headerStyle = hdr_geo)

  # header styling per block
  addStyle(wb, "Data", hdr_geo, rows = 1, cols = 1:length(geo_cols), gridExpand = TRUE)
  nrows <- nrow(dat)
  for (i in seq_len(nrow(cat))) {
    base <- length(geo_cols) + (i - 1) * 3
    addStyle(wb, "Data", hdr_num, rows = 1, cols = base + 1:2, gridExpand = TRUE)
    addStyle(wb, "Data", hdr_pct, rows = 1, cols = base + 3, gridExpand = TRUE)
    # live % formula: =IF(denom>0,100*num/denom,"")
    numL <- int2col(base + 1); denL <- int2col(base + 2)
    f <- sprintf('IF(%s%d>0,100*%s%d/%s%d,"")',
                 denL, 2:(nrows+1), numL, 2:(nrows+1), denL, 2:(nrows+1))
    writeFormula(wb, "Data", f, startCol = base + 3, startRow = 2)
    addStyle(wb, "Data", pct_sty, rows = 2:(nrows+1), cols = base + 3, gridExpand = TRUE)
  }
  # grey out pre-filled geo cells
  addStyle(wb, "Data", fill_lock, rows = 2:(nrows+1), cols = 1:5, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "Data", 1:length(all_headers),
               c(5, 14, 16, 22, 12, 12, 16, rep(15, length(ind_headers))))
  setColWidths(wb, "Data", seq(length(geo_cols)+3, length(all_headers), by = 3), 10) # % cols
  freezePane(wb, "Data", firstActiveRow = 2, firstActiveCol = 6)

  saveWorkbook(wb, path, overwrite = TRUE)
}

# nicer region names for the frame (use reference names as-is)
ref2 <- ref |> transmute(region, zone, woreda, adm3_pcode = ADM3_PCODE) |>
  arrange(region, zone, woreda)

# master (all regions) + per-region
build_one(ref2, here("template", "KPI_template_MASTER.xlsx"),
          "PHEM Woreda KPI — Data Collection Template (ALL regions)")
for (rg in sort(unique(ref2$region))) {
  g <- ref2 |> filter(region == rg)
  build_one(g, here("template", "by_region", paste0(gsub("[^A-Za-z0-9]+","_",rg), ".xlsx")),
            paste0("PHEM Woreda KPI — Data Collection Template (", rg, ")"))
}

# --- CSV versions (single fillable sheet; no formulas) ----------------------
csvdir <- here("template", "csv"); dir.create(file.path(csvdir,"by_region"),
                                              recursive = TRUE, showWarnings = FALSE)
make_csv <- function(geo, path) {
  d <- geo |> transmute(S.N = row_number(), Region = region, Zone = zone,
                        Woreda = woreda, ADM3_PCODE = adm3_pcode,
                        Population = NA_real_, Reporting_period = NA_character_)
  for (i in seq_len(nrow(cat))) {
    d[[sprintf("%d_%s_Num",   cat$order[i], cat$code[i])]] <- NA_real_
    d[[sprintf("%d_%s_Denom", cat$order[i], cat$code[i])]] <- NA_real_
  }
  write_csv(d, path, na = "")
}
make_csv(ref2, file.path(csvdir, "KPI_template_MASTER.csv"))
for (rg in sort(unique(ref2$region)))
  make_csv(filter(ref2, region == rg),
           file.path(csvdir, "by_region", paste0(gsub("[^A-Za-z0-9]+","_",rg), ".csv")))
# indicator guide as CSV too
write_csv(cat |> transmute(order, code, group, label, value_type, target,
                           numerator = num_def, denominator = denom_def),
          file.path(csvdir, "indicator_guide.csv"))

message(sprintf("Templates: template/KPI_template_MASTER.xlsx + %d per-region (xlsx & csv) + template/csv/",
                length(unique(ref2$region))))
