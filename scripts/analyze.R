#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# analyze.R  —  standalone analysis figures + tables for slides/reports
#
# Reads output/*.csv (run harmonize.R first) and writes to:
#   exports/figures/  PNGs ready to drop into PowerPoint
#   exports/tables/   CSVs of the same numbers
#
# Performance colour ramp: red (poor) -> yellow -> green (good), so weak KPIs
# read as red. (The grey->orange->red count ramp is reserved for case/death
# count maps, not performance %.)
# ----------------------------------------------------------------------------

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(ggplot2); library(forcats); library(scales)
})

here <- function(...) file.path(getwd(), ...)
FIG <- here("exports", "figures"); TAB <- here("exports", "tables")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB, recursive = TRUE, showWarnings = FALSE)

source(here("scripts", "catalog.R"))   # FOCUS_PERIOD, PERIOD_LABELS
catalog  <- read_csv(here("output", "indicator_catalog.csv"), show_col_types = FALSE)
region   <- read_csv(here("output", "kpi_region.csv"),   show_col_types = FALSE)
national <- read_csv(here("output", "kpi_national.csv"), show_col_types = FALSE)

# This report is single-period: pin every figure/table to the focus (newest)
# period. The cross-period comparison lives in analyze_trends.R / kpi_trends.qmd.
region   <- filter(region,   period == FOCUS_PERIOD)
national <- filter(national, period == FOCUS_PERIOD)
PSUB <- unname(PERIOD_LABELS[FOCUS_PERIOD]); if (is.na(PSUB)) PSUB <- FOCUS_PERIOD

grp_levels <- c("Identifying","Reporting","Analysis","Laboratory","Response","Preparedness")
lab_order  <- catalog$label[order(catalog$order)]
perf_fill  <- function(...) scale_fill_gradientn(
  colours = c("#d73027","#fc8d59","#fee08b","#d9ef8b","#1a9850"),
  limits = c(0,100), oob = squish, name = "% (capped)", ...)

prop <- function(df) filter(df, value_type == "proportion")

# tidytext-style per-facet ordering (defined inline; tidytext not installed)
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
}
scale_x_reordered <- function(...) {
  ggplot2::scale_x_discrete(labels = function(x) sub("___.+$", "", x), ...)
}

theme_kpi <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey40", hjust = 0))

# ---- TABLE 1: region x indicator (wide %) ----------------------------------
wide <- prop(region) |>
  mutate(label = factor(label, levels = lab_order)) |>
  select(region, label, value_pct_capped) |>
  pivot_wider(names_from = region, values_from = value_pct_capped) |>
  arrange(label)
natvec <- prop(national) |> select(label, NATIONAL = value_pct_capped)
wide <- left_join(wide, natvec, by = "label") |> arrange(factor(label, levels = lab_order))
write_csv(wide, file.path(TAB, "region_x_indicator_pct.csv"))
write_csv(national, file.path(TAB, "national_summary.csv"))

# reporting completeness (region x indicator, % of woredas that reported)
rep_wide <- prop(region) |>
  mutate(label = factor(label, levels = lab_order)) |>
  select(region, label, pct_reported) |>
  pivot_wider(names_from = region, values_from = pct_reported) |>
  left_join(prop(national) |> select(label, NATIONAL = pct_reported), by = "label") |>
  arrange(factor(label, levels = lab_order)) |> rename(Indicator = label)
write_csv(rep_wide, file.path(TAB, "region_x_indicator_reporting.csv"))

# ---- FIGURE 1: heatmap region x indicator (+ National column) ---------------
# Region + National in one frame; National pushed to the far right.
hm <- bind_rows(
    prop(region)   |> select(region, label, value_pct_capped, pct_reported),
    prop(national) |> mutate(region = "NATIONAL") |>
      select(region, label, value_pct_capped, pct_reported)) |>
  left_join(select(catalog, label, order), by = "label") |>
  mutate(label = fct_reorder(str_wrap(label, 42), -order),
         region = factor(region, levels = c(setdiff(sort(unique(region)), "NATIONAL"),
                                            "NATIONAL")),
         # distinguish a genuine 0 from "not reported" (no computable value)
         cell = ifelse(is.na(value_pct_capped), "NR", as.character(round(value_pct_capped))))
p1 <- ggplot(hm, aes(region, label, fill = value_pct_capped)) +
  geom_tile(colour = "white", linewidth = .4) +
  geom_text(aes(label = cell), size = 2.6,
            colour = ifelse(hm$cell == "NR", "grey45", "black")) +
  geom_vline(xintercept = nlevels(hm$region) - 0.5, colour = "grey30", linewidth = .6) +
  perf_fill(na.value = "grey85") +
  labs(title = "PHEM KPI performance by region",
       subtitle = "Week 28-52, 2025  |  proportion indicators, % (capped at 100)  |  NR = not reported",
       x = NULL, y = NULL,
       caption = paste("Computed as 100 x summed numerator / summed denominator across reporting woredas.",
                       "\nGrey 'NR' = no computable value (denominator not reported); a numeric 0 is a true zero.")) +
  theme_kpi + theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave(file.path(FIG, "01_heatmap_region_indicator.png"), p1,
       width = 11.5, height = 9, dpi = 150, bg = "white")

# ---- FIGURE 1b: reporting completeness (region x indicator) -----------------
rr <- bind_rows(
    prop(region)   |> select(region, label, pct_reported),
    prop(national) |> mutate(region = "NATIONAL") |> select(region, label, pct_reported)) |>
  left_join(select(catalog, label, order), by = "label") |>
  mutate(label = fct_reorder(str_wrap(label, 42), -order),
         region = factor(region, levels = c(setdiff(sort(unique(region)), "NATIONAL"),
                                            "NATIONAL")))
p1b <- ggplot(rr, aes(region, label, fill = pct_reported)) +
  geom_tile(colour = "white", linewidth = .4) +
  geom_text(aes(label = round(pct_reported)), size = 2.6) +
  geom_vline(xintercept = nlevels(rr$region) - 0.5, colour = "grey30", linewidth = .6) +
  scale_fill_gradientn(colours = c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#1b7837"),
                       limits = c(0,100), oob = scales::squish, name = "% reported") +
  labs(title = "Reporting completeness by region",
       subtitle = "Share of woredas that actually filled in each indicator (Week 28-52, 2025)",
       x = NULL, y = NULL,
       caption = "Low values mean a region's performance for that indicator rests on few woredas — interpret with caution.") +
  theme_kpi + theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave(file.path(FIG, "01b_reporting_completeness.png"), p1b,
       width = 11.5, height = 9, dpi = 150, bg = "white")

# ---- FIGURE 2: PHEM core-function average by region -------------------------
cf <- prop(region) |>
  group_by(region, group) |>
  summarise(value = mean(value_pct_capped, na.rm = TRUE), .groups = "drop") |>
  mutate(group = factor(group, levels = grp_levels))
natcf <- prop(national) |>
  group_by(group) |> summarise(nat = mean(value_pct_capped, na.rm = TRUE), .groups="drop") |>
  mutate(group = factor(group, levels = grp_levels))
p2 <- ggplot(cf, aes(fct_reorder(region, value), value, fill = value)) +
  geom_col() + coord_flip() + facet_wrap(~group, ncol = 3) +
  geom_hline(data = natcf, aes(yintercept = nat), linetype = "dashed", colour = "grey30") +
  perf_fill() +
  labs(title = "Average performance by PHEM core function",
       subtitle = "Dashed line = national average",
       x = NULL, y = "Mean of indicator %", caption = "Week 28-52, 2025") +
  theme_kpi + theme(legend.position = "none")
ggsave(file.path(FIG, "02_corefunction_by_region.png"), p2,
       width = 11, height = 7, dpi = 150, bg = "white")

# ---- FIGURE 3: per-indicator region ranking (small multiples) --------------
pind <- prop(region) |>
  left_join(select(catalog, label, order), by = "label") |>
  mutate(label = fct_reorder(str_wrap(label, 34), order))
p3 <- ggplot(pind, aes(reorder_within(region, value_pct_capped, label),
                       value_pct_capped, fill = value_pct_capped)) +
  geom_col() + coord_flip() +
  facet_wrap(~label, scales = "free_y", ncol = 4) +
  scale_x_reordered() + perf_fill() +
  labs(title = "Region ranking per indicator", x = NULL, y = "%",
       caption = "Week 28-52, 2025  |  proportion indicators") +
  theme_kpi + theme(legend.position = "none",
                    strip.text = element_text(size = 7),
                    axis.text = element_text(size = 6))
ggsave(file.path(FIG, "03_per_indicator_ranking.png"), p3,
       width = 14, height = 16, dpi = 130, bg = "white", limitsize = FALSE)

# ---- FIGURE 4: region choropleths (adm1) for key indicators ----------------
# Map ALL 14 regions; the 3 that did not report (Tigray, Central Ethiopia,
# South West Ethiopia) are shown grey, not blank.
maps_ok <- requireNamespace("sf", quietly = TRUE)
if (maps_ok) {
  suppressMessages(library(sf))
  SHP_DIR <- "/data/r_projects_cloud/learnin_repo/EPHEM-weekly-cleaning - Alert/shapefile"
  reg_map <- tribble(
    ~region,              ~adm1_en,
    "Afar","Afar","Amhara","Amhara","Benishangul Gumuz","Benishangul Gumz",
    "Dire Dawa","Dire Dawa","Gambella","Gambela","Harari","Harari",
    "Addis Ababa","Addis Ababa","Oromia","Oromia","Sidama","Sidama",
    "Somali","Somali","South Ethiopia","Southern Ethiopia")   # SER = Southern Ethiopi only
  adm1 <- sf::read_sf(file.path(SHP_DIR, "eth_admbnda_adm1_csa_bofedb_2021.shp")) |>
    left_join(reg_map, by = c("ADM1_EN" = "adm1_en")) |>
    mutate(region = ifelse(is.na(region), paste0("(not reporting) ", ADM1_EN), region)) |>
    group_by(region) |> summarise(reporting = !grepl("not reporting", region[1]),
                                  .groups = "drop")

  key <- c("RPT_TIMELY","RPT_COMPLETE","RESP_2448","EPRP_PLAN",
           "NOTIFY_24H","LAB_7D")
  mapdat <- prop(region) |>
    left_join(select(catalog, indicator = code, order), by = "indicator") |>
    filter(indicator %in% key) |>
    mutate(label = fct_reorder(str_wrap(label, 30), order))
  # cross all map regions with the selected indicators so non-reporting regions
  # still draw (grey) in every panel
  base <- tidyr::crossing(region = adm1$region, label = levels(mapdat$label))
  msf <- adm1 |> right_join(base, by = "region") |>
    left_join(select(mapdat, region, label, value_pct_capped),
              by = c("region", "label")) |>
    mutate(label = factor(label, levels = levels(mapdat$label)))

  p4 <- ggplot(msf) +
    geom_sf(aes(fill = value_pct_capped), colour = "grey55", linewidth = .15) +
    facet_wrap(~label, ncol = 3) +
    perf_fill(na.value = "grey85") +
    labs(title = "KPI performance by region (selected indicators)",
         subtitle = "Week 28-52, 2025  |  3 regions (Tigray, Central Ethiopia, South West Ethiopia) did not report",
         caption = "Grey = region did not report this KPI system. Performance: red low, green high.") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(size = 8),
          plot.caption = element_text(colour = "grey40", hjust = 0))
  ggsave(file.path(FIG, "04_region_choropleths.png"), p4,
         width = 12, height = 8, dpi = 150, bg = "white")
  cat("map figure written\n")
} else cat("sf not available, skipped maps\n")

# ---- FIGURE 5: woreda-level choropleths (adm3, joined on P-code) ------------
wp_file <- here("output", "kpi_woreda_pcode.csv")
if (maps_ok && file.exists(wp_file)) {
  adm3 <- sf::read_sf(file.path(SHP_DIR, "eth_admbnda_adm3_csa_bofedb_2021.shp")) |>
    select(adm3_pcode = ADM3_PCODE)
  wp <- read_csv(wp_file, show_col_types = FALSE) |> filter(period == FOCUS_PERIOD)
  keyw <- c("RPT_TIMELY","RPT_COMPLETE","RESP_2448","LAB_7D","NOTIFY_24H","EPRP_PLAN")
  lab_for <- setNames(str_wrap(catalog$label, 30), catalog$code)
  ord_for <- setNames(catalog$order, catalog$code)
  # build one sf layer per indicator (keep ALL woredas so outlines always show)
  msf <- do.call(rbind, lapply(keyw, function(ind) {
    d <- wp |> filter(indicator == ind, value_type == "proportion") |>
      select(adm3_pcode, value_pct_capped)
    adm3 |> left_join(d, by = "adm3_pcode") |>
      mutate(label = lab_for[[ind]], ord = ord_for[[ind]])
  }))
  msf$label <- forcats::fct_reorder(msf$label, msf$ord)
  p5 <- ggplot(msf) +
    geom_sf(aes(fill = value_pct_capped), colour = "grey80", linewidth = .05) +
    facet_wrap(~label, ncol = 3) +
    scale_fill_gradientn(
      colours = c("#d73027","#fc8d59","#fee08b","#d9ef8b","#1a9850"),
      limits = c(0,100), oob = scales::squish, na.value = "grey92",
      name = "% (capped)") +
    labs(title = "Woreda-level KPI performance (selected indicators)",
         subtitle = "Week 28-52, 2025  |  joined to 2021 woreda boundaries on P-code",
         caption = "Grey = no woreda-level data reported. Performance: red low, green high.") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(size = 8),
          plot.caption = element_text(colour = "grey40", hjust = 0))
  ggsave(file.path(FIG, "05_woreda_choropleths.png"), p5,
         width = 12, height = 8, dpi = 150, bg = "white")
  cat("woreda map figure written\n")
}

cat("Figures ->", FIG, "\nTables  ->", TAB, "\n")
cat("done\n")
