#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# analyze_trends.R  —  cross-period (multi-quarter) comparison
#
# Compares the reporting periods now in the master (2025 Wk03-15, Wk16-27,
# Wk28-52). Focus, per the brief: (1) data-quality / reporting issues over
# time, (2) the major KPI outputs of interest, side by side.
#
# IMPORTANT period-length caveat: Wk28-52 spans TWO quarters (weeks 28-52),
# while Wk03-15 and Wk16-27 are single quarters. The KPIs are PROPORTIONS
# (ratios), so they stay comparable across periods regardless of length; only
# raw counts scale with period length. This is stated on the count-based panels.
#
# Periods are discovered from the data, ordered by PERIOD_LEVELS (catalog.R),
# so adding a future quarter needs no change here.
#
# Reads output/*.csv (run harmonize + spatialize first). Writes:
#   exports/figures/T0*_*.png   trend figures
#   exports/tables/trend_*.csv  the underlying numbers
# ----------------------------------------------------------------------------

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(ggplot2); library(forcats); library(scales)
})
here <- function(...) file.path(getwd(), ...)
source(here("scripts", "catalog.R"))                 # PERIOD_LEVELS / LABELS, KPI_CATALOG
FIG <- here("exports", "figures"); TAB <- here("exports", "tables")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB, recursive = TRUE, showWarnings = FALSE)

catalog  <- read_csv(here("output", "indicator_catalog.csv"),  show_col_types = FALSE)
master   <- read_csv(here("output", "master_woreda.csv"),      show_col_types = FALSE)
geo      <- read_csv(here("output", "master_woreda_geo.csv"),  show_col_types = FALSE)
region   <- read_csv(here("output", "kpi_region.csv"),         show_col_types = FALSE)
national <- read_csv(here("output", "kpi_national.csv"),       show_col_types = FALSE)

# ---- guardrail: no proportion may exceed 100% ------------------------------
# The rollups are built over complete pairs with numerator <= denominator, so a
# proportion can never exceed 100. Assert it here so any future data-entry error
# that slips through (e.g. a leaked total row) stops the run loudly instead of
# drawing a >100% bar. Display values are also capped defensively below.
chk <- bind_rows(region, national) |> filter(value_type == "proportion")
bad <- chk |> filter(!is.na(value_pct) & value_pct > 100.0001)
if (nrow(bad) > 0) {
  print(head(as.data.frame(bad), 10))
  stop(sprintf("Guardrail tripped: %d proportion rollup(s) exceed 100%%. ",
               nrow(bad)),
       "Check harmonize.R valid_pair (complete pair + numerator <= denominator).")
}

# ---- period factor (oldest -> newest), discovered from the data -------------
periods_present <- unique(master$period)
ord <- c(intersect(PERIOD_LEVELS, periods_present),
         sort(setdiff(periods_present, PERIOD_LEVELS)))
plab <- ifelse(ord %in% names(PERIOD_LABELS), PERIOD_LABELS[ord], ord)
pf   <- function(x) factor(x, levels = ord, labels = plab)
NPER <- length(ord)
newest <- tail(ord, 1)

prop  <- function(df) filter(df, value_type == "proportion")
grp_levels <- c("Identifying","Reporting","Analysis","Laboratory","Response","Preparedness")
perf_cols  <- c("#d73027","#fc8d59","#fee08b","#d9ef8b","#1a9850")
theme_kpi <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey40", hjust = 0))
# newest period drawn bold + red, earlier periods muted (per house style)
per_cols <- setNames(c(rep("grey60", NPER - 1), "#c00000"), plab)
per_size <- setNames(c(rep(0.7, NPER - 1), 1.5), plab)

LENGTH_NOTE <- paste0("Note: Wk28-52 spans two quarters; Wk03-15 and Wk16-27 are single quarters.\n",
                      "Proportions stay comparable across periods; raw counts do not.")

# ============================================================================
# A. DATA-QUALITY / REPORTING over time
# ============================================================================

# ---- participation: regions x periods (n mapped woredas) --------------------
# distinct mapped P-codes per region/period handles Addis (sub-city) and
# facility reporting cleanly; falls back to raw woreda where unmapped.
part <- master |>
  group_by(period, region) |>
  summarise(n_unit = n_distinct(woreda),
            reported_any = sum(!is.na(numerator) | !is.na(denominator) |
                               !is.na(value_reported)) > 0,
            .groups = "drop")
cover_geo <- geo |> filter(!is.na(adm3_pcode)) |>
  distinct(period, region, adm3_pcode) |>
  count(period, region, name = "n_woreda_mapped")
part <- part |> left_join(cover_geo, by = c("period", "region")) |>
  mutate(n_woreda_mapped = coalesce(n_woreda_mapped, 0L),
         period_f = pf(period))
write_csv(part |> select(period, region, n_unit, n_woreda_mapped, reported_any),
          file.path(TAB, "trend_participation_by_period.csv"))

# T01: participation matrix (region x period), tile = mapped woreda count
p_part <- ggplot(part, aes(period_f, fct_rev(region), fill = n_woreda_mapped)) +
  geom_tile(colour = "white", linewidth = .5) +
  geom_text(aes(label = ifelse(reported_any, n_woreda_mapped, "—")), size = 3) +
  scale_fill_gradientn(colours = c("#f7f7f7", perf_cols[3], perf_cols[5]),
                       trans = "sqrt", na.value = "grey90", name = "woredas\nreporting") +
  labs(title = "Who reported, and how widely — by period",
       subtitle = "Mapped woredas reporting per region (— = region did not report that period)",
       x = NULL, y = NULL,
       caption = paste("Tigray & Central Ethiopia reported only in Wk03-15. Oromia and Harari coverage collapsed by Wk28-52.",
                       "\n", LENGTH_NOTE)) +
  theme_kpi
ggsave(file.path(FIG, "T01_participation_matrix.png"), p_part,
       width = 10.5, height = 7, dpi = 150, bg = "white")

# T02: woreda coverage trend per region (lines, newest period marked)
cov_trend <- part |> filter(reported_any)
p_cov <- ggplot(cov_trend, aes(period_f, n_woreda_mapped, group = region)) +
  geom_line(colour = "grey70") +
  geom_point(aes(colour = period_f), size = 2) +
  scale_colour_manual(values = per_cols, guide = "none") +
  ggrepel::geom_text_repel(data = cov_trend |> group_by(region) |>
                             filter(period == newest),
                           aes(label = region), size = 3, hjust = 0,
                           direction = "y", nudge_x = .1, segment.colour = "grey80") +
  scale_y_continuous(trans = "log1p", breaks = c(0,5,10,25,50,100,200,400)) +
  labs(title = "Woreda reporting coverage over time, by region",
       subtitle = "Mapped woredas reporting each period (log scale)",
       x = NULL, y = "woredas reporting",
       caption = paste("Oromia: 435 -> 423 -> 8 woredas. Harari: 9 -> 9 -> 1.", LENGTH_NOTE)) +
  theme_kpi + theme(plot.margin = margin(5, 60, 5, 5))
ggsave(file.path(FIG, "T02_coverage_trend.png"), p_cov,
       width = 10, height = 6.5, dpi = 150, bg = "white")

# ---- period-level data-quality scorecard -----------------------------------
dq <- master |> group_by(period) |>
  summarise(
    regions_reporting = n_distinct(region),
    woreda_units      = n_distinct(woreda),
    records           = n(),
    pct_filled        = round(100 * mean(!is.na(numerator) | !is.na(denominator) |
                                         !is.na(value_reported)), 1),
    impossible_num_gt_denom = sum(value_type == "proportion" & !is.na(numerator) &
                                  !is.na(denominator) & denominator > 0 &
                                  numerator > denominator),
    over_100_raw      = sum(value_type == "proportion" & !is.na(value_pct) &
                            value_pct > 100, na.rm = TRUE),
    .groups = "drop") |>
  arrange(factor(period, levels = ord))

# mapped woredas (geo) per period for the coverage row
dq_geo <- geo |> filter(!is.na(adm3_pcode)) |> distinct(period, adm3_pcode) |>
  count(period, name = "mapped_woredas")
dq <- dq |> left_join(dq_geo, by = "period") |>
  relocate(mapped_woredas, .after = woreda_units)
write_csv(dq, file.path(TAB, "trend_data_quality.csv"))

# T03: data-quality scorecard as a small figure (long form)
# 4 distinct metrics (impossible-rows == raw values >100% in this data, the same
# data-entry error class — both kept in trend_data_quality.csv, one shown here).
dq_long <- dq |>
  transmute(period_f = pf(period),
            `Regions reporting` = regions_reporting,
            `Woredas reporting (mapped)` = mapped_woredas,
            `% of indicator entries filled` = pct_filled,
            `Data-entry errors (num>denom)` = impossible_num_gt_denom) |>
  pivot_longer(-period_f, names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, levels = c(
    "Regions reporting","Woredas reporting (mapped)","% of indicator entries filled",
    "Data-entry errors (num>denom)")))
p_dq <- ggplot(dq_long, aes(period_f, value, fill = period_f)) +
  geom_col(width = .65) +
  geom_text(aes(label = value), vjust = -0.3, size = 3) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = per_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, .18))) +
  labs(title = "Data-quality scorecard by period",
       subtitle = "Reporting breadth fell across the year while completeness of returns rose",
       x = NULL, y = NULL,
       caption = paste("'% of indicator entries filled' = share of all woreda x indicator slots that contained a value (a blank slot is 'not reported').",
                       "\n'Data-entry errors' count rows where the numerator exceeds the denominator; these are excluded from every percentage.",
                       "\n", LENGTH_NOTE)) +
  theme_kpi + theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(FIG, "T03_data_quality_scorecard.png"), p_dq,
       width = 11, height = 6.5, dpi = 150, bg = "white")

# ---- national reporting completeness per indicator x period -----------------
rep_trend <- prop(national) |>
  left_join(select(catalog, indicator = code, order), by = "indicator") |>
  mutate(label = fct_reorder(str_wrap(label, 40), -order), period_f = pf(period))
p_rep <- ggplot(rep_trend, aes(period_f, label, fill = pct_reported)) +
  geom_tile(colour = "white", linewidth = .4) +
  geom_text(aes(label = round(pct_reported)), size = 2.6) +
  scale_fill_gradientn(colours = c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#1b7837"),
                       limits = c(0,100), oob = squish, name = "% reported") +
  labs(title = "Indicator reporting completeness over time (national)",
       subtitle = "Share of reporting woredas that filled each indicator",
       x = NULL, y = NULL,
       caption = "Use to show regions which indicators are chronically left blank.") +
  theme_kpi
ggsave(file.path(FIG, "T04_reporting_completeness_trend.png"), p_rep,
       width = 8.5, height = 9, dpi = 150, bg = "white")

# ============================================================================
# B. MAJOR KPI OUTPUTS over time
# ============================================================================

# ---- national value % per indicator x period --------------------------------
natw <- prop(national) |>
  left_join(select(catalog, indicator = code, order, target), by = "indicator") |>
  select(period, indicator, label, order, target, value_pct, pct_reported) |>
  arrange(order, factor(period, levels = ord))
write_csv(natw |> select(-order),
          file.path(TAB, "trend_national_by_period.csv"))
write_csv(prop(region) |>
            left_join(select(catalog, indicator = code, order), by = "indicator") |>
            select(period, region, indicator, label, value_pct, pct_reported) |>
            arrange(region, factor(period, levels = ord)),
          file.path(TAB, "trend_region_by_period.csv"))

# T05: national KPI trajectory, small multiples (one panel per indicator)
nd <- natw |> mutate(label = fct_reorder(str_wrap(label, 34), order),
                     period_f = pf(period))
p_nat <- ggplot(nd, aes(period_f, value_pct, group = 1)) +
  geom_hline(aes(yintercept = target), linetype = "dashed", colour = "grey55") +
  geom_line(colour = "grey55", linewidth = .7) +
  geom_point(aes(colour = period_f, size = period_f)) +
  geom_text(aes(label = round(value_pct)), vjust = -0.7, size = 2.4, colour = "grey25") +
  facet_wrap(~label, ncol = 4) +
  scale_colour_manual(values = per_cols, guide = "none") +
  scale_size_manual(values = per_size, guide = "none") +
  scale_y_continuous(limits = c(0, 108), breaks = c(0,50,80,100)) +
  labs(title = "National KPI performance across the three periods",
       subtitle = "Each panel = one indicator, % (100 x summed numerator / denominator). Dashed = 80% target; newest period in red.",
       x = NULL, y = "%",
       caption = paste("Proportions are length-invariant, so the three periods are directly comparable.", LENGTH_NOTE)) +
  theme_kpi + theme(strip.text = element_text(size = 7.5),
                    axis.text.x = element_text(size = 7, angle = 20, hjust = 1))
ggsave(file.path(FIG, "T05_national_kpi_trajectory.png"), p_nat,
       width = 13, height = 14, dpi = 140, bg = "white", limitsize = FALSE)

# ---- core-function composite (national) per period --------------------------
cf <- prop(region) |>
  group_by(period, group) |>
  summarise(value = mean(value_pct_capped, na.rm = TRUE), .groups = "drop") |>
  mutate(group = factor(group, levels = grp_levels), period_f = pf(period))
write_csv(cf |> select(period, group, value), file.path(TAB, "trend_corefunction.csv"))
p_cf <- ggplot(cf, aes(group, value, fill = period_f)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  geom_text(aes(label = round(value)), position = position_dodge(width = .8),
            vjust = -0.3, size = 2.7) +
  scale_fill_manual(values = per_cols, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, .12))) +
  labs(title = "PHEM core-function performance by period",
       subtitle = "Mean of region indicator % within each core function",
       x = NULL, y = "mean %",
       caption = paste("Newest period in red.", LENGTH_NOTE)) +
  theme_kpi + theme(legend.position = "top",
                    axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(file.path(FIG, "T06_corefunction_by_period.png"), p_cf,
       width = 10, height = 6, dpi = 150, bg = "white")

# T07: composite score by region x period (common regions only, slope-friendly)
comp <- prop(region) |> group_by(period, region) |>
  summarise(composite = round(mean(value_pct_capped, na.rm = TRUE), 1),
            mean_rep = round(mean(pct_reported, na.rm = TRUE)), .groups = "drop")
write_csv(comp, file.path(TAB, "trend_region_composite.csv"))
comp_w <- comp |> select(period, region, composite) |>
  mutate(period = factor(period, levels = ord)) |>
  pivot_wider(names_from = period, values_from = composite)
write_csv(comp_w, file.path(TAB, "trend_region_composite_wide.csv"))

p_comp <- ggplot(comp, aes(pf(period), composite, group = region)) +
  geom_line(colour = "grey75") +
  geom_point(aes(colour = pf(period)), size = 2) +
  ggrepel::geom_text_repel(data = comp |> group_by(region) |> filter(period == newest),
                           aes(label = region), size = 3, hjust = 0, nudge_x = .1,
                           direction = "y", segment.colour = "grey85") +
  scale_colour_manual(values = per_cols, guide = "none") +
  ylim(0, 100) +
  labs(title = "Composite KPI score by region, over time",
       subtitle = "Mean of a region's proportion indicators (capped 0-100)",
       x = NULL, y = "composite score",
       caption = paste("Read alongside coverage — a high score on very few woredas is not representative.", LENGTH_NOTE)) +
  theme_kpi + theme(plot.margin = margin(5, 70, 5, 5))
ggsave(file.path(FIG, "T07_region_composite_trend.png"), p_comp,
       width = 10, height = 7, dpi = 150, bg = "white")

cat("trend figures -> ", FIG, " (T01-T07)\n")
cat("trend tables  -> ", TAB, " (trend_*.csv)\n")
cat("\nperiods compared:", paste(ord, collapse = ", "), "\n")
print(as.data.frame(dq))
cat("done\n")
