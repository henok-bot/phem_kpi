#!/usr/bin/env Rscript

# ----------------------------------------------------------------------------
# analyze_depth.R  —  deeper analysis: gap vs target, core-function scorecards,
# best/worst woredas, and one slide-ready PNG per indicator.
#
# Reads output/*.csv (run harmonize.R + spatialize.R first). Writes to
# exports/figures/ , exports/figures/indicators/ , exports/tables/.
# Every output carries the reporting rate so weak coverage is visible.
# ----------------------------------------------------------------------------

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(ggplot2); library(forcats); library(scales)
})
here <- function(...) file.path(getwd(), ...)
FIG <- here("exports","figures"); IND <- file.path(FIG,"indicators"); TAB <- here("exports","tables")
dir.create(IND, recursive = TRUE, showWarnings = FALSE)

source(here("scripts", "catalog.R"))   # FOCUS_PERIOD, PERIOD_LABELS
catalog  <- read_csv(here("output","indicator_catalog.csv"), show_col_types = FALSE)
geo_master <- read_csv(here("output","master_woreda_geo.csv"), show_col_types = FALSE)
region   <- read_csv(here("output","kpi_region.csv"),   show_col_types = FALSE)
national <- read_csv(here("output","kpi_national.csv"), show_col_types = FALSE)
wpcode   <- read_csv(here("output","kpi_woreda_pcode.csv"), show_col_types = FALSE)

# single-period report: pin to the focus (newest) period (see analyze.R)
geo_master <- filter(geo_master, period == FOCUS_PERIOD)
region     <- filter(region,     period == FOCUS_PERIOD)
national   <- filter(national,   period == FOCUS_PERIOD)
wpcode     <- filter(wpcode,     period == FOCUS_PERIOD)
PSUB <- unname(PERIOD_LABELS[FOCUS_PERIOD]); if (is.na(PSUB)) PSUB <- FOCUS_PERIOD
prop <- function(df) filter(df, value_type == "proportion")
tgt  <- catalog |> select(indicator = code, target, order)

perf_cols <- c("#d73027","#fc8d59","#fee08b","#d9ef8b","#1a9850")
perf_fill <- function(...) scale_fill_gradientn(colours = perf_cols, limits = c(0,100),
                                                oob = squish, na.value = "grey85", ...)
theme_kpi <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey40", hjust = 0))

# ---- 1. gap vs target ------------------------------------------------------
gap <- prop(region) |> left_join(tgt, by = "indicator") |>
  mutate(gap = round(value_pct - target, 1),
         status = ifelse(value_pct >= target, "meets target", "below target"))
write_csv(gap |> select(region, indicator, label, value_pct, target, gap,
                        status, pct_reported),
          file.path(TAB, "gap_vs_target.csv"))

# how many indicators each region meets
score <- gap |> group_by(region) |>
  summarise(indicators = n(), met = sum(value_pct >= target, na.rm = TRUE),
            pct_met = round(100*met/indicators), .groups = "drop")
write_csv(score, file.path(TAB, "region_targets_met.csv"))

p_score <- ggplot(score, aes(fct_reorder(region, pct_met), pct_met, fill = pct_met)) +
  geom_col() + coord_flip() +
  geom_text(aes(label = paste0(met,"/",indicators)), hjust = -0.15, size = 3) +
  perf_fill(name = "% met") + ylim(0, 100) +
  labs(title = "Indicators meeting the 80% target, by region",
       subtitle = "Week 28-52, 2025  |  proportion indicators",
       x = NULL, y = "% of indicators meeting target",
       caption = "Target = 80% benchmark.") +
  theme_kpi + theme(legend.position = "none")
ggsave(file.path(FIG, "06_targets_met_by_region.png"), p_score,
       width = 9, height = 6, dpi = 150, bg = "white")

# ---- 1b. geographic coverage (% of woredas reporting, per region) ----------
# Denominator = woreda count in the 2021 reference (HIDDEN — it undercounts
# today's woredas, so the % is an upper bound). We publish only the %.
refg <- read_csv(here("reference_geo_names.csv"), show_col_types = FALSE)
refmap <- c("Afar"="Afar","Amhara"="Amhara","Benishangul Gumz"="Benishangul Gumuz",
  "Dire Dawa"="Dire Dawa","Gambela"="Gambella","Harari"="Harari","Addis Ababa"="Addis Ababa",
  "Oromia"="Oromia","Sidama"="Sidama","Somali"="Somali","Southern Ethiopi"="South Ethiopia")
expected <- refg |> mutate(kreg = refmap[region]) |> filter(!is.na(kreg)) |>
  count(kreg, name = "expected")
reported <- geo_master |> filter(!is.na(adm3_pcode)) |>
  distinct(region, adm3_pcode) |> count(region, name = "reported")
cover <- expected |> left_join(reported, by = c("kreg" = "region")) |>
  mutate(reported = coalesce(reported, 0L),
         coverage_pct = pmin(round(100 * reported / expected), 100)) |>
  select(region = kreg, reported, coverage_pct) |>      # expected NOT exported
  arrange(coverage_pct)
write_csv(cover, file.path(TAB, "geographic_coverage.csv"))

# Dire Dawa & Harari reported at facility/aggregate level -> woreda coverage N/A
cover_plot <- cover |> filter(reported > 0)
p_cov <- ggplot(cover_plot, aes(fct_reorder(region, coverage_pct), coverage_pct, fill = coverage_pct)) +
  geom_col() + coord_flip() +
  geom_text(aes(label = paste0(coverage_pct, "%  (", reported, " woredas)")),
            hjust = -0.05, size = 3) +
  scale_fill_gradientn(colours = perf_cols, limits = c(0,100), oob = squish) +
  ylim(0, 115) +
  labs(title = "Geographic coverage: share of woredas reporting, by region",
       subtitle = "Week 28-52, 2025  |  % relative to the 2021 reference woreda list",
       x = NULL, y = "% of woredas reporting (upper bound)",
       caption = paste("The 2021 reference undercounts today's woredas, so true coverage is at most this.",
                       "\nOromia reported only a small fraction of its woredas. Dire Dawa & Harari reported at facility/aggregate level (not shown).")) +
  theme_kpi + theme(legend.position = "none")
ggsave(file.path(FIG, "07_geographic_coverage.png"), p_cov,
       width = 9, height = 6, dpi = 150, bg = "white")

# ---- 1c. reporting report-card per region (two completeness dimensions) -----
# (1) geographic = % of woredas reporting; (2) indicator = avg % of indicators
# a reporting woreda actually filled in. Shown as gap-to-100 so gaps stand out.
ind_compl <- prop(region) |> group_by(region) |>
  summarise(`Indicator completeness` = round(mean(pct_reported, na.rm = TRUE)), .groups = "drop")
card <- cover |> select(region, `Woreda coverage` = coverage_pct) |>
  full_join(ind_compl, by = "region") |>
  filter(!region %in% c("Dire Dawa","Harari")) |>     # facility/aggregate level
  tidyr::pivot_longer(-region, names_to = "metric", values_to = "pct") |>
  mutate(missing = 100 - pct)
ord <- card |> group_by(region) |> summarise(m = mean(pct, na.rm = TRUE)) |> arrange(m)
card <- card |> mutate(region = factor(region, levels = ord$region))

p_card <- ggplot(card, aes(pct, region, fill = metric)) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_dodge(width = .8), hjust = -0.15, size = 2.9) +
  scale_fill_manual(values = c("Woreda coverage" = "#2166ac",
                               "Indicator completeness" = "#80b1d3"), name = NULL) +
  xlim(0, 112) +
  labs(title = "Reporting report-card by region",
       subtitle = "How complete each region's submission was (Week 28-52, 2025) — closer to 100% is better",
       x = "% complete", y = NULL,
       caption = paste("Woreda coverage = share of the region's woredas that reported (vs 2021 reference, an upper bound).",
                       "\nIndicator completeness = average share of indicators a reporting woreda filled in.",
                       "\nDire Dawa & Harari excluded (reported at facility/aggregate level).")) +
  theme_kpi + theme(legend.position = "top")
ggsave(file.path(FIG, "08_reporting_report_card.png"), p_card,
       width = 10, height = 6, dpi = 150, bg = "white")

# ---- 1d. most-incomplete indicators nationally (what to push) --------------
# Use a COMMON denominator (the fullest-reported indicator's submission count)
# so indicators missing from many regions/woredas also read as incomplete.
denom_all <- max(prop(national)$n_woreda, na.rm = TRUE)
miss <- prop(national) |>
  mutate(pct_complete = round(100 * n_reported / denom_all),
         label = fct_reorder(str_wrap(label, 40), pct_complete))
p_miss <- ggplot(miss, aes(pct_complete, label, fill = pct_complete)) +
  geom_col() +
  geom_text(aes(label = paste0(pct_complete, "%")), hjust = -0.15, size = 2.9) +
  scale_fill_gradientn(colours = perf_cols, limits = c(0,100), oob = squish) +
  xlim(0, 112) +
  labs(title = "How completely each indicator was reported (national)",
       subtitle = "Lowest bars are the indicators woredas most often leave blank — focus reporting effort here",
       x = "% of all submissions that included the indicator", y = NULL,
       caption = "Week 28-52, 2025. Common denominator = the fullest-reported indicator, so indicators missing from many regions also read low.") +
  theme_kpi + theme(legend.position = "none")
ggsave(file.path(FIG, "09_most_incomplete_indicators.png"), p_miss,
       width = 10, height = 8, dpi = 150, bg = "white")

# ---- 2. core-function scorecard (region x group + composite) ---------------
cf <- prop(region) |> group_by(region, group) |>
  summarise(value = round(mean(value_pct_capped, na.rm = TRUE), 1),
            pct_reported = round(mean(pct_reported, na.rm = TRUE)), .groups = "drop")
composite <- prop(region) |> group_by(region) |>
  summarise(composite_score = round(mean(value_pct_capped, na.rm = TRUE), 1),
            mean_reporting = round(mean(pct_reported, na.rm = TRUE)), .groups = "drop") |>
  arrange(desc(composite_score))
cf_wide <- cf |> select(region, group, value) |>
  pivot_wider(names_from = group, values_from = value) |>
  left_join(composite, by = "region") |> arrange(desc(composite_score))
write_csv(cf_wide, file.path(TAB, "corefunction_scorecard.csv"))

# ---- 3. best / worst woredas per key indicator -----------------------------
keyw <- c("RPT_TIMELY","RPT_COMPLETE","RESP_2448","LAB_7D","NOTIFY_24H",
          "EPRP_PLAN","CB_EVENTS","DHIS2_RPT")
bw <- wpcode |> filter(indicator %in% keyw, value_type == "proportion",
                       denom_sum >= 3, !is.na(value_pct)) |>
  group_by(indicator, label) |>
  mutate(rk_hi = rank(-value_pct, ties.method = "first"),
         rk_lo = rank(value_pct,  ties.method = "first")) |>
  filter(rk_hi <= 5 | rk_lo <= 5) |>
  mutate(end = ifelse(rk_hi <= 5, "top 5", "bottom 5")) |>
  arrange(indicator, desc(value_pct)) |>
  select(indicator, label, end, woreda = adm3_en, region = ref_region,
         value_pct = value_pct, num_sum, denom_sum) |> ungroup()
write_csv(bw, file.path(TAB, "best_worst_woredas.csv"))

# ---- 4. one slide-ready PNG per indicator ----------------------------------
natv <- prop(national) |> select(indicator, nat = value_pct, nat_rep = pct_reported)
for (i in seq_len(nrow(catalog))) {
  cc <- catalog[i, ]
  if (cc$value_type != "proportion") next
  d <- region |> filter(indicator == cc$code)
  if (!nrow(d)) next
  nat <- natv |> filter(indicator == cc$code)
  d <- d |> mutate(region = fct_reorder(region, coalesce(value_pct_capped, -1)))
  pp <- ggplot(d, aes(region, value_pct_capped, fill = value_pct_capped)) +
    geom_col() + coord_flip() +
    geom_hline(yintercept = cc$target, linetype = "dashed", colour = "grey25") +
    geom_hline(data = nat, aes(yintercept = nat), colour = "#2166ac", linewidth = .7) +
    geom_text(aes(label = paste0(round(value_pct_capped), "%  (",
                                 round(pct_reported), "% rep)")),
              hjust = -0.05, size = 2.9) +
    perf_fill() + ylim(0, 115) +
    labs(title = str_wrap(cc$label, 60),
         subtitle = paste0("National: ", round(nat$nat), "%  (blue)   |   target ",
                           cc$target, "% (dashed)   |   Week 28-52, 2025"),
         x = NULL, y = "%",
         caption = paste0("Bar label = value and % of woredas reporting (rep).  Core function: ",
                          cc$group, ".")) +
    theme_kpi + theme(legend.position = "none")
  ggsave(file.path(IND, sprintf("%02d_%s.png", cc$order, cc$code)), pp,
         width = 8, height = 5, dpi = 140, bg = "white")
}

cat("depth tables ->", TAB, "\nper-indicator PNGs ->", IND, "\n")
cat("composite ranking:\n"); print(as.data.frame(composite))
cat("done\n")
