# PHEM Woreda-level KPI analysis

Regional KPI workbooks (Week 28–52, 2025) + the national "PHEM Achievements
Story" narrative. Source files are heterogeneous (different sheet names,
layouts, a few mislabeled — e.g. Afar's sheet says "Gambella").

## Two ways to read the files

### 1. Browse interactively in Positron (no download needed)
```r
source("scripts/view.R")     # load the viewers
kpi_files()                  # list all source files
sheets("Benishangul")        # list sheets in a workbook (fuzzy name)
vx("Benishangul")            # open sheet 1 in the Data Explorer
vx("Benishangul", "Data")    # open a sheet by fuzzy name
vx("Afar", 2)                # open the 2nd sheet by index
vd("DOC")                    # open the .docx (as rendered markdown)
```
`vx()` opens Positron's **Data Explorer** (sort / filter / scroll). Sheets are
read raw (no header guessing) so you see the true layout.

### 2. Converted copies (also openable directly in Positron)
```sh
Rscript scripts/convert.R            # convert ./ -> ./converted
Rscript scripts/convert.R <file>     # convert one file
```
Output in `converted/`:
- `converted/INDEX.md` — start here; lists every file + its sheets
- `<workbook>.md` — all sheets of a workbook as readable tables
- `<workbook>/<sheet>.csv` — one CSV per sheet (opens in the Data Explorer)
- `<docx>.md` — Word doc as markdown (paragraphs + tables)

The converter is reusable on any future `.xlsx` / `.xls` / `.docx`.

## Folder layout

```
raw/2025-Wk03-15/    Q1 workbooks (13 regions, incl. Tigray & Central Ethiopia)
raw/2025-Wk16-27/    Q2 workbooks (11 regions)
raw/2025-Wk28-52/    Q3-Q4 workbooks (11 regions; two quarters of weeks)
All GIS names.xlsx               cleaning key (reported name -> Gname)   — shared, root
reference_geo_names.csv          region/zone/woreda/PCODE reference      — shared, root
Population by GIS woreda best.xlsx  clean woreda name -> population       — shared, root
output/ exports/ template/ converted/ reports/ reports/archive/ scripts/
```

Raw period data lives in `raw/<period>/` (one folder per quarter). The cleaning
key, reference table, population file and shapefile are period-independent and
stay in root. The harmonizer reads **every** period folder in one pass (config in
`catalog.R`) and stacks them into the master via a `period` column.

**Three periods are loaded.** `Wk28-52` spans two quarters (weeks 28–52); the
other two are single quarters. KPIs are proportions, so they stay comparable
across periods; only raw counts scale with period length.

**To add another quarter:** drop its workbooks in `raw/<new-period>/`, add one
config block in `scripts/catalog.R`, append the period to `PERIOD_LEVELS`, and
rerun. See "Reproducing this next quarter" in `reports/pipeline_documentation.qmd`.

## Analysis pipeline

Run everything from the project root:
```sh
Rscript scripts/run_all.R
```
Or run stages individually (each writes to `output/`, `exports/`, or `reports/`):

| Script | Purpose | Output |
|---|---|---|
| `scripts/convert.R` | raw xlsx/docx → readable copies | `converted/` |
| `scripts/catalog.R` | canonical 23-indicator catalog + per-region read config | (sourced) |
| `scripts/harmonize.R` | build tidy master + region/national rollups | `output/master_woreda.csv`, `kpi_region.csv`, `kpi_national.csv`, `harmonize_log.txt` |
| `scripts/crosswalk_build.R` | map reported names → `ADM3_PCODE` via key + reference | `output/crosswalk_woreda.csv`, `crosswalk_unmatched.csv` |
| `scripts/spatialize.R` | attach P-code to master + woreda rollup | `output/master_woreda_geo.csv`, `kpi_woreda_pcode.csv` |
| `scripts/spatialize.R` | attach P-code + backfill population (coalesce reported / reference) | `output/master_woreda_geo.csv`, `kpi_woreda_pcode.csv`, `woreda_population.csv` |
| `scripts/analyze.R` | core figures + tables for slides (focus period) | `exports/figures/01-05*.png`, `exports/tables/*.csv` |
| `scripts/analyze_depth.R` | gap/scorecard/best-worst/coverage/completeness + per-indicator PNGs (focus period) | `exports/figures/06-09*.png`, `exports/figures/indicators/*.png` |
| `scripts/analyze_trends.R` | **cross-period** comparison figures + tables (all periods) | `exports/figures/T01-T07*.png`, `exports/tables/trend_*.csv` |
| `scripts/make_template.R` | blank fill-in data-collection templates (pre-filled standard woreda names + Population) | `template/*.xlsx`, `template/csv/*` |
| `reports/kpi_report.qmd` | single-period report, pinned to the newest period (HTML + Word) | `reports/kpi_report.{html,docx}`, `reports/archive/` |
| `reports/kpi_trends.qmd` | **cross-period trends** report (HTML + Word) | `reports/kpi_trends.{html,docx}` |
| `reports/pipeline_documentation.qmd` | concept + teaching walkthrough + next-quarter guide | `reports/pipeline_documentation.{html,docx}` |
| `reports/kpi_dashboard.qmd` | interactive **Quarto dashboard** (plotly + leaflet + DT) | `reports/kpi_dashboard.html` |
| `shiny/prep_data.R` → `shiny/` | live **Shiny dashboard** (login, reactive filters, fast map) | `shiny/data/*.rds` + the running app |

### Key conventions
- **Periods:** three loaded (`2025-Wk03-15`, `2025-Wk16-27`, `2025-Wk28-52`),
  each row carries its `period`. The single-period report tracks `FOCUS_PERIOD`
  (newest, auto). Cross-period comparison is in `kpi_trends.qmd`.
- **Percentages:** computed as `100 × Σnumerator / Σdenominator` (totals, not a
  mean of woreda percentages). A `value_pct_capped` (0–100) is used for colour;
  raw `value_pct` is kept (timeliness/completeness can exceed 100%).
- **Population:** carried in `population` (currently only Amhara reports it).
- **Reporting vs zero:** blank source cells are `NA` (not reported, shown as
  **NR**/grey), distinct from a genuine `0`. Every output carries `pct_reported`;
  most indicators were reported by ~68% of woredas (timeliness ~99%).
- **Regions:** 11 reported in Wk16-27 and Wk28-52; **13 in Wk03-15** (Tigray and
  Central Ethiopia reported only that first period). South West Ethiopia never
  reported. South Ethiopia (SER) = 2021 "Southern Ethiopi" only.
- **Population:** `population_best` = reported value, else backfilled from
  `Population by GIS woreda best.xlsx` (for attack rate / incidence).
- **Maps:** region + woreda both work. Performance ramp red (poor) → green (good).
- **Targets:** 80% benchmark in `catalog.R` (`KPI_CATALOG$target`), editable.

## Dashboards

Two interactive dashboards sit on top of the same pipeline output:

- **Quarto dashboard** — `reports/kpi_dashboard.qmd`. A static interactive HTML
  page (Overview, Regional, Trends, leaflet Map, Data quality, Data). Render with
  `quarto render reports/kpi_dashboard.qmd`. Free to host on **GitHub Pages**.
- **Shiny app** — `shiny/`. A live app with a login (`shinymanager`), reactive
  filters (period / region / indicator) and a fast woreda choropleth. Build its
  data with `Rscript shiny/prep_data.R`, run with `R -e 'shiny::runApp("shiny")'`,
  host on **shinyapps.io**.

Both reuse the GIS geometry in `assets/eth_adm3_geo.rds` (1,082 woredas, joined to
`output/kpi_woreda_pcode.csv` on `adm3_pcode`) and the local logo
`assets/ephi_logo.png` (the Shiny app uses `shiny/www/ephi_logo.png`).

- **Step-by-step hosting** (GitHub Pages, shinyapps.io, optional Cloudflare login):
  `reports/dashboard_and_hosting.md`.
- **Learn Shiny from scratch** (beginner guide that ends with a walkthrough of this
  app): `shiny/LEARN_SHINY.md`.

### Spatial status
Shapefile: `…/EPHEM-weekly-cleaning - Alert/shapefile/eth_admbnda_adm{1,2,3}_csa_bofedb_2021.shp`.
Joins are on **P-code**. The crosswalk chains reported name → `Gname`
(`All GIS names.xlsx`) → reference woreda (`reference_geo_names.csv`, region-scoped)
→ `ADM3_PCODE`. **99.7% mapped** (Addis summarised to sub-city). Two Gambella
woredas remain in `crosswalk_unmatched.csv`; add confident fixes to
`MANUAL_OVERRIDE` in `crosswalk_build.R`.
