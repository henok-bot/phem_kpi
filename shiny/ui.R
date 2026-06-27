# ui.R
# Describes WHAT the page looks like (the layout + the input/output slots).
# It contains no analysis — every output here is filled in by server.R.
# The whole UI is wrapped in shinymanager::secure_app() to add the login gate.

# choices for the indicator dropdown: a "composite" option, then every indicator
INDICATOR_CHOICES <- c(`Composite (mean of proportions)` = "__COMPOSITE__",
                       INDICATORS)

ui <- bslib::page_navbar(
  id      = "tabs",
  title   = tags$span(
    tags$img(src = "ephi_logo.png", height = "30px",
             style = "margin-right:8px; vertical-align:middle;"),
    "PHEM KPI Dashboard"),
  window_title = "PHEM KPI Dashboard — EPHI",
  theme   = bslib::bs_theme(bootswatch = "flatly", primary = EPHI_GREEN),
  navbar_options = bslib::navbar_options(bg = EPHI_GREEN, theme = "dark"),
  # make the chart-heavy tabs fill the window (less scrolling, bigger figures);
  # "Data" is left scrollable for its long table.
  fillable = c("Overview", "Trends", "Regional", "Map", "Explorer"),

  header  = tags$head(tags$link(rel = "stylesheet", href = "custom.css")),

  # ── shared left sidebar: filters that drive most tabs ──────────────────────
  sidebar = bslib::sidebar(
    width = 280,
    selectInput("sel_period", "Reporting period",
                choices = PERIOD_CHOICES, selected = FOCUS_PERIOD),
    shinyWidgets::pickerInput(
      "sel_regions", "Regions",
      choices = ALL_REGIONS, selected = ALL_REGIONS, multiple = TRUE,
      options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                            selectedTextFormat = "count > 2",
                                            liveSearch = TRUE)),
    selectInput("sel_indicator", "Indicator (Map & Explorer)",
                choices = INDICATOR_CHOICES, selected = "__COMPOSITE__"),
    tags$hr(),
    tags$small(class = "text-muted",
               "Proportions are capped at 100%. Wk28–52 spans two quarters; ",
               "proportions stay comparable across periods, counts do not.")
  ),

  # ── Tab 1: Overview ────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Overview",
    uiOutput("value_boxes"),
    tags$details(
      class = "mb-2",
      tags$summary("How to read"),
      tags$small(
        "Every KPI % is computed among woredas that reported ",
        "(Σ numerator ÷ Σ denominator), so it reflects performance ",
        "among reporters, not all woredas. Read performance alongside the ",
        "reporting columns — a high % built on a few woredas (e.g. Oromia ",
        "this period) is not comparable to one built on most of a region. ",
        "Woreda coverage is measured against the 2021 reference list (an upper bound).")),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Regional reporting & performance — read together"),
        DT::DTOutput("t_reg_summary"),
        bslib::card_footer(tags$details(
          tags$summary("Column definitions"),
          tags$ul(
            tags$li(tags$b("units reported"), " — distinct reporting units for the region (woredas in most regions)."),
            tags$li(tags$b("indicators (of 21)"), " — how many of the 21 proportion KPIs produced a usable value. (The framework has 23 KPIs: 21 proportions plus 2 rates — attack rate and CFR — which are not 0–100% and are shown in the Data tab/Map, so they are not counted here.) The rest were blank or had no events to measure (a zero denominator), so they are N/A."),
            tags$li(tags$b("mean performance %"), " — average of the region's usable KPI percentages (each = Σ num ÷ Σ denom among reporting woredas, capped at 100). Read it with the columns to its left."),
            tags$li(tags$b("NB:"), " Dire Dawa reports per health facility (PHCU/hospital) and Harari as a single regional row, so their units are not woredas."))))),
      bslib::card(
        bslib::card_header("National KPI performance — among woredas that reported"),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput("p_nat_bar", height = "100%"), color = EPHI_GREEN))
    ),
    bslib::card(
      height = "300px",
      bslib::card_header("Performance by PHEM core function — national, selected period (mean of indicators)"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("p_corefunction", height = "100%"), color = EPHI_GREEN))
  ),

  # ── Tab 2: Trends ──────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Trends",
    bslib::card(
      bslib::card_header("National KPI trajectory across periods"),
      selectInput("trend_labels", "Show indicators (default: all)",
                  choices = names(INDICATORS), multiple = TRUE,
                  width = "100%"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("p_trend_nat", height = "100%"), color = EPHI_GREEN)),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Regional composite trend"),
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("p_trend_comp", height = "100%"),
                    color = EPHI_GREEN)),
      bslib::card(bslib::card_header("Data-quality metrics by period"),
                  DT::DTOutput("t_dq"))
    )
  ),

  # ── Tab 3: Regional ────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Regional",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Region × indicator performance (%, selected period)"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("p_heatmap", height = "100%"), color = EPHI_GREEN))
  ),

  # ── Tab 4: Map ─────────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Map",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(textOutput("map_title", inline = TRUE)),
      shinycssloaders::withSpinner(
        leaflet::leafletOutput("map", height = "100%"), color = EPHI_GREEN))
  ),

  # ── Tab 5: Explorer ────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Explorer",
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(textOutput("expl_title", inline = TRUE)),
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("p_indicator", height = "100%"),
                    color = EPHI_GREEN)),
      bslib::card(bslib::card_header("Best & worst woredas (this indicator)"),
                  DT::DTOutput("t_bestworst"))
    )
  ),

  # ── Tab 6: Data ────────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Data",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Woreda × indicator values (selected period) — search & export"),
      DT::DTOutput("t_data"))
  ),

  bslib::nav_spacer(),
  bslib::nav_item(tags$a("EPHI", href = "https://www.ephi.gov.et", target = "_blank"))
)

# add the username / password gate
ui <- shinymanager::secure_app(ui, enable_admin = FALSE,
                               tags_top = tags$div(
                                 tags$img(src = "ephi_logo.png", width = 90),
                                 tags$h4("PHEM KPI Dashboard")))
