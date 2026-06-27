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
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header("National KPI performance — selected period"),
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("p_nat_bar", height = "560px"),
                    color = EPHI_GREEN)),
      bslib::card(bslib::card_header("% of indicators at target, by region"),
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("p_reg_targets", height = "560px"),
                    color = EPHI_GREEN))
    )
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
        plotly::plotlyOutput("p_trend_nat", height = "440px"), color = EPHI_GREEN)),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Regional composite trend"),
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("p_trend_comp", height = "360px"),
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
        plotly::plotlyOutput("p_heatmap", height = "620px"), color = EPHI_GREEN))
  ),

  # ── Tab 4: Map ─────────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Map",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(textOutput("map_title", inline = TRUE)),
      shinycssloaders::withSpinner(
        leaflet::leafletOutput("map", height = "640px"), color = EPHI_GREEN))
  ),

  # ── Tab 5: Explorer ────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Explorer",
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header(textOutput("expl_title", inline = TRUE)),
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput("p_indicator", height = "480px"),
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
