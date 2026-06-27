# server.R
# The "engine": turns inputs (sidebar filters) into outputs (plots, maps, tables).
# Pattern used throughout:
#   reactive(...)  = a value that recomputes when its inputs change.
#   render*(...)   = produces an output, re-running when the reactives it uses change.

server <- function(input, output, session) {

  # ── login ──────────────────────────────────────────────────────────────────
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(APP_CREDENTIALS))

  # ── helpers ──────────────────────────────────────────────────────────────
  ind_label <- function(code) {
    if (identical(code, "__COMPOSITE__")) return("Composite (mean of proportions)")
    nm <- names(INDICATORS)[match(code, INDICATORS)]
    if (is.na(nm)) code else nm
  }

  # ── reactive data slices (the single source the outputs read from) ─────────
  # req() pauses these until the sidebar inputs exist. Before the user logs in
  # (shinymanager), the inputs are NULL; without req() a filter(period == NULL)
  # would error on the first flush.
  reg_slice <- reactive({
    req(input$sel_period, input$sel_regions)
    region |> dplyr::filter(period == input$sel_period,
                            region %in% input$sel_regions)
  })
  nat_slice <- reactive({
    req(input$sel_period)
    national |> dplyr::filter(period == input$sel_period)
  })
  wide_slice <- reactive({
    req(input$sel_period, input$sel_regions)
    wide |> dplyr::filter(period == input$sel_period,
                          region %in% input$sel_regions)
  })

  # ── Overview: value boxes (reporting-first) ────────────────────────────────
  output$value_boxes <- renderUI({
    req(input$sel_period)
    pf_val <- trend_dq |> dplyr::filter(period == input$sel_period) |>
      dplyr::pull(pct_filled)
    s <- kpi_value_boxes(reg_slice(), wide_slice(), pf_val)
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      bslib::value_box("Regions reporting",
                       paste0(s$n_reg, " of ", N_REGIONS_TOTAL),
                       showcase = shiny::icon("hospital"), theme = "primary"),
      bslib::value_box("Woredas reporting", format(s$n_wor, big.mark = ","),
                       showcase = shiny::icon("location-dot"), theme = "info"),
      bslib::value_box("Woreda coverage", paste0(s$coverage, "%"),
                       showcase = shiny::icon("map-pin"), theme = cov_col(s$coverage)),
      bslib::value_box("KPI cells filled", paste0(s$entries_filled, "%"),
                       showcase = shiny::icon("list-check"),
                       theme = cov_col(s$entries_filled)))
  })

  # per-region reporting + performance, read side by side (cautious interpretation).
  # Dire Dawa reports per facility and Harari as one aggregate row — noted under
  # the table rather than carried as a column.
  reg_summary <- reactive({
    perf <- reg_slice() |> dplyr::filter(value_type == "proportion") |>
      dplyr::group_by(region) |>
      dplyr::summarise(`indicators (of 21)` = sum(!is.na(value_pct_capped)),
                       `mean performance %` = round(mean(value_pct_capped, na.rm = TRUE)),
                       .groups = "drop")
    units <- wide_slice() |> dplyr::group_by(region) |>
      dplyr::summarise(`units reported` = dplyr::n_distinct(woreda), .groups = "drop")
    units |>
      dplyr::left_join(perf, by = "region") |>
      dplyr::select(region, `units reported`, `indicators (of 21)`, `mean performance %`) |>
      dplyr::arrange(dplyr::desc(`units reported`))
  })
  output$t_reg_summary <- DT::renderDT(
    DT::datatable(reg_summary(), rownames = FALSE,
                  options = list(dom = "t", pageLength = 20, scrollX = TRUE)))

  output$p_nat_bar      <- plotly::renderPlotly(plot_national_bar(nat_slice()))
  output$p_corefunction <- plotly::renderPlotly(plot_corefunction_bar(nat_slice()))

  # ── Trends ──────────────────────────────────────────────────────────────────
  output$p_trend_nat <- plotly::renderPlotly(
    plot_trend_national(trend_nat, labels = input$trend_labels))
  output$p_trend_comp <- plotly::renderPlotly(plot_trend_composite(trend_comp))
  output$t_dq <- DT::renderDT({
    trend_dq |>
      dplyr::mutate(period = unname(PERIOD_LABELS[period])) |>
      dplyr::rename(`regions` = regions_reporting, `woreda units` = woreda_units,
                    `mapped` = mapped_woredas, `records` = records,
                    `% filled` = pct_filled,
                    `num>denom` = impossible_num_gt_denom,
                    `raw>100%` = over_100_raw) |>
      DT::datatable(rownames = FALSE, options = list(dom = "t"))
  })

  # ── Regional heatmap ────────────────────────────────────────────────────────
  output$p_heatmap <- plotly::renderPlotly(
    plot_region_heatmap(reg_slice(), nat_slice()))

  # ── Map (two-step: base once, recolour via proxy) ──────────────────────────
  output$map <- leaflet::renderLeaflet(build_base_map(geo1))
  output$map_title <- renderText({
    req(input$sel_indicator, input$sel_period)
    paste0("Woreda map — ", ind_label(input$sel_indicator), " — ",
           unname(PERIOD_LABELS[input$sel_period]))
  })
  observe({
    req(input$sel_period, input$sel_indicator)
    proxy <- leaflet::leafletProxy("map")
    update_kpi_choropleth(proxy, geo3, wpcode, input$sel_period,
                          input$sel_indicator, title = ind_label(input$sel_indicator))
  })

  # ── Explorer ────────────────────────────────────────────────────────────────
  expl_code <- reactive({
    req(input$sel_indicator)
    if (identical(input$sel_indicator, "__COMPOSITE__")) "RPT_TIMELY"
    else input$sel_indicator
  })
  output$expl_title <- renderText(
    paste0(ind_label(expl_code()), " — by region"))
  output$p_indicator <- plotly::renderPlotly(
    plot_indicator_by_region(reg_slice(), expl_code()))
  output$t_bestworst <- DT::renderDT({
    req(input$sel_period)
    d <- wpcode |>
      dplyr::filter(period == input$sel_period, indicator == expl_code(),
                    denom_sum >= 3) |>
      dplyr::transmute(woreda = adm3_en, region = ref_region,
                       `%` = round(value_pct_capped), denom = denom_sum)
    if (!nrow(d)) return(DT::datatable(d, rownames = FALSE))
    d <- d[order(-d$`%`), ]
    show <- rbind(utils::head(d, 5), utils::tail(d, 5))
    DT::datatable(show, rownames = FALSE,
                  options = list(dom = "t", pageLength = 10))
  })

  # ── Data table ──────────────────────────────────────────────────────────────
  output$t_data <- DT::renderDT({
    wide_slice() |>
      dplyr::select(region, zone, woreda, population_best,
                    RPT_TIMELY, RPT_COMPLETE, NOTIFY_24H, RESP_2448, LAB_7D,
                    EPRP_PLAN, DHIS2_RPT, ATTACK_RATE, CFR) |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 1))) |>
      DT::datatable(rownames = FALSE, filter = "top", extensions = "Buttons",
                    options = list(pageLength = 25, scrollX = TRUE,
                                   dom = "Bfrtip", buttons = c("csv", "excel")))
  })
}
