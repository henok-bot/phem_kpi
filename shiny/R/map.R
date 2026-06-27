# R/map.R
# Two-step leaflet map (the pattern that makes metric switching feel instant):
#   1. build_base_map()        — tiles + region borders, rendered ONCE.
#   2. update_kpi_choropleth()  — recolours woreda polygons + legend via a proxy,
#                                 so we never redraw the whole map.

CHORO_GROUP <- "kpi_layer"

build_base_map <- function(geo1) {
  leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE,
                                                     minZoom = 5)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolylines(data = geo1, color = "#333", weight = 1.4,
                          opacity = 0.8, group = "adm1") |>
    leaflet::setView(lng = 39.5, lat = 9.0, zoom = 6)
}

# value per woreda for the chosen period + indicator (or the composite when
# indicator_code is "__COMPOSITE__"), joined to geometry on adm3_pcode.
.kpi_values <- function(wpcode, period, indicator_code) {
  if (identical(indicator_code, "__COMPOSITE__")) {
    wpcode |>
      dplyr::filter(period == !!period, value_type == "proportion") |>
      dplyr::group_by(adm3_pcode) |>
      dplyr::summarise(v = mean(value_pct_capped, na.rm = TRUE), .groups = "drop")
  } else {
    wpcode |>
      dplyr::filter(period == !!period, indicator == indicator_code) |>
      dplyr::group_by(adm3_pcode) |>
      dplyr::summarise(v = mean(value_pct_capped, na.rm = TRUE), .groups = "drop")
  }
}

update_kpi_choropleth <- function(proxy, geo3, wpcode, period, indicator_code,
                                  title = "") {
  vals <- .kpi_values(wpcode, period, indicator_code)
  g    <- dplyr::left_join(geo3, vals, by = "adm3_pcode")
  pal  <- perf_pal()
  lab  <- sprintf("<b>%s</b><br/>%s: %s", g$adm3_en, title,
                  ifelse(is.na(g$v), "no data", paste0(round(g$v), "%"))) |>
    lapply(htmltools::HTML)
  proxy |>
    leaflet::clearGroup(CHORO_GROUP) |>
    leaflet::clearControls() |>
    leaflet::addPolygons(data = g, fillColor = ~pal(v), fillOpacity = 0.85,
                         weight = 0.4, color = "white", group = CHORO_GROUP,
                         label = lab,
                         highlightOptions = leaflet::highlightOptions(
                           weight = 2, color = "#333", bringToFront = TRUE)) |>
    leaflet::addLegend(data = g, pal = pal, values = c(0, 100),
                       title = paste0(title, " (%)"), position = "bottomright")
}
