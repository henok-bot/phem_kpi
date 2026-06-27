# Learning Shiny — from zero to this dashboard

This guide assumes you know some R (dplyr, ggplot) but have never built a Shiny
app. It builds up in small steps, then walks through *our* KPI app file by file,
and ends with how to run and deploy it. Work through it with an R console open —
type the examples, don't just read them.

---

## 1. The one idea behind Shiny

A Shiny app is a normal R session that a web page can talk to. You write two things:

- a **UI** — what the page looks like (boxes, dropdowns, plots), and
- a **server** — R code that fills those boxes in, and *re-runs automatically*
  whenever the user changes an input.

That automatic re-running is called **reactivity**. You never write "when the user
clicks, do X". You just say "this plot is made from that dropdown", and Shiny
figures out when to redraw it.

Two halves talk through two lists:

- `input$something` — the current value of an input (read it in the server).
- `output$something` — a slot in the UI you fill with a plot/table/text.

---

## 2. The smallest possible app

Create a file `mini.R` and run `shiny::runApp("mini.R")` (or paste into the console):

```r
library(shiny)

ui <- fluidPage(
  sliderInput("n", "How many points?", min = 10, max = 500, value = 100),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  output$scatter <- renderPlot({
    plot(rnorm(input$n), rnorm(input$n))   # input$n updates as you drag
  })
}

shinyApp(ui, server)
```

Drag the slider — the plot redraws itself. That is the whole model. Things to notice:

- `sliderInput("n", ...)` creates `input$n`.
- `plotOutput("scatter")` creates an empty slot named `scatter`.
- `renderPlot({ ... })` fills `output$scatter`. Because it *reads* `input$n`,
  Shiny re-runs it whenever `n` changes. This is reactivity — no event handling.

Every input/output type comes in a matching pair:

| You want…            | UI function           | Server function     |
|----------------------|-----------------------|---------------------|
| a plot               | `plotOutput`          | `renderPlot`        |
| an interactive plot  | `plotly::plotlyOutput`| `plotly::renderPlotly` |
| a table              | `DT::DTOutput`        | `DT::renderDT`      |
| a map                | `leaflet::leafletOutput` | `leaflet::renderLeaflet` |
| some text            | `textOutput`          | `renderText`        |
| arbitrary UI         | `uiOutput`            | `renderUI`          |

---

## 3. Inputs: reading what the user chose

Common inputs (all create `input$id`):

```r
selectInput("region", "Region", choices = c("Afar", "Amhara"))   # dropdown
sliderInput("week", "Week", min = 1, max = 52, value = 10)        # slider
checkboxInput("show", "Show target line", value = TRUE)           # tickbox
```

In the server you just use `input$region`, `input$week`, `input$show`. The first
time the app loads they already hold their default values, so your outputs render
immediately.

---

## 4. `reactive()` — compute something once, reuse it everywhere

Suppose three plots all need "the data filtered to the chosen region". You could
filter three times. Better: filter **once** in a `reactive()` and let each plot
read it. A reactive recomputes only when its inputs change, and caches its result
between changes.

```r
server <- function(input, output, session) {

  # a reactive is a function you CALL with ()
  filtered <- reactive({
    dplyr::filter(my_data, region == input$region)
  })

  output$plot1 <- renderPlot( plot(filtered()$x) )     # note filtered()
  output$plot2 <- DT::renderDT( filtered() )
}
```

Rules of thumb:
- Define a `reactive()` for any value more than one output needs.
- Call it with parentheses: `filtered()`.
- `render*()` and `reactive()` are the only places you may read `input$...`.

This is exactly what our app does — see `reg_slice()`, `nat_slice()`,
`wide_slice()` in `server.R`. They are "the data for the chosen period and
regions", computed once, read by every chart.

---

## 5. Layout with bslib (what our app uses)

Plain `fluidPage` is fine for learning. For a real dashboard we use **bslib**,
which gives Bootstrap 5 pages, a navbar with tabs, a sidebar, cards and value
boxes. The skeleton:

```r
library(bslib)
ui <- page_navbar(
  title   = "My dashboard",
  sidebar = sidebar( selectInput("region", "Region", choices = ...) ),
  nav_panel("Overview", card(card_header("A plot"), plotlyOutput("p1"))),
  nav_panel("Data",     DTOutput("t1"))
)
```

- `page_navbar()` — the whole page with a top navbar.
- `sidebar()` — the left panel of filters, **shared by all tabs**.
- `nav_panel("Title", ...)` — one tab.
- `card()` / `card_header()` — a titled box to put an output in.
- `layout_columns(col_widths = c(7, 5), cardA, cardB)` — put cards side by side
  (Bootstrap's grid is 12 wide, so `c(7, 5)` = 58% / 42%).

### Value boxes

The big number tiles on the Overview tab:

```r
value_box(title = "Woredas reporting", value = "1,082",
          showcase = shiny::icon("location-dot"), theme = "info")
```

In our app the four value boxes are built in the server (because their numbers
depend on the filters) and dropped into a `uiOutput("value_boxes")` slot — see
`output$value_boxes <- renderUI({ ... })`.

---

## 6. An interactive map in three lines

leaflet maps are themselves interactive (pan/zoom/click) without any Shiny:

```r
library(leaflet)
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  setView(lng = 39.5, lat = 9.0, zoom = 6)
```

To colour woredas by a value you join your numbers to a spatial object (an `sf`
data frame with a `geometry` column) and call `addPolygons(fillColor = ~pal(v))`.

**The fast pattern (used in our app).** Re-drawing the whole map on every change
is slow. Instead draw the base map (tiles + borders) **once**, then only recolour
the polygons using a *proxy*:

```r
output$map <- renderLeaflet( build_base_map(geo1) )   # once

observe({                                             # whenever a filter changes
  leafletProxy("map") |>
    clearGroup("kpi_layer") |>
    addPolygons(data = joined_sf, fillColor = ~pal(v), group = "kpi_layer")
})
```

`observe({ })` is like a `render`, but instead of producing an output it just
*does something* (here, update the existing map) whenever its inputs change. See
`R/map.R` and the `observe()` in `server.R`.

---

## 7. How a Shiny project is organised

A one-file app (`ui`, `server`, `shinyApp` in one script) is fine when small. Ours
is split so each piece stays readable. This is the standard layout:

```
shiny/
├── app.R         # entry point: sources the three files, calls shinyApp()
├── global.R      # runs ONCE at startup: load packages + data, define constants
├── ui.R          # the layout (no analysis)
├── server.R      # the logic (reactives + render*)
├── R/            # helper functions, sourced by global.R
│   ├── value_boxes.R
│   ├── plots.R
│   └── map.R
├── www/          # static web files served as-is (logo, custom.css)
└── data/         # the .rds the app reads (built by prep_data.R)
```

Why `global.R`? Anything defined there is visible to both `ui.R` and `server.R`,
and it runs only once no matter how many people connect — so it is where you load
data. Anything inside `server <- function(...)` runs **per user session**.

`www/` is special: files in it are served to the browser by name. That is why the
logo is `tags$img(src = "ephi_logo.png")` (no path) — the browser fetches
`www/ephi_logo.png`. (The old Malaria app pointed at an external URL, which broke
when that site was unreachable; a local `www/` copy is the fix.)

---

## 8. Reading OUR app, file by file

Open these next to this guide:

1. **`prep_data.R`** (run from the project root, not part of the running app).
   Turns the pipeline's CSV output into small `data/*.rds` files. Run it after
   every data refresh. This keeps the app fast and the deploy small.

2. **`global.R`** — loads packages, reads the `.rds`, defines constants
   (`PERIOD_CHOICES`, `ALL_REGIONS`, `INDICATORS`, the colour palette) and the
   login table `APP_CREDENTIALS`, then `source()`s the `R/` helpers.

3. **`ui.R`** — a `page_navbar` with a shared `sidebar` (period, regions,
   indicator) and six `nav_panel` tabs (Overview, Trends, Regional, Map, Explorer,
   Data). Each tab is just cards holding empty output slots. The final line wraps
   everything in `shinymanager::secure_app()` to add the login screen.

4. **`server.R`** — defines the three reactive slices, then one `render*` per
   output slot. Read it top to bottom: every output is "take a slice, hand it to a
   plotting helper". The map uses the base-once / proxy-update pattern.

5. **`R/plots.R`, `R/map.R`, `R/value_boxes.R`** — plain functions that take a
   data frame and return a plot/list. No Shiny in them, so you can test them in a
   normal console, e.g. `plot_national_bar(dplyr::filter(national, period==FOCUS_PERIOD))`.

A good exercise: add a new value box. (a) compute the number in
`R/value_boxes.R`, (b) add one more `value_box(...)` in the `renderUI` in
`server.R`, (c) widen `col_widths`. Nothing else changes.

---

## 9. The login gate (shinymanager)

Two changes turn any app into a password-protected one:

```r
# ui.R  — wrap the UI
ui <- shinymanager::secure_app(ui)

# server.R — check credentials at the top of the server
res_auth <- shinymanager::secure_server(
  check_credentials = shinymanager::check_credentials(APP_CREDENTIALS))
```

`APP_CREDENTIALS` (in `global.R`) is a data frame of `user` / `password`. Add a
row per user. For a real deployment, do not leave passwords in source — see the
note in `../reports/dashboard_and_hosting.md` on environment variables.

---

## 10. Running it locally

From the project root:

```r
# one-time: build the data the app reads
Rscript shiny/prep_data.R

# run the app (opens in your browser)
R -e 'shiny::runApp("shiny", launch.browser = TRUE)'
```

Log in with one of the users in `APP_CREDENTIALS` (e.g. `ephi` / `kpi2026`).
Change a filter and watch every tab update.

When something breaks, read the R console — Shiny prints the error and the output
that caused it. The usual beginner mistakes: forgetting the `()` when calling a
reactive, mismatched output IDs between `ui.R` and `server.R`, and reading
`input$...` outside a reactive/render.

---

## 11. Deploying to the web

That is its own walkthrough (creating the shinyapps.io account, the token, and
`rsconnect::deployApp`). It lives in **`../reports/dashboard_and_hosting.md`**,
which also covers publishing the Quarto dashboard to GitHub.

---

## 12. Where to go next

- The official tutorial: <https://shiny.posit.co/r/getting-started/>
- bslib dashboards: <https://rstudio.github.io/bslib/>
- Mastering Shiny (free book): <https://mastering-shiny.org/>
