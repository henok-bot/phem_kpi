# app.R
# Entry point. Run locally with:
#   shiny::runApp("shiny")            # from the project root
# or open this file in RStudio and click "Run App".
#
# The three sources below run in order: global (data + helpers), ui (layout),
# server (logic). shinyApp() ties the ui and server together.
source("global.R", local = FALSE)
source("ui.R",     local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui = ui, server = server)
