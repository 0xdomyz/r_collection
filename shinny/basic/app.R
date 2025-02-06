library(shiny)

# Source the UI and server scripts
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
