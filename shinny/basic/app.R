library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
# input
      box(
        title = "Controls",
        sliderInput("input1", "Number of observations:", 1, 100, 50)
      )
    )
  )
)


server <- function(input, output) {
  # set up
  set.seed(122)
  histdata <- rnorm(500)

  # process & results
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$input1)]
    hist(data)
  })
}


shinyApp(ui = ui, server = server)
