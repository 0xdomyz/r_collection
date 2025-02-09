library(shiny)
library(shinydashboard)

# set up
set.seed(122)
histdata <- rnorm(500)

ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
# input
      column(
        4,
        sliderInput("input1", "Number of observations:", 1, 100, 50)
      ),
# output
      column(8, plotOutput("plot1", height = 250))
    )
  )
)


server <- function(input, output) {


  # process & results
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$input1)]
    hist(data)
  })
}


shinyApp(ui = ui, server = server)
