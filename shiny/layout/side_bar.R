library(shiny)

ui <- fluidPage(
  fluidPage(
    titlePanel("a"), 
    sidebarLayout(
      sidebarPanel(numericInput("n", "n", 5)),
      mainPanel(plotOutput("plot"))
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(replicate(1000,mean(rbeta(input$n,1,1))))
  })
}

shinyApp(ui, server)