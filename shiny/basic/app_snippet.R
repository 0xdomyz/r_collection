library(shiny)

ui <- fluidPage(
    fluidRow(
        sliderInput("input1", "Input:", 1, 20, 10),
        plotOutput("plot1")
    )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    hist(iris$Petal.Length, breaks = input$input1)
  })
}

shinyApp(ui, server)