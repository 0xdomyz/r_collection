library(shiny)
library(shinydashboard)

server <- function(input, output) {
  set.seed(123)
  data <- reactive({
    n <- input$slider
    x <- rnorm(n)
    y <- 2 * x + rnorm(n)
    data.frame(x = x, y = y)
  })
  
  output$scatterPlot <- renderPlot({
    plot(data()$x, data()$y, main = "Scatter Plot", xlab = "X", ylab = "Y")
  })
  
  output$modelSummary <- renderPrint({
    model <- lm(y ~ x, data = data())
    summary(model)
  })
  
  output$regPlot <- renderPlot({
    model <- lm(y ~ x, data = data())
    plot(data()$x, data()$y, main = "Regression Plot", xlab = "X", ylab = "Y")
    abline(model, col = "red")
  })
}

shinyApp(ui, server)
