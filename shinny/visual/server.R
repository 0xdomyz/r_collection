library(shiny)
library(shinydashboard)
library(ggplot2)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    p <- ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar))
    
    if (input$plotType == "Scatter Plot") {
      p <- p + geom_point() + labs(title = "Scatter Plot")
    } else if (input$plotType == "Box Plot") {
      p <- p + geom_boxplot(aes_string(group = input$xvar)) + labs(title = "Box Plot")
    } else if (input$plotType == "Histogram") {
      p <- p + geom_histogram(binwidth = 1) + labs(title = "Histogram")
    }
    
    p
  })
}

shinyApp(ui, server)
