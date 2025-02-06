library(shiny)
library(shinydashboard)
library(ggplot2)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    ggplot(iris, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
      geom_point() +
      labs(title = "Scatter Plot", x = input$xvar, y = input$yvar)
  })
  
  output$boxPlot <- renderPlot({
    ggplot(iris, aes_string(x = input$color, y = input$yvar, fill = input$color)) +
      geom_boxplot() +
      labs(title = "Box Plot", x = input$color, y = input$yvar)
  })
  
  output$summary <- renderPrint({
    summary(iris)
  })
}

shinyApp(ui, server)
