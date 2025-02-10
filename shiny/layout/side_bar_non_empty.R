library(shiny)

ui <- fluidPage(
  fluidPage(
    titlePanel("App"), 
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "input2",
          "Input:",
          choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
        ),
        sliderInput("input1", "Input:", 1, 20, 10)
      ),
      mainPanel(plotOutput("plot1"))
    )
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    hist(iris[[input$input2]], breaks = input$input1)
  })
}

shinyApp(ui, server)