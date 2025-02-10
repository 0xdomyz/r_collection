library(shiny)

ui <- fluidPage(
    fluidPage(
        fluidRow(
            column(4, 
                numericInput("n", "Sample size", 2)
            ),
            column(8, 
                sliderInput("alpha", "Alpha", 0, 10, 1, 0.1)
            )
        ),
        fluidRow(
            column(6, 
                selectInput("beta", "Beta", c(1, 2, 3))
            ),
            column(6, 
                textInput("gamma", "Gamma", "Hello, world!")
            )
        ),
        plotOutput("plot")
    )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(replicate(100,mean(
        rbeta(input$n, input$alpha, as.numeric(input$beta))
        ))
    )
  })
}

shinyApp(ui, server)

