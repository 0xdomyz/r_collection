library(shiny)
library(shinydashboard)
library(DT)
library(rmarkdown)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = input$header, sep = input$sep)
  })
  
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  output$table <- renderTable({
    req(data())
    data()
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(data = data())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
