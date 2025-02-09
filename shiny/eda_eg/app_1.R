library(shiny)
library(shinydashboard)
library(vroom)
library(tidyverse)

# set up
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")
print("Data loaded")

ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(column(
      8,
      selectInput("prod_title", "Product", choices = products$title)
    ), column(
      4,
      selectInput(
        "y_axis",
        "Y axis",
        choices = c("count", "rate"),
        selected = "count"
      )
    )),
    fluidRow(
      column(4, tableOutput("diag")),
      column(4, tableOutput("body")),
      column(4, tableOutput("location"))
    ),
    fluidRow(column(12, plotOutput("injuries"))),
      fluidRow(
    column(2, actionButton("story", "show a story")),
    column(10, textOutput("narrative"))
  )
  )
)


server <- function(input, output) {
  print("Server ran")
  
  selected = reactive({
    prod_code = products[products$title == input$prod_title, "prod_code"] %>% as.numeric()
    df = injuries %>% filter(prod_code == {{ prod_code }})
    print(paste0(
      "selected ",
      as.character(nrow(df)),
      " rows, prod_code = ",
      as.character(prod_code)
    ))
    return(df)
  })
  
  # tables
  output$diag <- renderTable({
    diag <- selected() %>% count(diag, wt = weight, sort = TRUE)
    return(diag)
  })
  output$body <- renderTable(selected() %>% count(body_part, wt = weight, sort = TRUE))
  output$location <- renderTable(selected() %>% count(location, wt = weight, sort = TRUE))
  
  # chart
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  output$injuries <- renderPlot({
    
    if (input$y_axis == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries estimate numbers")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  })

  # story
  narrative = reactive({
    if (input$story > 0) {
      story = selected() %>%
        sample_n(1) %>%
        pull(narrative)
      return(story)
    } else {
      return(NULL)
    }
  })
  output$narrative <- renderText(narrative())

}


shinyApp(ui = ui, server = server)
