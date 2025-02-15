library(shiny)
library(shinydashboard)
library(vroom)
library(tidyverse)

# set up
data = read.csv("Comprehensive_Banking_Database.csv")
print("Data loaded")

# processing 
data$Transaction.Date = as.Date(data$Transaction.Date, format = "%m/%d/%Y")
data$Transaction.EOM = as.Date(paste0(format(data$Transaction.Date, "%Y-%m"), "-01")) + months(1) - days(1)
print("Data processed")

date_choices = as.character(sort(unique(data$Transaction.EOM)))

ui <- dashboardPage(
  dashboardHeader(title = "Basic Interactive Dashboard"),
  dashboardSidebar(
    selectInput("Account.Type", "Account Type", 
      choices = unique(data$Account.Type), selected = unique(data$Account.Type)[1]),
    selectInput("Transaction.EOM.min", "Transaction EOM Min", 
      choices = date_choices, selected = date_choices[1]),
    selectInput("Transaction.EOM.max", "Transaction EOM Max", 
      choices = date_choices, selected = date_choices[length(date_choices)]),
    selectInput("Transaction.Type", "Transaction Type", 
      choices = unique(data$Transaction.Type), selected = unique(data$Transaction.Type)[1])
  ),
  dashboardBody(
    fluidRow(
      column(6, plotOutput("count")),
      column(6, plotOutput("volume"))
    ),
    fluidRow(
      column(12, plotOutput("rate"))
    )
  )
)


server <- function(input, output) {
  print("Server ran")
  
  a_ts = reactive({
    print(nrow(data))
    print(input$Account.Type)
    print(input$Transaction.EOM.min)
    print(input$Transaction.EOM.max)
    data_filtered = data %>% filter(Account.Type == input$Account.Type, 
                                Transaction.EOM >= input$Transaction.EOM.min, 
                                Transaction.EOM <= input$Transaction.EOM.max, 
                                Transaction.Type == input$Transaction.Type)
    
    a_ts = data_filtered %>% 
      group_by(Transaction.EOM) %>%
      summarise(
          distinct_customers = n_distinct(Customer.ID),
          total_transactions = n(),
          total_amount = sum(Transaction.Amount),
          pct = sum(Loan.Status == "Rejected") / n()
      )

    return(a_ts)
  })
    
  # chart
  output$count <- renderPlot({
    a_ts() %>% 
      ggplot(aes(x = Transaction.EOM, y = total_transactions)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$volume <- renderPlot({
    a_ts() %>% 
      ggplot(aes(x = Transaction.EOM, y = total_amount)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$rate <- renderPlot({
    a_ts() %>% 
      ggplot(aes(x = Transaction.EOM, y = pct)) +
      geom_line() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

}


shinyApp(ui = ui, server = server)
