library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Statistical Modeling Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Model", tabName = "model", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Scatter Plot", plotOutput("scatterPlot"), width = 6),
                box(title = "Model Summary", verbatimTextOutput("modelSummary"), width = 6)
              ),
              fluidRow(
                box(title = "Controls", 
                    sliderInput("slider", "Number of Observations:", min = 10, max = 100, value = 50), 
                    width = 12)
              )
      ),
      tabItem(tabName = "model",
              h2("Linear Regression Model"),
              fluidRow(
                box(title = "Regression Plot", plotOutput("regPlot"), width = 12)
              )
      )
    )
  )
)
