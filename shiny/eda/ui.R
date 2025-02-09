library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Summary Statistics", tabName = "summary", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h2("Welcome to the Data Science Dashboard"),
              p("This dashboard provides an exploratory analysis of the Iris dataset.")
      ),
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Scatter Plot", plotOutput("scatterPlot"), width = 6),
                box(title = "Box Plot", plotOutput("boxPlot"), width = 6)
              ),
              fluidRow(
                box(title = "Controls", 
                    selectInput("xvar", "X-axis variable", choices = names(iris)[1:4]),
                    selectInput("yvar", "Y-axis variable", choices = names(iris)[1:4]),
                    selectInput("color", "Color by", choices = names(iris)[5]),
                    width = 12)
              )
      ),
      tabItem(tabName = "summary",
              h2("Summary Statistics"),
              verbatimTextOutput("summary")
      )
    )
  )
)
