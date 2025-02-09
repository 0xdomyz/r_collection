library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Data Visualization Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar"))
    ),
    selectInput("xvar", "X-axis variable", choices = names(mtcars)),
    selectInput("yvar", "Y-axis variable", choices = names(mtcars)),
    selectInput("plotType", "Plot Type", choices = c("Scatter Plot", "Box Plot", "Histogram"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "visualizations",
              fluidRow(
                box(title = "Plot", plotOutput("plot"), width = 12)
              )
      )
    )
  )
)
