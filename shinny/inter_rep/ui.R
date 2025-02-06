library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Interactive Reporting Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Generate Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fileInput("file", "Choose CSV File",
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              tags$hr(),
              checkboxInput("header", "Header", TRUE),
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ",")
      ),
      tabItem(tabName = "analysis",
              h2("Data Analysis"),
              fluidRow(
                box(title = "Summary Statistics", verbatimTextOutput("summary")),
                box(title = "Data Table", tableOutput("table"))
              )
      ),
      tabItem(tabName = "report",
              h2("Generate Report"),
              downloadButton("report", "Download Report")
      )
    )
  )
)
