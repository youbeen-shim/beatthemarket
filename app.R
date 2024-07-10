# # Load packages ----
# library(shiny)
# library(bslib)
# library(quantmod)
# 
# # Source helpers ----
# source("helpers.R")
# 
# # User interface ----
# ui <- page_sidebar(
#   title = "stockVis",
#   sidebar = sidebar(
#     helpText(
#       "Select a stock to examine. 
#        (Information will be collected from Yahoo finance)"
#     ),
#     textInput("symb", "Symbol", "SPY"),
#     dateRangeInput(
#       "dates",
#       "Date range",
#       start = "2020-01-01",
#       end = as.character(Sys.Date())
#     ),
#     br(),
#     br(),
#     checkboxInput(
#       "log",
#       "Plot y axis on log scale",
#       value = FALSE
#     ),
#     checkboxInput(
#       "adjust",
#       "Adjust prices for inflation",
#       value = FALSE
#     )
#   ),
#   card(
#     card_header("Price over time"),
#     plotOutput("plot")
#   )
# )
# 
# # Server logic
# server <- function(input, output) {
#   
#   dataInput <- reactive({
#     getSymbols(input$symb, src = "yahoo",
#                from = input$dates[1],
#                to = input$dates[2],
#                auto.assign = FALSE)
#   })
#   
#   inflationAdj <- reactive({
#     if (!input$adjust) return(dataInput())
#     adjust(dataInput())
#   })
#   
#   output$plot <- renderPlot({
#     chartSeries(inflationAdj(), theme = chartTheme("white"),
#                 type = "line", log.scale = input$log, TA = NULL)
#   })
#   
# }
# 
# # Run the app
# shinyApp(ui, server)
library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(lubridate)

# Fetch SPY stock prices for the last 24 months
getSymbols("SPY", src = "yahoo", from = Sys.Date() - months(24), to = Sys.Date())
spy_data <- data.frame(date = index(SPY), coredata(SPY))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Investment Game"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Portfolio", tabName = "portfolio", icon = icon("wallet")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("stockPlot"), width = 12)
              ),
              fluidRow(
                box(selectInput("action", "Action:", choices = c("Invest", "Pull Out")), width = 4),
                box(numericInput("amount", "Amount:", value = 1000, min = 0), width = 4),
                box(actionButton("submit", "Submit"), width = 4)
              )
      ),
      tabItem(tabName = "portfolio",
              fluidRow(
                box(dataTableOutput("portfolioTable"), width = 12)
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(plotOutput("performancePlot"), width = 12),
                box(textOutput("summaryText"), width = 12)
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize portfolio
  portfolio <- reactiveVal(data.frame(
    Month = character(),
    Action = character(),
    Amount = numeric(),
    Value = numeric(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$submit, {
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    
    if (current_month <= 24) {
      new_entry <- data.frame(
        Month = as.character(current_month),
        Action = input$action,
        Amount = input$amount,
        Value = ifelse(input$action == "Invest", input$amount * as.numeric(Cl(SPY)[current_month]), -input$amount),
        stringsAsFactors = FALSE
      )
      portfolio(rbind(current_portfolio, new_entry))
    }
  })
  
  output$stockPlot <- renderPlot({
    ggplot(spy_data, aes(x = date, y = SPY.Close)) +
      geom_line() +
      labs(title = "SPY Stock Prices", x = "Date", y = "Price")
  })
  
  output$portfolioTable <- renderDataTable({
    portfolio()
  })
  
  output$performancePlot <- renderPlot({
    portfolio_data <- portfolio()
    flat_invest <- seq(1, 24) * 1000
    
    user_invest <- cumsum(portfolio_data$Value)
    flat_value <- cumsum(flat_invest * as.numeric(Cl(SPY)[1:24]))
    
    performance_data <- data.frame(
      Month = 1:24,
      User = user_invest,
      Flat = flat_value
    )
    
    ggplot(performance_data, aes(x = Month)) +
      geom_line(aes(y = User, color = "User")) +
      geom_line(aes(y = Flat, color = "Flat")) +
      labs(title = "Portfolio Performance", x = "Month", y = "Value")
  })
  
  output$summaryText <- renderText({
    portfolio_data <- portfolio()
    user_value <- sum(portfolio_data$Value)
    flat_value <- sum(seq(1, 24) * 1000 * as.numeric(Cl(SPY)[1:24]))
    
    paste("User Portfolio Value: $", round(user_value, 2), "\nFlat Strategy Value: $", round(flat_value, 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)