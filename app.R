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
# library(shiny)
# library(quantmod)
# library(dplyr)
# library(ggplot2)
# library(shinydashboard)
# library(lubridate)
# 
# # Fetch SPY stock prices for the last 24 months
# getSymbols("SPY", src = "yahoo", from = Sys.Date() - months(24), to = Sys.Date())
# spy_data <- data.frame(date = index(SPY), coredata(SPY))
# 
# # Define UI
# ui <- dashboardPage(
#   dashboardHeader(title = "Investment Game"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#       menuItem("Portfolio", tabName = "portfolio", icon = icon("wallet")),
#       menuItem("Summary", tabName = "summary", icon = icon("chart-line"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "dashboard",
#               fluidRow(
#                 box(plotOutput("stockPlot"), width = 12)
#               ),
#               fluidRow(
#                 box(selectInput("action", "Action:", choices = c("Invest", "Pull Out")), width = 4),
#                 box(numericInput("amount", "Amount:", value = 1000, min = 0), width = 4),
#                 box(actionButton("submit", "Submit"), width = 4)
#               )
#       ),
#       tabItem(tabName = "portfolio",
#               fluidRow(
#                 box(dataTableOutput("portfolioTable"), width = 12)
#               )
#       ),
#       tabItem(tabName = "summary",
#               fluidRow(
#                 box(plotOutput("performancePlot"), width = 12),
#                 box(textOutput("summaryText"), width = 12)
#               )
#       )
#     )
#   )
# )
# 
# # Define server logic
# server <- function(input, output, session) {
#   # Initialize portfolio
#   portfolio <- reactiveVal(data.frame(
#     Month = character(),
#     Action = character(),
#     Amount = numeric(),
#     Value = numeric(),
#     stringsAsFactors = FALSE
#   ))
#   
#   observeEvent(input$submit, {
#     current_portfolio <- portfolio()
#     current_month <- nrow(current_portfolio) + 1
#     
#     if (current_month <= 24) {
#       new_entry <- data.frame(
#         Month = as.character(current_month),
#         Action = input$action,
#         Amount = input$amount,
#         Value = ifelse(input$action == "Invest", input$amount * as.numeric(Cl(SPY)[current_month]), -input$amount),
#         stringsAsFactors = FALSE
#       )
#       portfolio(rbind(current_portfolio, new_entry))
#     }
#   })
#   
#   output$stockPlot <- renderPlot({
#     ggplot(spy_data, aes(x = date, y = SPY.Close)) +
#       geom_line() +
#       labs(title = "SPY Stock Prices", x = "Date", y = "Price")
#   })
#   
#   output$portfolioTable <- renderDataTable({
#     portfolio()
#   })
#   
#   output$performancePlot <- renderPlot({
#     portfolio_data <- portfolio()
#     flat_invest <- seq(1, 24) * 1000
#     
#     user_invest <- cumsum(portfolio_data$Value)
#     flat_value <- cumsum(flat_invest * as.numeric(Cl(SPY)[1:24]))
#     
#     performance_data <- data.frame(
#       Month = 1:24,
#       User = user_invest,
#       Flat = flat_value
#     )
#     
#     ggplot(performance_data, aes(x = Month)) +
#       geom_line(aes(y = User, color = "User")) +
#       geom_line(aes(y = Flat, color = "Flat")) +
#       labs(title = "Portfolio Performance", x = "Month", y = "Value")
#   })
#   
#   output$summaryText <- renderText({
#     portfolio_data <- portfolio()
#     user_value <- sum(portfolio_data$Value)
#     flat_value <- sum(seq(1, 24) * 1000 * as.numeric(Cl(SPY)[1:24]))
#     
#     paste("User Portfolio Value: $", round(user_value, 2), "\nFlat Strategy Value: $", round(flat_value, 2))
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(lubridate)

# Fetch SPY stock prices for the last 24 months
start_date <- Sys.Date() - months(24)
end_date <- Sys.Date()
getSymbols("SPY", src = "yahoo", from = start_date, to = end_date)
spy_data <- data.frame(date = index(SPY), coredata(SPY))

# Calculate monthly returns
spy_data <- spy_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(open = first(SPY.Open), close = last(SPY.Close)) %>%
  mutate(perc_change = (close / lag(close) - 1) * 100)

# Define UI
ui <- fluidPage(
  titlePanel("SPY Investment Portfolio"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("investmentUI"),
      numericInput("sell", "Sell Amount:", value = 0, min = 0, step = 50),
      actionButton("submit", "Submit for this Month"),
      textOutput("error")
    ),
    mainPanel(
      fluidRow(
        box(
          title = "SPY Stock Prices",
          width = 12,
          plotOutput("stockPlot")
        )
      ),
      fluidRow(
        box(
          title = "Portfolio Performance",
          width = 12,
          tableOutput("portfolioTable")
        )
      ),
      fluidRow(
        box(
          title = "SPY Performance",
          width = 12,
          htmlOutput("spyChange")
        )
      ),
      fluidRow(
        box(
          title = "Cash Account",
          width = 12,
          htmlOutput("cashAvailable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize portfolio
  portfolio <- reactiveVal(data.frame(
    Month = as.Date(character()),
    Investment = numeric(),
    CumulativeInvestment = numeric(),
    Value = numeric(),
    MarketChangeValue = numeric(),
    Cash = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Error message
  error_message <- reactiveVal("")
  
  # Generate dynamic UI for investment input
  output$investmentUI <- renderUI({
    current_portfolio <- portfolio()
    available_cash <- if (nrow(current_portfolio) == 0) 1000 else tail(current_portfolio$Cash, 1) + 1000
    
    numericInput("investment", paste("Investment Amount (out of $", round(available_cash, 2), "):", sep = ""), value = 0, min = 0, max = available_cash, step = 50)
  })
  
  observeEvent(input$submit, {
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    error_message("")
    
    if (current_month <= nrow(spy_data)) {
      investment <- input$investment
      sell_amount <- input$sell
      last_cash <- if (current_month == 1) 1000 else tail(current_portfolio$Cash, 1)
      previous_value <- if (current_month == 1) 0 else tail(current_portfolio$MarketChangeValue, 1)
      
      # Check for illegal actions
      if (investment > last_cash + 1000) {
        error_message("Error: Investment amount exceeds available cash.")
        return()
      }
      if (sell_amount > previous_value) {
        error_message("Error: Sell amount exceeds portfolio value.")
        return()
      }
      
      # Calculate new cash including the sell amount and interest on previous cash
      new_cash <- (last_cash - investment + sell_amount) * 1.05 + (1000 - investment)
      
      cumulative_investment <- sum(current_portfolio$Investment) + investment
      
      if (current_month == 1) {
        market_change_value <- investment - sell_amount
      } else {
        change <- spy_data$close[current_month] / spy_data$close[current_month - 1]
        market_change_value <- (previous_value - sell_amount) * change + investment
      }
      
      value <- market_change_value
      
      new_entry <- data.frame(
        Month = spy_data$month[current_month],
        Investment = investment,
        CumulativeInvestment = cumulative_investment,
        Value = value,
        MarketChangeValue = market_change_value,
        Cash = new_cash,
        stringsAsFactors = FALSE
      )
      portfolio(rbind(current_portfolio, new_entry))
    }
  })
  
  output$error <- renderText({
    error_message()
  })
  
  output$stockPlot <- renderPlot({
    current_portfolio <- portfolio()
    invested_months <- as.Date(current_portfolio$Month)
    
    spy_data <- spy_data %>%
      mutate(point_color = ifelse(month %in% tail(invested_months, 1), "red", "grey"))
    
    ggplot(spy_data, aes(x = month, y = close)) +
      geom_line() +
      geom_point(aes(color = point_color), size = 3) +
      scale_color_identity() +
      labs(title = "SPY Stock Prices", x = "Date", y = "Price")
  })
  
  output$portfolioTable <- renderTable({
    req(input$submit)
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      return(data.frame("No data available yet."))
    }
    
    current_portfolio %>%
      mutate(
        Month = format(as.Date(Month), "%Y-%m"),
        Investment = round(Investment, 2),
        CumulativeInvestment = round(CumulativeInvestment, 2),
        Value = round(Value, 2)
      )
  }, rownames = FALSE)
  
  output$spyChange <- renderUI({
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    
    if (current_month > 1 && current_month <= nrow(spy_data)) {
      spy_change <- spy_data$perc_change[current_month]
      HTML(paste("<h4>SPY % Change from last month:</h4>", "<b>", round(spy_change, 2), "%</b>"))
    } else {
      HTML("<h4>SPY % Change from last month:</h4> <b>Data will be displayed here.</b>")
    }
  })
  
  output$portfolioChange <- renderUI({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) > 1) {
      last_market_change_value <- tail(current_portfolio$MarketChangeValue, 1)
      prev_market_change_value <- current_portfolio$MarketChangeValue[nrow(current_portfolio) - 1]
      portfolio_change_due_to_market <- (last_market_change_value / prev_market_change_value - 1) * 100
      paste("Portfolio % Change due to market conditions:", round(portfolio_change_due_to_market, 2), "%")
    } else {
      "Portfolio % Change due to market conditions will be displayed here."
    }
  })
  
  output$cashAvailable <- renderUI({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      HTML("<h4>Cash available for next month:</h4> <b>Data will be displayed here.</b>")
    } else {
      latest_entry <- tail(current_portfolio, 1)
      HTML(paste("<h4>Cash available for next month:</h4>", "<b>$", round(latest_entry$Cash, 2), "</b>"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
