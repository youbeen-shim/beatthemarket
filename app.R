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
      numericInput("sell", "Sell Amount:", value = 0, min = 0),
      actionButton("submit", "Submit for this Month"),
      textOutput("error")
    ),
    mainPanel(
      fluidRow(
        column(12,
               h3("SPY Performance"),
               htmlOutput("spyChange")
        )
      ),
      fluidRow(
        column(12,
               h3("Cash Account"),
               htmlOutput("cashAvailable")
        )
      ),
      fluidRow(
        column(12,
               h3("SPY Stock Prices"),
               plotOutput("stockPlot")
        )
      ),
      fluidRow(
        column(12,
               h3("Portfolio Performance"),
               tableOutput("portfolioTable")
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
    InterestAdjustedCash = numeric(),
    AmountAvailableForInvestment = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Error message
  error_message <- reactiveVal("")
  
  # Generate dynamic UI for investment input
  output$investmentUI <- renderUI({
    current_portfolio <- portfolio()
    available_cash <- if (nrow(current_portfolio) == 0) 1000 else tail(current_portfolio$AmountAvailableForInvestment, 1)
    
    numericInput("investment", 
                 paste("Investment Amount (out of $", round(available_cash, 2), "):", sep = ""), 
                 value = 0, min = 0, max = available_cash, step = 50)
  })
  
  observeEvent(input$submit, {
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    error_message("")
    
    if (current_month <= nrow(spy_data)) {
      investment <- input$investment
      sell_amount <- input$sell
      last_cash <- if (current_month == 1) 1000 else tail(current_portfolio$AmountAvailableForInvestment, 1)
      previous_value <- if (current_month == 1) 0 else tail(current_portfolio$MarketChangeValue, 1)
      
      # Check for illegal actions
      if (investment > last_cash) {
        error_message("Error: Investment amount exceeds available cash.")
        return()
      }
      if (sell_amount > previous_value) {
        error_message("Error: Sell amount exceeds portfolio value.")
        return()
      }
      
      # Calculate cash, interest adjusted cash, and amount available for investment
      cash <- last_cash - investment + sell_amount
      interest_adjusted_cash <- cash * 1.05
      amount_available_for_investment <- interest_adjusted_cash + 1000
      
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
        Cash = cash,
        InterestAdjustedCash = interest_adjusted_cash,
        AmountAvailableForInvestment = amount_available_for_investment,
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
        Value = round(Value, 2),
        Cash = round(Cash, 2),
        InterestAdjustedCash = round(InterestAdjustedCash, 2),
        AmountAvailableForInvestment = round(AmountAvailableForInvestment, 2)
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
      HTML(paste("<h4>Cash available for next month:</h4>", "<b>$", round(latest_entry$AmountAvailableForInvestment, 2), "</b>"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
