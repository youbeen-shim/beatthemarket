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
      tags$hr(),
      actionButton("reset", "Reset Game"),
      textOutput("error"),
      tags$hr(),
      h3("SPY Performance"),
      htmlOutput("spyChange"),
      tags$hr(),
      h3("Cash Account"),
      htmlOutput("cashAvailable")
    ),
    mainPanel(
      fluidRow(
        column(12,
               h4("Initial Circumstance"),
               HTML("<p>You start with $7,500 in your investment account and $2,500 in your cash account. You will receive $1,000 each month.</p>"),
               h3("SPY Stock Prices"),
               plotOutput("stockPlot")
        )
      ),
      fluidRow(
        column(12,
               h3("Portfolio Performance"),
               tableOutput("portfolioTable")
        )
      ),
      fluidRow(
        column(12,
               h3("Investment and Cash Account Values Over Time"),
               plotOutput("accountPlot")
        )
      ),
      fluidRow(
        column(12,
               h3("Game Summary"),
               htmlOutput("summary")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize portfolio with empty data frame
  initial_portfolio <- data.frame(
    Month = as.Date(character()),
    Investment = numeric(),
    CumulativeInvestment = numeric(),
    Value = numeric(),
    MarketChangeValue = numeric(),
    Cash = numeric(),
    InterestAdjustedCash = numeric(),
    AmountAvailableForInvestment = numeric(),
    stringsAsFactors = FALSE
  )
  portfolio <- reactiveVal(initial_portfolio)
  
  # Error message
  error_message <- reactiveVal("")
  
  # Generate dynamic UI for investment input
  output$investmentUI <- renderUI({
    current_portfolio <- portfolio()
    available_cash <- if (nrow(current_portfolio) == 0) 2500 else tail(current_portfolio$AmountAvailableForInvestment, 1)
    
    numericInput("investment", 
                 paste("Investment Amount (out of $", round(available_cash, 2), "):", sep = ""), 
                 value = 0, min = 0, max = available_cash, step = 50)
  })
  
  observeEvent(input$submit, {
    current_portfolio <- portfolio()
    current_month <- nrow(current_portfolio) + 1
    error_message("")
    
    # Check if this is the first month and initialize the portfolio
    if (current_month == 1) {
      new_entry <- data.frame(
        Month = spy_data$month[current_month],
        Investment = 7500,
        CumulativeInvestment = 7500,
        Value = 7500,
        MarketChangeValue = 7500,
        Cash = 2500,
        InterestAdjustedCash = 2500 * 1.05,
        AmountAvailableForInvestment = 2500 * 1.05 + 1000,
        stringsAsFactors = FALSE
      )
      portfolio(rbind(current_portfolio, new_entry))
      current_portfolio <- portfolio()
      current_month <- nrow(current_portfolio) + 1
    }
    
    if (current_month <= nrow(spy_data)) {
      investment <- input$investment
      sell_amount <- input$sell
      last_cash <- if (current_month == 1) 2500 else tail(current_portfolio$AmountAvailableForInvestment, 1)
      previous_value <- if (current_month == 1) 7500 else tail(current_portfolio$MarketChangeValue, 1)
      
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
        market_change_value <- investment - sell_amount + 7500
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
      
      # Display the summary when the last month is reached
      if (current_month == nrow(spy_data)) {
        total_income <- (current_month - 1) * 1000
        starting_amount <- 10000
        ending_amount <- value + cash
        account_growth <- (ending_amount - starting_amount) / starting_amount * 100
        raw_investment_growth <- (ending_amount - total_income - starting_amount) / starting_amount * 100
        summary_text <- paste(
          "<h4>Summary</h4>",
          "<p><b>Starting Amount:</b> $10,000</p>",
          "<p><b>Total Income:</b> $", total_income, "</p>",
          "<p><b>Ending Amount:</b> $", round(ending_amount, 2), "</p>",
          "<p><b>Account Growth (%):</b> ", round(account_growth, 2), "%</p>",
          "<p><b>Account Growth (Raw Investment %):</b> ", round(raw_investment_growth, 2), "%</p>"
        )
        output$summary <- renderUI({
          HTML(summary_text)
        })
      }
    }
  })
  
  observeEvent(input$reset, {
    portfolio(initial_portfolio)
    error_message("")
    output$summary <- renderUI({
      HTML("<p>Game reset. Start a new game by making investments.</p>")
    })
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
  
  output$cashAvailable <- renderUI({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) == 0) {
      HTML("<h4>Cash available for next month:</h4> <b>Data will be displayed here.</b>")
    } else {
      latest_entry <- tail(current_portfolio, 1)
      HTML(paste("<h4>Cash available for next month:</h4>", "<b>$", round(latest_entry$AmountAvailableForInvestment, 2), "</b>"))
    }
  })
  
  output$accountPlot <- renderPlot({
    current_portfolio <- portfolio()
    if (nrow(current_portfolio) > 0) {
      account_data <- current_portfolio %>%
        select(Month, Cash, Investment = MarketChangeValue) %>%
        gather(key = "AccountType", value = "Amount", -Month)
      
      ggplot(account_data, aes(x = Month, y = Amount, color = AccountType)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Investment and Cash Account Values Over Time", x = "Month", y = "Amount ($)", color = "Account Type") +
        scale_y_continuous(labels = scales::dollar)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


