# ==============================================================================
# BDA400 / Data Science Tools and Techniques - Assignment 6
# Title: Interactive Portfolio & Technical Analysis Dashboard (R Shiny)
# ==============================================================================

# ------------------------------------------------------------------------------
# STEP 1: DATA COLLECTION AND SETUP (Libraries & Error-Handled Fetching)
# ------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)
library(patchwork)
library(bslib)

# Helper function to fetch data safely with exception handling & multiple sources
fetch_stock_data <- function(symbol, source_choice, start_date, end_date) {
  tryCatch({
    validate(
      need(nchar(trimws(symbol)) > 0, "Please enter a valid ticker symbol."),
      need(start_date < end_date, "Start Date must be earlier than End Date.")
    )
    
    # Fetch from selected source (Yahoo Finance or FRED)
    raw_data <- getSymbols(
      Symbols = toupper(trimws(symbol)),
      src = source_choice,
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    )
    
    # Handle missing values (NA)
    raw_data <- na.omit(raw_data)
    return(raw_data)
    
  }, error = function(e) {
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# STEP 2: VISUALIZING STOCK DATA (User Interface / UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel("BDA400 Portfolio Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # --- Section 1: Data Source & Symbol Controls ---
      h4("1. Data Selection"),
      selectInput(
        "data_source",
        "Select Data Source:",
        choices = c("Yahoo Finance" = "yahoo", "FRED (Economic Data)" = "FRED"),
        selected = "yahoo"
      ),
      textInput("stock_symbol", "Enter Ticker Symbol:", value = "AAPL"),
      helpText("Examples: AAPL, MSFT, TSLA, GOOGL (or DGS10 for FRED)"),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = Sys.Date()
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      hr(),
      
      # --- Section 2: Chart Type Customization ---
      h4("2. Chart Visualization"),
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Candlestick", "Line Graph", "Area Chart"),
        selected = "Candlestick"
      ),
      
      hr(),
      
      # --- Section 3: Technical Indicators (Dynamic On/Off Toggles) ---
      h4("3. Technical Indicators"),
      checkboxGroupInput(
        "technical_indicators",
        "Toggle Indicators On/Off:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI")
      ),
      
      hr(),
      
      # --- Section 4: Customizable Trading Strategy Parameters ---
      h4("4. Trading Rule Parameters"),
      sliderInput("short_ma_len", "Short-Term SMA Window:", min = 5, max = 30, value = 10),
      sliderInput("long_ma_len", "Long-Term SMA Window:", min = 20, max = 100, value = 30),
      checkboxInput("use_rsi_filter", "Filter Signals with RSI (Buy < 70, Sell > 30)", value = TRUE),
      checkboxInput("show_annotations", "Show Buy/Sell Annotations on Chart", value = TRUE)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Interactive Technical Chart",
          br(),
          plotOutput("stock_chart", height = "720px")
        ),
        tabPanel(
          "Trading Signals Log",
          br(),
          h4("Generated Buy / Sell Crossover Signals"),
          tableOutput("signal_table")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# STEPS 2, 3 & 4: SERVER LOGIC (Filtering, Indicators, Rules & Annotations)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive dataset with exception handling and timeframe aggregation
  processed_data <- reactive({
    req(input$stock_symbol, input$date_range)
    
    raw_xts <- fetch_stock_data(
      symbol = input$stock_symbol,
      source_choice = input$data_source,
      start_date = input$date_range[1],
      end_date = input$date_range[2]
    )
    
    validate(
      need(!is.null(raw_xts) && nrow(raw_xts) > 5,
           "Error: Could not fetch sufficient data. Check the ticker symbol, data source, or expand the date range.")
    )
    
    # Convert timeframe (Daily, Weekly, Monthly)
    if (input$time_frame == "Weekly") {
      raw_xts <- to.weekly(raw_xts, indexAt = "lastof", drop.time = TRUE)
    } else if (input$time_frame == "Monthly") {
      raw_xts <- to.monthly(raw_xts, indexAt = "lastof", drop.time = TRUE)
    }
    
    # Standardize columns for both OHLC (Yahoo) and single-column (FRED) data
    if (ncol(raw_xts) >= 4) {
      df <- data.frame(
        Date   = as.Date(index(raw_xts)),
        Open   = as.numeric(Op(raw_xts)),
        High   = as.numeric(Hi(raw_xts)),
        Low    = as.numeric(Lo(raw_xts)),
        Close  = as.numeric(Cl(raw_xts))
      )
    } else {
      val <- as.numeric(raw_xts[, 1])
      df <- data.frame(
        Date  = as.Date(index(raw_xts)),
        Open  = val,
        High  = val,
        Low   = val,
        Close = val
      )
    }
    
    df <- na.omit(df)
    n_rows <- nrow(df)
    
    # Ensure window sizes do not exceed available data rows
    s_len <- min(input$short_ma_len, max(2, floor(n_rows / 3)))
    l_len <- min(input$long_ma_len, max(3, floor(n_rows / 2)))
    
    # --- STEP 3: Calculate Technical Indicators ---
    df$Short_MA <- SMA(df$Close, n = s_len)
    df$Long_MA  <- SMA(df$Close, n = l_len)
    
    if (n_rows > 15) {
      df$RSI <- RSI(df$Close, n = 14)
    } else {
      df$RSI <- NA
    }
    
    if (n_rows > 35) {
      macd_vals <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = SMA)
      df$MACD <- macd_vals[, "macd"]
      df$MACD_Signal <- macd_vals[, "signal"]
    } else {
      df$MACD <- NA
      df$MACD_Signal <- NA
    }
    
    # --- STEP 4: Implement Trading Rules ---
    # Detect exact crossover points where Short MA crosses Long MA
    prev_short <- lag(df$Short_MA)
    prev_long  <- lag(df$Long_MA)
    
    df$Signal <- "Hold"
    
    buy_cond  <- !is.na(df$Short_MA) & !is.na(df$Long_MA) &
                 !is.na(prev_short) & !is.na(prev_long) &
                 (prev_short <= prev_long) & (df$Short_MA > df$Long_MA)
    
    sell_cond <- !is.na(df$Short_MA) & !is.na(df$Long_MA) &
                 !is.na(prev_short) & !is.na(prev_long) &
                 (prev_short >= prev_long) & (df$Short_MA < df$Long_MA)
    
    # Optional RSI filter for sophisticated rule refinement
    if (input$use_rsi_filter && !all(is.na(df$RSI))) {
      buy_cond  <- buy_cond & (df$RSI < 70)
      sell_cond <- sell_cond & (df$RSI > 30)
    }
    
    df$Signal[buy_cond]  <- "Buy"
    df$Signal[sell_cond] <- "Sell"
    
    return(df)
  })
  
  # Render main combined visualization
  output$stock_chart <- renderPlot({
    df <- processed_data()
    
    # Base ggplot canvas
    p_main <- ggplot(df, aes(x = Date)) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste0(toupper(input$stock_symbol), " - Technical Analysis (", input$time_frame, ")"),
        subtitle = paste0("Chart Type: ", input$chart_type, " | Short MA: ", input$short_ma_len, " | Long MA: ", input$long_ma_len),
        y = "Price ($)",
        x = ""
      )
    
    # Apply selected chart type
    if (input$chart_type == "Line Graph") {
      p_main <- p_main +
        geom_line(aes(y = Close), color = "#2c3e50", linewidth = 0.9)
      
    } else if (input$chart_type == "Area Chart") {
      p_main <- p_main +
        geom_area(aes(y = Close), fill = "#3498db", alpha = 0.25) +
        geom_line(aes(y = Close), color = "#2980b9", linewidth = 0.9)
      
    } else if (input$chart_type == "Candlestick") {
      df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
      p_main <- ggplot(df, aes(x = Date)) +
        geom_segment(aes(xend = Date, y = Low, yend = High, color = Direction), linewidth = 0.6) +
        geom_rect(aes(
          xmin = Date - 0.35,
          xmax = Date + 0.35,
          ymin = pmin(Open, Close),
          ymax = pmax(Open, Close),
          fill = Direction
        )) +
        scale_color_manual(values = c("Up" = "#27ae60", "Down" = "#e74c3c")) +
        scale_fill_manual(values = c("Up" = "#27ae60", "Down" = "#e74c3c")) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "top") +
        labs(
          title = paste0(toupper(input$stock_symbol), " - Technical Analysis (", input$time_frame, ")"),
          subtitle = paste0("Candlestick Chart | Short MA: ", input$short_ma_len, " | Long MA: ", input$long_ma_len),
          y = "Price ($)",
          x = ""
        )
    }
    
    # Overlay Moving Averages if toggled ON
    if ("Moving Averages" %in% input$technical_indicators) {
      p_main <- p_main +
        geom_line(aes(y = Short_MA, linetype = "Short SMA"), color = "#e67e22", linewidth = 0.9, na.rm = TRUE) +
        geom_line(aes(y = Long_MA, linetype = "Long SMA"), color = "#8e44ad", linewidth = 0.9, na.rm = TRUE) +
        scale_linetype_manual(name = "Indicators", values = c("Short SMA" = "solid", "Long SMA" = "dashed"))
    }
    
    # Annotate Buy and Sell signals on the chart
    if (input$show_annotations) {
      buy_df  <- subset(df, Signal == "Buy")
      sell_df <- subset(df, Signal == "Sell")
      
      if (nrow(buy_df) > 0) {
        p_main <- p_main +
          geom_point(data = buy_df, aes(x = Date, y = Low * 0.985), shape = 24, size = 3.5, fill = "#2ecc71", color = "black") +
          geom_text(data = buy_df, aes(x = Date, y = Low * 0.965, label = "BUY"), color = "#1e8449", fontface = "bold", size = 3.5)
      }
      
      if (nrow(sell_df) > 0) {
        p_main <- p_main +
          geom_point(data = sell_df, aes(x = Date, y = High * 1.015), shape = 25, size = 3.5, fill = "#e74c3c", color = "black") +
          geom_text(data = sell_df, aes(x = Date, y = High * 1.035, label = "SELL"), color = "#922b21", fontface = "bold", size = 3.5)
      }
    }
    
    # Assemble panel list (Main Chart + Optional RSI + Optional MACD)
    plot_list <- list(p_main)
    heights_vec <- c(3)
    
    if ("RSI" %in% input$technical_indicators && !all(is.na(df$RSI))) {
      p_rsi <- ggplot(df, aes(x = Date, y = RSI)) +
        geom_line(color = "#2980b9", linewidth = 0.8, na.rm = TRUE) +
        geom_hline(yintercept = 70, linetype = "dashed", color = "#e74c3c") +
        geom_hline(yintercept = 30, linetype = "dashed", color = "#27ae60") +
        ylim(0, 100) +
        theme_minimal(base_size = 12) +
        labs(y = "RSI (14)", x = "")
      
      plot_list <- append(plot_list, list(p_rsi))
      heights_vec <- c(heights_vec, 1.2)
    }
    
    if ("MACD" %in% input$technical_indicators && !all(is.na(df$MACD))) {
      p_macd <- ggplot(df, aes(x = Date)) +
        geom_line(aes(y = MACD), color = "#16a085", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = MACD_Signal), color = "#d35400", linetype = "dashed", linewidth = 0.8, na.rm = TRUE) +
        geom_hline(yintercept = 0, color = "gray60") +
        theme_minimal(base_size = 12) +
        labs(y = "MACD", x = "Date")
      
      plot_list <- append(plot_list, list(p_macd))
      heights_vec <- c(heights_vec, 1.2)
    }
    
    # Combine stacked charts cleanly using patchwork
    wrap_plots(plot_list, ncol = 1, heights = heights_vec)
  })
  
  # Render table of triggered Buy/Sell events
  output$signal_table <- renderTable({
    df <- processed_data()
    trades <- df %>%
      filter(Signal %in% c("Buy", "Sell")) %>%
      mutate(
        Date = as.character(Date),
        Close = round(Close, 2),
        Short_MA = round(Short_MA, 2),
        Long_MA = round(Long_MA, 2),
        RSI = round(RSI, 2)
      ) %>%
      select(Date, Signal, Close, Short_MA, Long_MA, RSI)
    
    if (nrow(trades) == 0) {
      return(data.frame(Message = "No Buy/Sell crossover signals triggered in this date range."))
    }
    return(trades)
  })
}

# Run the application
shinyApp(ui = ui, server = server)