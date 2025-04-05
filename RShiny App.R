library(shiny)
library(shinydashboard)
library(forecast)
library(tseries)
library(ggplot2)
library(rugarch)
library(lubridate)
library(dygraphs)
library(TSA)
library(gridExtra)

ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analyzer"),
  dashboardSidebar(
    fileInput("file", "Upload CSV File", accept = ".csv"),
    uiOutput("date_col_ui"),
    uiOutput("value_col_ui"),
    radioButtons("freq", "Data Frequency:",
                 choices = c(Daily = "day", Weekly = "week", 
                             Monthly = "month", Quarterly = "quarter", 
                             Yearly = "year")),
    numericInput("forecast_periods", "Forecast Periods:", value = 12, min = 1),
    actionButton("analyze", "Run Analysis")
  ),
  dashboardBody(
    tabBox(width = 12,
           tabPanel("Data Overview",
                    dygraphOutput("ts_plot"),
                    verbatimTextOutput("data_summary")),
           tabPanel("Decomposition",
                    plotOutput("decomp_plot"),
                    h4("Interpretation:"),
                    textOutput("decomp_interp")),
           tabPanel("Detrending Analysis",
                    plotOutput("detrend_plot"),
                    h4("Detrended Series:"),
                    plotOutput("detrended_series"),
                    h4("Residual Analysis:"),
                    plotOutput("detrend_resid_acf"),
                    verbatimTextOutput("detrend_resid_test"),
                    h4("Interpretation:"),
                    textOutput("detrend_interp")),
           tabPanel("Periodogram",
                    plotOutput("periodogram_plot"),
                    h4("Dominant Frequencies:"),
                    verbatimTextOutput("periodogram_peaks"),
                    h4("Interpretation:"),
                    textOutput("periodogram_interp")),
           tabPanel("ACF/PACF",
                    plotOutput("acf_plot"),
                    plotOutput("pacf_plot"),
                    h4("Interpretation:"),
                    textOutput("acf_interp")),
           tabPanel("Stationarity",
                    verbatimTextOutput("adf_test"),
                    plotOutput("stationarity_plot"),
                    h4("Interpretation:"),
                    textOutput("stationarity_interp")),
           tabPanel("Exponential Smoothing (ETS)",
                    verbatimTextOutput("ets_model"),
                    h4("Residual Analysis:"),
                    plotOutput("ets_resid_plot"),
                    plotOutput("ets_resid_acf"),
                    verbatimTextOutput("ets_resid_test"),
                    h4("Interpretation:"),
                    textOutput("ets_interp")),
           tabPanel("ARIMA/SARIMA",
                    verbatimTextOutput("arima_model"),
                    h4("Residual Analysis:"),
                    plotOutput("arima_resid_plot"),
                    plotOutput("arima_resid_acf"),
                    verbatimTextOutput("arima_resid_test"),
                    h4("Interpretation:"),
                    textOutput("arima_interp")),
           tabPanel("Model Comparison",
                    verbatimTextOutput("model_compare"),
                    h4("Interpretation:"),
                    textOutput("compare_interp")),
           tabPanel("Forecast",
                    plotOutput("forecast_plot"),
                    h4("Forecast from Best Model:"),
                    verbatimTextOutput("best_forecast"),
                    h4("Interpretation:"),
                    textOutput("forecast_interp")),
           tabPanel("ARCH/GARCH",
                    verbatimTextOutput("garch_model"),
                    h4("Residual Analysis:"),
                    plotOutput("garch_resid_plot"),
                    plotOutput("garch_resid_acf"),
                    verbatimTextOutput("garch_resid_test"),
                    h4("Interpretation:"),
                    textOutput("garch_interp"))
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df
  })
  
  # Automatic column detection
  auto_detect_columns <- reactive({
    req(data())
    df <- data()
    
    # Try to automatically detect date column
    date_col <- NULL
    for (col in names(df)) {
      if (any(class(df[[col]]) %in% c("Date", "POSIXct", "POSIXt"))) {
        date_col <- col
        break
      }
      # Try to parse as date
      if (is.character(df[[col]]) || is.factor(df[[col]])) {
        if (!all(is.na(as.Date(as.character(df[[col]]), optional = TRUE)))) {
          date_col <- col
          break
        }
      }
    }
    
    # Try to automatically detect value column (first numeric column that's not the date)
    value_col <- NULL
    for (col in names(df)) {
      if (col != date_col && is.numeric(df[[col]])) {
        value_col <- col
        break
      }
    }
    
    list(date_col = date_col, value_col = value_col)
  })
  
  # Dynamic UI for column selection with auto-selected defaults
  output$date_col_ui <- renderUI({
    req(data())
    cols <- names(data())
    auto_cols <- auto_detect_columns()
    selectInput("date_col", "Select Date Column", 
                choices = cols, selected = auto_cols$date_col)
  })
  
  output$value_col_ui <- renderUI({
    req(data())
    cols <- names(data())
    auto_cols <- auto_detect_columns()
    selectInput("value_col", "Select Value Column", 
                choices = cols, selected = auto_cols$value_col)
  })
  
  ts_data <- eventReactive(input$analyze, {
    req(data(), input$date_col, input$value_col)
    df <- data()
    
    # Convert date column to proper date format
    date_col <- df[[input$date_col]]
    if(is.numeric(date_col)) {
      # Handle numeric dates (like years)
      dates <- as.Date(paste0(date_col, "-01-01"))
    } else {
      dates <- tryCatch({
        as.Date(date_col)
      }, error = function(e) {
        # Try parsing with lubridate
        parse_date_time(date_col, orders = c("ymd", "dmy", "mdy", "ym", "my"))
      })
    }
    
    values <- df[[input$value_col]]
    
    # Create time series object
    freq <- switch(input$freq,
                   "day" = 7,
                   "week" = 52,
                   "month" = 12,
                   "quarter" = 4,
                   "year" = 1)
    
    start_year <- year(dates[1])
    start_unit <- switch(input$freq,
                         "day" = yday(dates[1]),
                         "week" = week(dates[1]),
                         "month" = month(dates[1]),
                         "quarter" = quarter(dates[1]),
                         "year" = 1)
    
    ts(values, start = c(start_year, start_unit), frequency = freq)
  })
  
  # Detrending functions
  detrended_data <- reactive({
    req(ts_data())
    # Using linear detrending
    time <- 1:length(ts_data())
    lm_model <- lm(ts_data() ~ time)
    resid <- ts(resid(lm_model), start = start(ts_data()), frequency = frequency(ts_data()))
    list(model = lm_model, detrended = resid)
  })
  
  # Periodogram analysis - fixed implementation
  periodogram_data <- reactive({
    req(ts_data())
    p <- spec.pgram(ts_data(), plot = FALSE)
    # Find top 5 frequencies
    df <- data.frame(freq = p$freq, spec = p$spec)
    top_freq <- df[order(-df$spec),][1:5,]
    top_freq$period <- 1/top_freq$freq
    list(periodogram = p, top_freq = top_freq)
  })
  
  output$ts_plot <- renderDygraph({
    req(ts_data())
    dygraph(ts_data(), main = "Time Series Plot") %>%
      dyRangeSelector()
  })
  
  output$data_summary <- renderPrint({
    req(ts_data())
    cat("Time Series Summary:\n")
    summary(ts_data())
  })
  
  output$decomp_plot <- renderPlot({
    req(ts_data())
    decomp <- stl(ts_data(), s.window = "periodic")
    autoplot(decomp) + ggtitle("Time Series Decomposition")
  })
  
  output$decomp_interp <- renderText({
    req(ts_data())
    "This plot shows the decomposition of the time series into trend, seasonal, and remainder components. 
    The trend shows the long-term pattern, seasonality shows repeating patterns, and remainder shows the noise.
    If the seasonal component shows clear patterns, your data has seasonality. If the trend is not flat,
    your data may need differencing to make it stationary."
  })
  
  output$detrend_plot <- renderPlot({
    req(ts_data(), detrended_data())
    time <- 1:length(ts_data())
    df <- data.frame(time = time, value = as.numeric(ts_data()))
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      ggtitle("Original Series with Trend Line") +
      xlab("Time") + ylab("Value")
  })
  
  output$detrended_series <- renderPlot({
    req(detrended_data())
    autoplot(detrended_data()$detrended) +
      ggtitle("Detrended Series (Residuals from Linear Trend)") +
      ylab("Residuals")
  })
  
  output$detrend_resid_acf <- renderPlot({
    req(detrended_data())
    ggAcf(detrended_data()$detrended) + 
      ggtitle("ACF of Detrended Series")
  })
  
  output$detrend_resid_test <- renderPrint({
    req(detrended_data())
    cat("Ljung-Box Test on Detrended Series:\n")
    Box.test(detrended_data()$detrended, type = "Ljung-Box")
  })
  
  output$detrend_interp <- renderText({
    req(detrended_data())
    "The top plot shows the original series with fitted linear trend (red line). 
    The bottom plot shows the detrended series (residuals after removing trend).
    The ACF plot helps assess whether the detrended series shows remaining autocorrelation.
    The Ljung-Box test checks for remaining autocorrelation (p > 0.05 suggests no significant autocorrelation).
    If the detrended series appears stationary with no autocorrelation, linear detrending was successful."
  })
  
  output$periodogram_plot <- renderPlot({
    req(periodogram_data())
    plot(periodogram_data()$periodogram, main = "Periodogram")
  })
  
  output$periodogram_peaks <- renderPrint({
    req(periodogram_data())
    cat("Top 5 Dominant Frequencies:\n")
    cat("Frequency\tPeriod\t\tSpectral Density\n")
    print(periodogram_data()$top_freq)
  })
  
  output$periodogram_interp <- renderText({
    req(periodogram_data())
    "The periodogram shows the power (variance) associated with each frequency component.
    Peaks in the periodogram indicate important frequencies in your data. The 'Period' column
    converts frequency to time units (e.g., a period of 12 for monthly data suggests yearly seasonality).
    The highest peaks typically correspond to the main seasonal patterns in your data."
  })
  
  output$acf_plot <- renderPlot({
    req(ts_data())
    ggAcf(ts_data()) + ggtitle("Autocorrelation Function (ACF)")
  })
  
  output$pacf_plot <- renderPlot({
    req(ts_data())
    ggPacf(ts_data()) + ggtitle("Partial Autocorrelation Function (PACF)")
  })
  
  output$acf_interp <- renderText({
    req(ts_data())
    "ACF shows correlation between observations at different lags. PACF shows the partial correlation after 
    accounting for shorter lags. Slow decay in ACF suggests non-stationarity. Significant spikes at seasonal 
    lags in ACF suggest seasonality. The PACF helps identify the order of AR terms in ARIMA models."
  })
  
  output$adf_test <- renderPrint({
    req(ts_data())
    cat("Augmented Dickey-Fuller Test for Stationarity:\n")
    adf.test(ts_data())
  })
  
  output$stationarity_plot <- renderPlot({
    req(ts_data())
    autoplot(ts_data()) + 
      ggtitle("Time Series Plot for Stationarity Assessment") +
      ylab("Value") + xlab("Time")
  })
  
  output$stationarity_interp <- renderText({
    req(ts_data())
    "The ADF test checks for stationarity (null hypothesis: series is non-stationary). 
    If p-value > 0.05, the series is likely non-stationary and needs differencing. 
    The plot should show constant mean and variance over time for stationarity. 
    If the series has trends or changing variance, transformations may be needed."
  })
  
  ets_model <- reactive({
    req(ts_data())
    ets(ts_data())
  })
  
  output$ets_model <- renderPrint({
    req(ets_model())
    cat("Exponential Smoothing (ETS) Model:\n")
    summary(ets_model())
  })
  
  output$ets_resid_plot <- renderPlot({
    req(ets_model())
    autoplot(residuals(ets_model())) + 
      ggtitle("ETS Model Residuals")
  })
  
  output$ets_resid_acf <- renderPlot({
    req(ets_model())
    ggAcf(residuals(ets_model())) + 
      ggtitle("ACF of ETS Model Residuals")
  })
  
  output$ets_resid_test <- renderPrint({
    req(ets_model())
    cat("Ljung-Box Test on ETS Model Residuals:\n")
    Box.test(residuals(ets_model()), type = "Ljung-Box")
  })
  
  output$ets_interp <- renderText({
    req(ets_model())
    "ETS models decompose the series into Error (additive/multiplicative), Trend (none/additive/damped), 
    and Seasonal (none/additive/multiplicative) components. The model automatically selects the best 
    combination based on information criteria. For example, 'A,N,A' means additive errors, no trend, 
    and additive seasonality. Residuals should be white noise (uncorrelated with constant variance) - 
    check the ACF plot and Ljung-Box test results to verify this."
  })
  
  arima_model <- reactive({
    req(ts_data())
    auto.arima(ts_data(), seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  })
  
  output$arima_model <- renderPrint({
    req(arima_model())
    cat("Best ARIMA/SARIMA Model:\n")
    summary(arima_model())
  })
  
  output$arima_resid_plot <- renderPlot({
    req(arima_model())
    autoplot(residuals(arima_model())) + 
      ggtitle("ARIMA Model Residuals")
  })
  
  output$arima_resid_acf <- renderPlot({
    req(arima_model())
    ggAcf(residuals(arima_model())) + 
      ggtitle("ACF of ARIMA Model Residuals")
  })
  
  output$arima_resid_test <- renderPrint({
    req(arima_model())
    cat("Ljung-Box Test on ARIMA Model Residuals:\n")
    Box.test(residuals(arima_model()), type = "Ljung-Box")
  })
  
  output$arima_interp <- renderText({
    req(arima_model())
    "The ARIMA model is automatically selected based on AIC. The (p,d,q) terms represent AR order, 
    differencing, and MA order. Seasonal terms (P,D,Q) appear if seasonality is detected. 
    Residuals should be white noise (no patterns) - check the residual plots and Ljung-Box test. 
    ACF of residuals should have no significant spikes."
  })
  
  output$model_compare <- renderPrint({
    req(ets_model(), arima_model())
    cat("Model Comparison (AIC, BIC, Accuracy Measures):\n\n")
    cat("ETS Model:\n")
    print(accuracy(ets_model()))
    cat("\nARIMA Model:\n")
    print(accuracy(arima_model()))
    
    cat("\nWhich model is better?\n")
    if(AIC(ets_model()) < AIC(arima_model())) {
      cat("ETS model has lower AIC (better fit)\n")
    } else {
      cat("ARIMA model has lower AIC (better fit)\n")
    }
  })
  
  output$compare_interp <- renderText({
    req(ets_model(), arima_model())
    "Compare models using AIC/BIC (lower is better) and accuracy measures (MAPE, RMSE - lower is better). 
    ETS models are generally better for series with clear trend/seasonality, while ARIMA is more flexible 
    for complex patterns. The best model for forecasting is typically the one with lower AIC and better 
    accuracy measures on the test set."
  })
  
  best_model <- reactive({
    req(ets_model(), arima_model())
    if(AIC(ets_model()) < AIC(arima_model())) {
      return(ets_model())
    } else {
      return(arima_model())
    }
  })
  
  output$forecast_plot <- renderPlot({
    req(best_model(), input$forecast_periods)
    autoplot(forecast(best_model(), h = input$forecast_periods)) +
      ggtitle(paste("Forecast for next", input$forecast_periods, "periods"))
  })
  
  output$best_forecast <- renderPrint({
    req(best_model(), input$forecast_periods)
    forecast(best_model(), h = input$forecast_periods)
  })
  
  output$forecast_interp <- renderText({
    req(best_model())
    paste("Forecast from the", ifelse(AIC(ets_model()) < AIC(arima_model()), "ETS", "ARIMA"), "model (selected based on AIC).",
          "The forecast plot shows future predictions with 80% and 95% confidence intervals. 
    Wider intervals indicate more uncertainty. If seasonality is present, the forecast 
    should capture seasonal patterns. Check if the trend continues reasonably.")
  })
  
  garch_model <- reactive({
    req(ts_data())
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                       distribution.model = "norm")
    
    ugarchfit(spec = spec, data = ts_data())
  })
  
  output$garch_model <- renderPrint({
    req(garch_model())
    cat("GARCH Model Summary:\n")
    garch_model()
  })
  
  output$garch_resid_plot <- renderPlot({
    req(garch_model())
    plot(garch_model(), which = 8)
  })
  
  output$garch_resid_acf <- renderPlot({
    req(garch_model())
    plot(garch_model(), which = 10)
  })
  
  output$garch_resid_test <- renderPrint({
    req(garch_model())
    cat("Ljung-Box Test on Standardized Residuals:\n")
    Box.test(residuals(garch_model(), standardize = TRUE), type = "Ljung-Box")
  })
  
  output$garch_interp <- renderText({
    req(garch_model())
    "GARCH models volatility clustering (when high volatility follows high volatility). 
    The model has ARCH (α) and GARCH (β) terms. Significant coefficients indicate 
    volatility clustering. Standardized residuals should be i.i.d. - check the ACF plot 
    and Ljung-Box test results. The model is useful when residuals show changing variance (heteroskedasticity)."
  })
}

shinyApp(ui, server)