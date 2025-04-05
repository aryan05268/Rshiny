# Rshiny
# Time Series Analyzer

The **Time Series Analyzer** is an interactive Shiny web application that enables users to upload time series data in CSV format, perform advanced time series analysis, and generate interpretable forecasts using various statistical models.

## Features

- 📂 Upload CSV data and select date & target columns
- 📈 Visualize raw time series
- 🔍 Decompose time series (Additive/Multiplicative)
- 📉 Detrend data
- 🧪 Perform Augmented Dickey-Fuller (ADF) stationarity test
- 📊 Fit and forecast using:
  - Exponential Smoothing (ETS)
  - Auto ARIMA
  - GARCH
- 📌 Periodogram for frequency analysis
- 📤 Forecast plotting and model comparison
- 🧠 Interpretation of forecast results for each model

## How to Use

1. **Upload CSV File**
   - Ensure your file contains at least one date/time column and one numeric target column.
   - Accepts `.csv` files.

2. **Select Columns**
   - Choose the Date column and the Time Series column from dropdowns.

3. **Set Frequency**
   - Input frequency (`12` for monthly, `4` for quarterly, etc.).

4. **Explore & Analyze**
   - Navigate through tabs to:
     - Visualize the series
     - Decompose it
     - Test for stationarity
     - Detrend the series
     - Fit and forecast using ETS, ARIMA, and GARCH
     - Analyze frequency using Periodogram
     - View and compare forecasts
     - Read interpretive summaries for each method

## Requirements

- R (>= 4.0.0)
- R packages:
  - `shiny`
  - `ggplot2`
  - `forecast`
  - `tseries`
  - `rugarch`
  - `TSA`
  - `xts`
  - `lubridate`
  - `plotly`

Install dependencies in R using:

```r
install.packages(c("shiny", "ggplot2", "forecast", "tseries", "rugarch", "TSA", "xts", "lubridate", "plotly"))
