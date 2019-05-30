# Chapter 7 Code

# -------- Code Chank 1 --------
library(TSstudio)

data("USgas")
ts_plot(USgas)

data("EURO_Brent")
ts_plot(EURO_Brent)

data("USVSales")
ts_plot(USVSales)
# -------- Code Chank 2 --------
acf(USgas, lag.max = 60)
# -------- Code Chank 3 --------
acf(EURO_Brent, lag.max = 60)
# -------- Code Chank 4 --------
acf(USVSales, lag.max = 60)
# -------- Code Chank 5 --------
pacf(USgas, lag.max = 60)
# -------- Code Chank 6 --------
ts_lags(USgas)
# -------- Code Chank 7 --------
ts_lags(USgas, lags = c(12, 24, 36, 48))
# -------- Code Chank 8 --------
ts_lags(EURO_Brent)
# -------- Code Chank 9 --------
ts_lags(USVSales)
# -------- Code Chank 10 --------
us_vsales <- window(USVSales, start = c(1976,1), end = c(2018,6))
us_unrate <- window(USUnRate, start = c(1976,1), end = c(2018,6))
# -------- Code Chank 11 --------
library(plotly)
plot_ly(x = time(us_vsales), 
        y = us_vsales, 
        type = "scatter", 
        mode = "line", 
        name = "Total Vehicle Sales") %>%
  add_lines(x = time(us_unrate), 
            y = us_unrate,
            name = "Unemployment Rate", 
            yaxis = "y2") %>%
  layout(
    title = "Total Monthly Vehicle Sales vs Unemployment Rate in the US", 
    yaxis2 =  list(
      overlaying = "y",
      side = "right",
      title = "Percentage",
      showgrid = FALSE
    ),
    yaxis = list(title = "Thousands of Units",
                 showgrid = FALSE),
    legend = list(orientation = 'h'),
    margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2)
  )
# -------- Code Chank 12 --------
ccf(x = us_vsales, y = us_unrate, lag.max = 36)
# -------- Code Chank 13 --------
ccf_plot(x = USVSales, y = USUnRate, lags = 0:12)