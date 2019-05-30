# Chapter 5 Code

# -------- Code Chank 1 --------
library(TSstudio)
data(USVSales)

ts_info(USVSales)

ts_plot(USVSales,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of Units",
        Xtitle = "Years",
        Xgrid = TRUE,
        Ygrid = TRUE)


# The lags function return the series with its l lags
lags <- function(ts.obj, l){
  ts_merged <- NULL
  # Creating n lags
  for(i in 1:l){
    ts_merged <- ts.union(ts_merged, stats::lag(ts.obj, k = -i))
  }
  # Merge the lags with the original series
  ts_merged <- ts.union(ts.obj, ts_merged)
  # Set the columns names
  colnames(ts_merged) <- c("y", paste0("y_", 1:i))
  # Removing missing values as results of creating the lags
  ts_merged <- window(ts_merged,
                      start = start(ts.obj) + l,
                      end = end(ts.obj))
  return(ts_merged)
}

head(lags(USVSales, l = 3))

ts_info(lags(USVSales, l = 3))

ts_mean <- function(mts.obj){
  ts_avg <- ts_sum(mts.obj) / dim(mts.obj)[2] # Simple average calculation
  return(ts_avg)
}

sma <- function(ts.obj, order){
  l <- order -1
  l <- lags(ts.obj = ts.obj, l = l)
  m <- ts_mean(l)
  u <- ts.union(ts.obj, m)
  colnames(u) <- c("original", "transformed")
  return(u)
}
sma_4 <- sma(USVSales, order = 4)
ts_plot(sma_4, type = "multiple",
        title = "US Vehicle Sales - SMA (Order 4)",
        Ytitle = "Thousands of Units",
        Ygrid = TRUE,
        Xgrid = TRUE,
        Xtitle = "Year")

sma_12 <- sma(USVSales, order = 12)
ts_plot(sma_12, type = "multiple",
        title = "US Vehicle Sales - SMA (Order 12)",
        Ytitle = "Thousands of Units",
        Ygrid = TRUE,
        Xgrid = TRUE,
        Xtitle = "Year")
two_sided_ma <- ts_ma(ts.obj = USVSales,
                      n = c(2,5),# Setting an order 5 and 11 moving average
                      n_left = 6, n_right = 5, # Setting an order 12 moving average
                      plot = TRUE,
                      multiple = TRUE,
                      margin = 0.04)
# Creating one-sided and two-sided moving average with an order of 12
one_sided_12 <- ts_ma(USVSales, n = NULL, n_left = 11, plot = FALSE)
two_sided_12 <- ts_ma(USVSales, n = NULL, n_left = 6, n_right = 5,plot =
                        FALSE)
one_sided <- one_sided_12$unbalanced_ma_12
two_sided <- two_sided_12$unbalanced_ma_12

ma <- cbind(USVSales, one_sided, two_sided)
p <- ts_plot(ma,
             Xgrid = TRUE,
             Ygrid = TRUE,
             type = "single",
             title = "One-Sided vs. Two-Sided Moving Average - Order 12")
library(plotly)
p <- p %>% layout(legend = list(x = 0.05, y = 0.95),
                  yaxis = list(title = "Thousands of Units"),
                  xaxis = list(title = "Year"))
data(USUnRate)
ts_info(USUnRate)
unemployment <- window(USUnRate, start = c(1990,1))
ts_plot(unemployment,
        title = "US Monthly Unemployment Rate",
        Ytitle = "Unemployment Rate (%)",
        Xtitle = "Year",
        Xgrid = TRUE,
        Ygrid = TRUE)
set.seed(1234)
ts_non_trend <- ts(runif(200, 5,5.2),
                   start = c(2000,1),
                   frequency = 12)
ts_linear_trend_p <- ts_non_trend + 1:length(ts_non_trend) / (0.5 *
                                                                length(ts_non_trend))
ts_linear_trend_n <- ts_non_trend - 1:length(ts_non_trend) / (0.5 *
                                                                length(ts_non_trend))
ts_exp_trend <- ts_non_trend + exp((1:length(ts_non_trend) -1 ) / (0.5 *
                                                                     length(ts_non_trend))) - 1
library(xts)
merged_series <- merge(Baseline_No_Trend = as.xts(ts_non_trend),
                       Positive_Linear_Trend = as.xts(ts_linear_trend_p),
                       Negative_Linear_Trend = as.xts(ts_linear_trend_n),
                       Exponential_Trend = as.xts(ts_exp_trend))
ts_plot(merged_series,
        type = "single",
        Xgrid = TRUE,
        Ygrid = TRUE,
        title = "Different Types of Trends",
        Ytitle = "The Values of the Series",
        Xtitle = "Year") %>%
  layout(legend = list(x = 0.1, y = 0.9))
seasonal_pattern <- sin(2*pi * (1:length(ts_non_trend)) /
                          frequency(ts_non_trend))
ts_seasonal <- ts_non_trend + seasonal_pattern
ts_plot(ts_seasonal,
        title = "Seasonal Pattern without Trend",
        Xgrid = TRUE,
        Ygrid = TRUE,
        Ytitle = "The Values of the Series",
        Xtitle = "Year")
seasonal_with_Ptrend <- ts_linear_trend_p + seasonal_pattern
seasonal_with_Ntrend <- ts_linear_trend_n - seasonal_pattern
seasonal_with_Etrend <- ts_exp_trend + seasonal_pattern
merged_series_seasonal <- merge(Positive_Linear_Trend =
                                  as.xts(seasonal_with_Ptrend),
                                Negative_Linear_Trend =
                                  as.xts(seasonal_with_Ntrend),
                                Exponential_Trend =
                                  as.xts(seasonal_with_Etrend))
ts_plot(merged_series_seasonal,
        type = "single",
        Xgrid = TRUE,
        Ygrid = TRUE,
        title = "Seasonal Pattern with Trend",
        Ytitle = "The Values of the Series",
        Xtitle = "Year") %>%
  layout(legend = list(x = 0.1, y = 0.9))
ts_heatmap(USgas,
           title = "Heatmap - the US Natural Gas Consumption")
ts_heatmap(USUnRate,
           title = "Heatmap - The US Unemployment Rate")
set.seed(1234)
white_noise <- ts(rnorm(12*10, mean = 0, sd = 1),
                  start = c(2008, 1),
                  frequency = 12)
ts_plot(white_noise, title = "White Noise ~ N(0, 1)",
        line.mode = "lines+markers",
        Xgrid = TRUE,
        Ygrid = TRUE,
        Ytitle = "The Values of the Series")
library(dplyr)
x <- lapply(1:24, function(i){
  p <- Box.test(white_noise, lag = i, type = "Ljung-Box")
  output <- data.frame(lag = i, p_value = p$p.value)
  return(output) }) %>% bind_rows
plot(x = x$lag,
     y = x$p_value, ylim = c(0,1),
     main = "Series white_noise - Ljung-Box Test",
     xlab = "Lag", ylab = "P-Value")
abline(h = 0.05, col="red", lwd=3, lty=2)

ts_plot(USgas,
        title = "US Monthly Natural Gas consumption",
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year",
        Xgrid = TRUE,
        Ygrid = TRUE)
data(AirPassengers)

ts_plot(AirPassengers,
        title = "Monthly Airline Passenger Numbers 1949-1960",
        Ytitle = "Thousands of Passengers",
        Xtitle = "Years",
        Xgrid = TRUE,
        Ygrid = TRUE)
library(forecast)
AirPassenger_lamda <- BoxCox.lambda(AirPassengers)
AirPassenger_lamda
AirPassenger_transform <- BoxCox(AirPassengers, lambda =
                                   AirPassenger_lamda)
ts_plot(AirPassenger_transform,
        title = "Monthly Airline Passenger Numbers 1949-1960 with Box-Cox
Transformation",
        Ytitle = "Number of Passengers - Scaled",
        Xtitle = "Years",
        Xgrid = TRUE,
        Ygrid = TRUE)
data(USVSales)
usv_decompose <- decompose(USVSales)
str(usv_decompose)
class(usv_decompose)

plot(usv_decompose)

air_decompose <- decompose(AirPassengers, type = "multiplicative")
plot(air_decompose)

