# Chapter 8 Code

# -------- Code Chank 1 --------
library(TSstudio)

data(USgas)

ts_info(USgas)

# Using the series time index to set the start and end point of each partiton
train <- window(USgas, 
                start = time(USgas)[1], 
                end = time(USgas)[length(USgas) - 12])

test <- window(USgas, 
               start = time(USgas)[length(USgas) - 12 + 1], 
               end = time(USgas)[length(USgas)])

ts_info(train)
ts_info(test)
# -------- Code Chank 2 --------
USgas_partitions <- ts_split(USgas, sample.out = 12)

train <- USgas_partitions$train
test <- USgas_partitions$test

ts_info(train)
ts_info(test)
# -------- Code Chank 3 --------
library(forecast)

md <- auto.arima(train)
# -------- Code Chank 4 --------
checkresiduals(md)
# -------- Code Chank 5 --------
fc <- forecast(md, h = 12)
# -------- Code Chank 6 --------
accuracy(fc, test)
# -------- Code Chank 7 --------
test_forecast(actual = USgas,
              forecast.obj = fc,
              test = test) 
# -------- Code Chank 8 --------
library(forecast)

naive_model <- naive(train, h  = 12)
test_forecast(actual = USgas,
              forecast.obj = naive_model,
              test = test)

accuracy(naive_model, test)
# -------- Code Chank 9 --------
snaive_model <- snaive(train, h = 12)
test_forecast(actual = USgas,
              forecast.obj = snaive_model,
              test = test)

accuracy(snaive_model, test)
# -------- Code Chank 10 --------
md_final <- auto.arima(USgas)

fc_final <- forecast(md_final, h = 12)
# -------- Code Chank 11 --------
plot_forecast(fc_final,
              title = "The US Natural Gas Consumption Forecast",
              Xtitle = "Year",
              Ytitle = "Billion Cubic Feet")
# -------- Code Chank 12 --------
fc_final2 <- forecast(md_final, 
                      h = 60, 
                      level = c(80, 90))

plot_forecast(fc_final2,
              title = "The US Natural Gas Consumption Forecast",
              Xtitle = "Year",
              Ytitle = "Billion Cubic Feet")
# -------- Code Chank 13 --------
fc_final3 <- forecast_sim(model = md_final,
                          h = 60, 
                          n = 500) 
# -------- Code Chank 14 --------
library(plotly)

fc_final3$plot %>% 
  layout(title = "US Natural Gas Consumption - Forecasting Simulation",
         yaxis = list(title = "Billion Cubic Feet"),
         xaxis = list(title = "Year"))
# -------- Code Chank 15 --------
set.seed(1234)
# -------- Code Chank 16 --------
USgas_forecast <- ts_backtesting(ts.obj = USgas,
                                 periods = 6,
                                 models = "abehntw",
                                 error = "MAPE",
                                 window_size = 12,
                                 h = 60,
                                 plot = FALSE)
# -------- Code Chank 17 --------
USgas_forecast$summary_plot

