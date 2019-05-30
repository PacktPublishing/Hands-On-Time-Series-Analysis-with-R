# Chapter 11 Code

# -------- Code Chank 1 --------
set.seed(12345)

stationary_ts <- arima.sim(model = list(order = c(1,0,0),
                                        ar = 0.5 ),
                           n = 500)
# -------- Code Chank 2 --------
library(TSstudio)

ts_plot(stationary_ts, 
        title = "Stationary Time Series",
        Ytitle = "Value",
        Xtitle = "Index")
# -------- Code Chank 3 --------
set.seed(12345)

non_stationary_ts <- arima.sim(model = list(order = c(1,1,0),ar = 0.3 ),n = 500)

ts_plot(non_stationary_ts, 
        title = "Non-Stationary Time Series",
        Ytitle = "Value",
        Xtitle = "Index")
# -------- Code Chank 4 --------
data(AirPassengers)

ts_plot(AirPassengers,
        title = "Monthly Airline Passenger Numbers 1949-1960",
        Ytitle = "Thousands of Passengers",
        Xtitle = "Year")
# -------- Code Chank 5 --------
ts_plot(diff(AirPassengers, lag = 1),
        title = "AirPassengers Series - First Differencing",
        Xtitle = "Year",
        Ytitle = "Differencing of Thousands of Passengers")
# -------- Code Chank 6 --------
ts_plot(diff(diff(AirPassengers, lag = 1), 12),
        title = "AirPassengers Series - First and Seasonal Differencing",
        Xtitle = "Year",
        Ytitle = "Differencing of Thousands of Passengers")
# -------- Code Chank 7 --------
ts_plot(diff(log(AirPassengers), lag = 1),
        title = "AirPassengers Series - First Differencing with Log Transformation",
        Xtitle = "Year",
        Ytitle = "Differencing/Log of Thousands of Passengers")
# -------- Code Chank 8 --------
set.seed(12345)

library(plotly)

p1 <- plot_ly()
p2 <- plot_ly()

for(i in 1:20){
  rm <- NULL
  rw <- arima.sim(model = list(order = c(0, 1, 0)), n = 500)
  p1 <- p1 %>% add_lines(x = time(rw), y = as.numeric(rw))
  p2 <- p2 %>% add_lines(x = time(diff(rw)), y = as.numeric(diff(rw)))
}
# -------- Code Chank 9 --------
p1 %>% layout(title = "Simulate Random Walk",
              yaxis = list(title = "Value"),
              xaxis = list(title = "Index")) %>%
  hide_legend()
# -------- Code Chank 10 --------
p2 %>% layout(title = "Simulate Random Walk with First-Order Differencing",
              yaxis = list(title = "Value"),
              xaxis = list(title = "Index")) %>%
  hide_legend()
# -------- Code Chank 11 --------
set.seed(12345)

ar2 <- arima.sim(model = list(order = c(2,0,0),
                              ar = c(0.9, -0.3)),
                 n = 500) 
# -------- Code Chank 12 --------
ts_plot(ar2, 
        title = "Simulate AR(2) Series",
        Ytitle = "Value",
        Xtitle = "Index")
# -------- Code Chank 13 --------
md_ar <- ar(ar2)
md_ar
# -------- Code Chank 14 --------
par(mfrow=c(1,2))
acf(ar2)
pacf(ar2)
# -------- Code Chank 15 --------
set.seed(12345)

ma2 <- arima.sim(model = list(order = c(0, 0, 2),
                              ma = c(0.5, -0.3)),
                 n = 500) 
# -------- Code Chank 16 --------
ts_plot(ma2, 
        title = "Simulate MA(2) Series",
        Ytitle = "Value",
        Xtitle = "Index")
# -------- Code Chank 17 --------
md_ma <- arima(ma2, order = c(0,0,2), method = "ML")
md_ma
# -------- Code Chank 18 --------
par(mfrow=c(1,2))
acf(ma2)
pacf(ma2)
# -------- Code Chank 19 --------
set.seed(12345)

arma <- arima.sim(model = list(order(1,0,2), ar = c(0.7), ma = c(0.5,-0.3)), n = 500)
# -------- Code Chank 20 --------
ts_plot(arma, 
        title = "Simulate ARMA(1,2) Series",
        Ytitle = "Value",
        Xtitle = "Index")
# -------- Code Chank 21 --------
arma_md <- arima(arma, order = c(1,0,2))
arma_md
# -------- Code Chank 22 --------
par(mfrow=c(1,2))
acf(arma)
pacf(arma)
# -------- Code Chank 23 --------
arima(arma, order = c(1, 0, 1), include.mean = FALSE)
# -------- Code Chank 24 --------
arima(arma, order = c(2, 0, 1), include.mean = FALSE)
# -------- Code Chank 25 --------
arima(arma, order = c(1, 0, 2), include.mean = FALSE)
# -------- Code Chank 26 --------
arima(arma, order = c(2, 0, 2), include.mean = FALSE)
# -------- Code Chank 27 --------
library(forecast)
best_arma <- arima(arma, order = c(1, 0, 2), include.mean = FALSE)

checkresiduals(best_arma)
# -------- Code Chank 28 --------
ar_fc <- forecast(md_ar, h = 100)
# -------- Code Chank 29 --------
plot_forecast(ar_fc, 
              title = "Forecast AR(2) Model",
              Ytitle = "Value",
              Xtitle = "Year")
# -------- Code Chank 30 --------
data(Coffee_Prices)

robusta_price <- window(Coffee_Prices[,1], start = c(2000, 1))
# -------- Code Chank 31 --------
ts_plot(robusta_price,
        title = "The Robusta Coffee Monthly Prices",
        Ytitle = "Price in USD",
        Xtitle = "Year") 
# -------- Code Chank 32 --------
acf(robusta_price)
# -------- Code Chank 33 --------
robusta_price_d1 <- diff(robusta_price)
# -------- Code Chank 34 --------
par(mfrow=c(1,2))
acf(robusta_price_d1)
pacf(robusta_price_d1)
# -------- Code Chank 35 --------
robusta_md <- arima(robusta_price, order = c(1, 1, 0))
# -------- Code Chank 36 --------
summary(robusta_md)
# -------- Code Chank 37 --------
checkresiduals(robusta_md)
# -------- Code Chank 38 --------
data(USgas)
# -------- Code Chank 39 --------
ts_plot(USgas,
        title = "US Monthly Natural Gas consumption", 
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year")
# -------- Code Chank 40 --------
USgas_split <- ts_split(USgas, sample.out = 12)

train <- USgas_split$train
test <- USgas_split$test
# -------- Code Chank 41 --------
par(mfrow=c(1,2))
acf(train, lag.max = 60)
pacf(train, lag.max = 60)
# -------- Code Chank 42 --------
USgas_d12 <- diff(train, 12)

ts_plot(USgas_d12,
        title = "US Monthly Natural Gas consumption - First Seasonal Difference", 
        Ytitle = "Billion Cubic Feet (First Difference)",
        Xtitle = "Year")
# -------- Code Chank 43 --------
USgas_d12_1 <- diff(diff(USgas_d12, 1))

ts_plot(USgas_d12_1,
        title = "US Monthly Natural Gas consumption - First Seasonal and Non-Seasonal Differencing", 
        Ytitle = "Billion Cubic Feet (Difference)",
        Xtitle = "Year")
# -------- Code Chank 44 --------
par(mfrow=c(1,2))
acf(USgas_d12_1, lag.max = 60)
pacf(USgas_d12_1, lag.max = 60)
# -------- Code Chank 45 --------
p <- q <- P <- Q <- 0:2
# -------- Code Chank 46 --------
arima_grid <- expand.grid(p,q,P,Q)
names(arima_grid) <- c("p", "q", "P", "Q")
arima_grid$d <- 1
arima_grid$D <- 1
# -------- Code Chank 47 --------
arima_grid$k <- rowSums(arima_grid)
# -------- Code Chank 48 --------
library(dplyr)

arima_grid <- arima_grid %>% filter(k <= 7)
# -------- Code Chank 49 --------
arima_search <- lapply(1:nrow(arima_grid), function(i){
  md <- NULL
  md <- arima(train, order = c(arima_grid$p[i], 1, arima_grid$q[i]), seasonal = list(order = c(arima_grid$P[i], 1, arima_grid$Q[i])))
  
  results <- data.frame(p = arima_grid$p[i], d = 1, q = arima_grid$q[i],
                        P = arima_grid$P[i], D = 1, Q = arima_grid$Q[i],
                        AIC = md$aic)
}) %>% bind_rows() %>% arrange(AIC)

# -------- Code Chank 50 --------
head(arima_search)
# -------- Code Chank 51 --------
USgas_best_md <- arima(train, order = c(1,1,1), seasonal = list(order = c(2,1,1)))
USgas_best_md
# -------- Code Chank 52 --------
USgas_test_fc <- forecast(USgas_best_md, h = 12)
# -------- Code Chank 53 --------
accuracy(USgas_test_fc, test)
# -------- Code Chank 54 --------
test_forecast(USgas, 
              forecast.obj = USgas_test_fc,
              test = test)
# -------- Code Chank 55 --------
final_md <- arima(USgas, order = c(1,1,1), seasonal = list(order = c(2,1,1)))
# -------- Code Chank 56 --------
checkresiduals(final_md)
# -------- Code Chank 57 --------
USgas_fc <- forecast(final_md, h = 12)
# -------- Code Chank 58 --------
plot_forecast(USgas_fc,
              title = "US Natural Gas Consumption - Forecast",
              Ytitle = "Billion Cubic Feet",
              Xtitle = "Year")
# -------- Code Chank 59 --------
USgas_auto_md1 <- auto.arima(train)
USgas_auto_md1
# -------- Code Chank 60 --------
USgas_auto_md2 <- auto.arima(train, 
                             max.order = 5,
                             D = 1,
                             d = 1,
                             ic = "aic",
                             stepwise = FALSE, 
                             approximation = FALSE)
USgas_auto_md2

# -------- Code Chank 61 --------
df <- ts_to_prophet(AirPassengers) %>% setNames(c("date", "y"))

df$lag12 <- dplyr::lag(df$y, n = 12)

library(lubridate)

df$month <- factor(month(df$date, label = TRUE), ordered = FALSE)

df$trend <- 1:nrow(df)
# -------- Code Chank 62 --------
par <- ts_split(ts.obj = AirPassengers, sample.out = 12)

train <- par$train

test <- par$test
# -------- Code Chank 63 --------
train_df <- df[1:(nrow(df) - 12), ]
test_df <- df[(nrow(df) - 12 + 1):nrow(df), ]
# -------- Code Chank 64 --------
md1 <- tslm(train ~ season + trend + lag12, data = train_df)
# -------- Code Chank 65 --------
checkresiduals(md1)
# -------- Code Chank 66 --------
md2 <- auto.arima(train, 
                  xreg = cbind(model.matrix(~ month,train_df)[,-1], 
                               train_df$trend, 
                               train_df$lag12), 
                  seasonal = TRUE, 
                  stepwise = FALSE, 
                  approximation = FALSE)
# -------- Code Chank 67 --------
summary(md2)
# -------- Code Chank 68 --------
checkresiduals(md2)
# -------- Code Chank 69 --------
fc1 <- forecast(md1, newdata = test_df)

fc2 <- forecast(md2, xreg = cbind(model.matrix(~ month,test_df)[,-1], 
                                  test_df$trend, 
                                  test_df$lag12))
# -------- Code Chank 70 --------
accuracy(fc1, test)
accuracy(fc2, test)
