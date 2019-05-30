# Chapter 10 Code

# -------- Code Chank 1 --------
library(TSstudio)

data("Coffee_Prices")

ts_info(Coffee_Prices)
# -------- Code Chank 2 --------
robusta <- Coffee_Prices[,1]
# -------- Code Chank 3 --------
library(plotly)

ts_plot(robusta) %>% layout(title = "The Robusta Coffee Monthly Prices",
                            yaxis = list(title = "USD per Kg."))
# -------- Code Chank 4 --------
library(tidyr)

sma_forecast <- function(df, h, m, w = NULL){
  
  # Error handling
  if(h > nrow(df)){
    stop("The length of the forecast horizon must be shorter than the length of the series")}
  
  if(m > nrow(df)){
    stop("The length of the rolling window must be shorter than the length of the series")}
  if(!is.null(w)){
    if(length(w) != m){
      stop("The weight argument is not aligned with the length of the rolling window")
    } else if(sum(w) !=1){
      stop("The sum of the average weight is different than 1")
    }
  }
  
  # Setting the average weigths
  if(is.null(w)){
    w <- rep(1/m, m)
  }
  
  # Setting the data frame 
  #-----------------------
  # Changing the Date object column name
  names(df)[1] <- "date" 
  # Setting the training and testing partition 
  # according to the forecast horizon
  df$type <- c(rep("train", nrow(df) - h), 
               rep("test", h)) 
  
  # Spreading the table by the partition type
  df1 <- df %>% spread(key = type, value = y)
  
  # Create the target variable
  df1$yhat <- df1$train
  
  
  # Simple moving average function
  for(i in (nrow(df1) - h + 1):nrow(df1)){
    r <- (i-m):(i-1) 
    df1$yhat[i] <- sum(df1$yhat[r] * w) 
  } 
  
  # dropping from the yhat variable the actual values
  # that were used for the rolling window
  df1$yhat <- ifelse(is.na(df1$test), NA, df1$yhat)
  
  df1$y <- ifelse(is.na(df1$test), df1$train, df1$test)
  
  return(df1)
}
# -------- Code Chank 5 --------
robusta_df <- ts_to_prophet(robusta)

robusta_fc_m1 <-  sma_forecast(robusta_df, h = 24, m = 1)
robusta_fc_m6 <-  sma_forecast(robusta_df, h = 24, m = 6)
robusta_fc_m12 <- sma_forecast(robusta_df, h = 24, m = 12)
robusta_fc_m24 <- sma_forecast(robusta_df, h = 24, m = 24)
robusta_fc_m36 <- sma_forecast(robusta_df, h = 24, m = 36)
# -------- Code Chank 6 --------
library(plotly)

plot_ly(data = robusta_df[650:nrow(robusta_df),], x = ~ ds, y = ~ y,
        type = "scatter", mode = "lines", 
        name = "Actual") %>%
  add_lines(x = robusta_fc_m1$date, y = robusta_fc_m1$yhat, 
            name = "SMA - 1", line = list(dash = "dash")) %>%
  add_lines(x = robusta_fc_m6$date, y = robusta_fc_m6$yhat, 
            name = "SMA - 6", line = list(dash = "dash")) %>%
  add_lines(x = robusta_fc_m12$date, y = robusta_fc_m12$yhat, 
            name = "SMA - 12", line = list(dash = "dash")) %>%
  add_lines(x = robusta_fc_m24$date, y = robusta_fc_m24$yhat, 
            name = "SMA - 24", line = list(dash = "dash")) %>%
  add_lines(x = robusta_fc_m36$date, y = robusta_fc_m36$yhat, 
            name = "SMA - 36", line = list(dash = "dash")) %>%
  layout(title = "Forecasting the Robusta Coffee Monthly Prices",
         xaxis = list(title = ""),
         yaxis = list(title = "USD per Kg."))
# -------- Code Chank 7 --------
data(USgas)

USgas_df <- ts_to_prophet(USgas)
# -------- Code Chank 8 --------
USgas_fc_m12a <- sma_forecast(USgas_df, h = 24, m = 12, w = c(1, rep(0,11)))
USgas_fc_m12b <- sma_forecast(USgas_df, h = 24, m = 12, w = c(0.8, rep(0,10), 0.2))
# -------- Code Chank 9 --------
plot_ly(data = USgas_df[190:nrow(USgas_df),], x = ~ ds, y = ~ y,
        type = "scatter", mode = "lines", 
        name = "Actual") %>%
  add_lines(x = USgas_fc_m12a$date, y = USgas_fc_m12a$yhat, 
            name = "WMA - Seasonal Lag", line = list(dash = "dash")) %>%
  add_lines(x = USgas_fc_m12b$date, y = USgas_fc_m12b$yhat, 
            name = "WMA - 12 (0.2/0.8)", line = list(dash = "dash")) %>%
  layout(title = "Forecasting the Monthly Consumption of Natural Gas in the US",
         xaxis = list(title = ""),
         yaxis = list(title = "Billion Cubic Feet"))
# -------- Code Chank 10 --------
alpha_df <- data.frame(index = seq(from = 1, to = 15, by = 1),
                       power = seq(from = 14, to = 0, by = -1))

alpha_df$alpha_0.01 <- 0.01 * (1 - 0.01) ^ alpha_df$power
alpha_df$alpha_0.2 <- 0.2 * (1 - 0.2) ^ alpha_df$power
alpha_df$alpha_0.4 <- 0.4 * (1 - 0.4) ^ alpha_df$power
alpha_df$alpha_0.6 <- 0.6 * (1 - 0.6) ^ alpha_df$power
alpha_df$alpha_0.8 <- 0.8 * (1 - 0.8) ^ alpha_df$power
alpha_df$alpha_1 <- 1 * (1 - 1) ^ alpha_df$power

plot_ly(data = alpha_df) %>%
  add_lines(x = ~ index, y = ~ alpha_0.01, name = "alpha = 0.01") %>%
  add_lines(x = ~ index, y = ~ alpha_0.2, name = "alpha = 0.2") %>%
  add_lines(x = ~ index, y = ~ alpha_0.4, name = "alpha = 0.4") %>%
  add_lines(x = ~ index, y = ~ alpha_0.6, name = "alpha = 0.6") %>%
  add_lines(x = ~ index, y = ~ alpha_0.8, name = "alpha = 0.8") %>%
  add_lines(x = ~ index, y = ~ alpha_1, name = "alpha = 1") %>%
  layout(title = "Decay Rate of the SES Weights",
         xaxis = list(title = "Index"),
         yaxis = list(title = "Weight"))
# -------- Code Chank 11 --------
robusta_par <- ts_split(robusta, sample.out = 12)
train <- robusta_par$train
test <- robusta_par$test

library(forecast)
fc_ses <- ses(train, h = 12, initial = "optimal")

fc_ses$model
# -------- Code Chank 12 --------
test_forecast(actual = robusta, forecast.obj = fc_ses, test = test) %>%
  layout(title = "Robusta Coffee Prices Forecast vs. Actual",
         xaxis = list(range = c(2010, max(time(robusta)))),
         yaxis = list(range = c(1, 3)))
# -------- Code Chank 13 --------
plot_forecast(fc_ses) %>%
  add_lines(x = time(test), y = as.numeric(test), name = "Testing Partition") %>%
  layout(title = "Robusta Coffee Prices Forecast vs. Actual",
         xaxis = list(range = c(2010, max(time(robusta)))),
         yaxis = list(range = c(0, 4)))
# -------- Code Chank 14 --------
robusta_par1 <- ts_split(robusta, sample.out = 24)

train1 <- robusta_par1$train 
test1 <- ts_split(robusta_par1$test, sample.out = 12)$train

robusta_par2 <- ts_split(robusta, sample.out = 12)

train2 <- robusta_par2$train
valid <- robusta_par2$test
# -------- Code Chank 15 --------
alpha <- seq(from = 0, to = 1, by = 0.01)
# -------- Code Chank 16 --------
alpha[1] <- 0.001
# -------- Code Chank 17 --------
library(dplyr)

ses_grid <- lapply(alpha, function(i){
  md1 <- md_accuracy1 <- md2 <- md_accuracy2 <- results <-  NULL
  md1 <- ses(train1, h = 12, alpha = i, initial = "simple")
  md_accuracy1 <- accuracy(md1, test1)
  
  md2 <- ses(train2, h = 12, alpha = i, initial = "simple")
  md_accuracy2 <- accuracy(md2, valid)
  
  resutls <- data.frame(alpha = i, 
                        train = md_accuracy1[9], 
                        test = md_accuracy1[10], 
                        valid = md_accuracy2[10])
  
}) %>% bind_rows()

# -------- Code Chank 18 --------
plot_ly(data = ses_grid, x = ~ alpha, y = ~ train, 
        line = list(color = 'rgb(205, 12, 24)'),
        type = "scatter", 
        mode = "lines", 
        name = "Training") %>%
  add_lines(x = ~ alpha, y = ~ test, line = list(color = "rgb(22, 96, 167)", dash = "dash"), name=  "Testing") %>%
  add_lines(x = ~ alpha, y = ~ valid, line = list(color = "green", dash = "dot"), name = "Validation") %>%
  layout(title = "SES Model Grid Search Results",
         yaxis = list(title = "MAPE (%)"))
# -------- Code Chank 19 --------
library(Quandl)

gdp <- Quandl("FRED/GDP", start_date = "2010-01-01", type = "ts")

gdp %>% ts_info()
# -------- Code Chank 20 --------
ts_plot(gdp, 
        title = "Gross Domestic Product",
        Ytitle = "Billions of Dollars",
        Xtitle = "Source: U.S. Bureau of Economic Analysis / fred.stlouisfed.org")
# -------- Code Chank 21 --------
gdp_par <- ts_split(gdp, sample.out = 8)

train <- gdp_par$train
test <- gdp_par$test
# -------- Code Chank 22 --------
fc_holt <- holt(train,  h = 8, initial = "optimal") 

fc_holt$model
# -------- Code Chank 23 --------
accuracy(fc_holt, test)
# -------- Code Chank 24 --------
test_forecast(gdp, forecast.obj = fc_holt, test = test)
# -------- Code Chank 25 --------
fc_holt_exp <- holt(train,  h = 8, beta = 0.75  ,initial = "optimal", exponential = TRUE) 

fc_holt_exp$model
# -------- Code Chank 26 --------
accuracy(fc_holt_exp, test)
# -------- Code Chank 27 --------
test_forecast(gdp, forecast.obj = fc_holt, test = test)
# -------- Code Chank 28 --------
data(USgas)

decompose(USgas) %>% plot()
# -------- Code Chank 29 --------
USgas_par <- ts_split(USgas, 12)

train <- USgas_par$train
test <- USgas_par$test
# -------- Code Chank 30 --------
md_hw <- HoltWinters(train)

md_hw
# -------- Code Chank 31 --------
test_forecast(actual = USgas, 
              forecast.obj = fc_hw, 
              test = test)
# -------- Code Chank 32 --------
shallow_grid <-  ts_grid(train, 
                         model = "HoltWinters",
                         periods = 6,
                         window_space = 6,
                         window_test = 12,
                         hyper_params = list(alpha = seq(0,1,0.1),
                                             beta = seq(0,1,0.1),
                                             gamma = seq(0,1,0.1)),
                         parallel = TRUE,
                         n.cores = 8)
# -------- Code Chank 33 --------
shallow_grid$grid_df[1:10,]
# -------- Code Chank 34 --------
deep_grid <-  ts_grid(train, 
                      model = "HoltWinters",
                      periods = 6,
                      window_space = 6,
                      window_test = 12,
                      hyper_params = list(alpha = seq(0.1,0.5,0.01),
                                          beta = seq(0,0.1,0.01),
                                          gamma = seq(0.2,0.4,0.01)),
                      parallel = TRUE,
                      n.cores = 8)
# -------- Code Chank 35 --------
plot_grid(deep_grid)
# -------- Code Chank 36 --------
plot_grid(deep_grid, type = "3D", top = 250)
# -------- Code Chank 37 --------
md_hw_grid <- HoltWinters(train, 
                          alpha = deep_grid$alpha,
                          beta = deep_grid$beta,
                          gamma = deep_grid$gamma)

fc_hw_grid <- forecast(md_hw_grid, h = 12)


test_forecast(actual = USgas, 
              forecast.obj = fc_hw_grid, 
              test = test)

accuracy(fc_hw_grid, test)

