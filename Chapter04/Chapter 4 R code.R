# Chapter 4 Code

# -------- Code Chank 1 --------
library(TSstudio)

data(EURO_Brent)

library(zoo)
# -------- Code Chank 2 --------
ts_info(EURO_Brent)
# -------- Code Chank 3 --------
class(EURO_Brent)
# -------- Code Chank 4 --------
frequency(EURO_Brent)
head(cycle(EURO_Brent), 12)
# -------- Code Chank 5 --------
start(EURO_Brent)
end(EURO_Brent)
head(time(EURO_Brent), 12)
# -------- Code Chank 6 --------
head(index(EURO_Brent))
# -------- Code Chank 7--------
class(index(EURO_Brent))
attributes(index(EURO_Brent))
# -------- Code Chank 8 --------
index(EURO_Brent) <- as.Date(index(EURO_Brent))
head(EURO_Brent)
class(index(EURO_Brent))
# -------- Code Chank 9 --------
monthly_dates <- seq.Date(from = as.Date("2019-01-01"), length.out = 12, by = "month")

head(monthly_dates)
# -------- Code Chank 10 --------
monthly_yearmon <- as.yearmon(monthly_dates)

head(monthly_yearmon)
# -------- Code Chank 11 --------
data(USgas)

head(time(USgas))
# -------- Code Chank 12 --------
head(as.Date.ts(USgas))

# -------- Code Chank 13 --------
data(US_indicators)

str(US_indicators)
# -------- Code Chank 14 --------
Vehicle_Sales1 <- zoo(x = US_indicators$`Vehicle Sales`,
                      frequency = 12)

class(Vehicle_Sales1 )

frequency(Vehicle_Sales1)

head(Vehicle_Sales1)
# -------- Code Chank 15 --------
Vehicle_Sales2 <- zoo(x = US_indicators$`Vehicle Sales`,
                      order.by = US_indicators$Date,
                      frequency = 12)

head(Vehicle_Sales2)

class(index(Vehicle_Sales2))
# -------- Code Chank 16 --------
data(USgas)

USgas_zoo <- as.zoo(USgas)
# -------- Code Chank 17 --------
ts_info(USgas)
ts_info(USgas_zoo)
# -------- Code Chank 18 --------
is.regular(EURO_Brent, strict = TRUE)

is.regular(Vehicle_Sales1, strict = TRUE)

is.regular(Vehicle_Sales2, strict = TRUE)

is.regular(USgas_zoo, strict = TRUE)

# -------- Code Chank 19 --------
is.regular(EURO_Brent, strict = FALSE)
is.regular(Vehicle_Sales2, strict = FALSE)
# -------- Code Chank 20 --------
US_indicators_zoo <- zoo(x = US_indicators[,c("Vehicle Sales", "Unemployment Rate")], 
                         frequency = 12,
                         order.by = US_indicators$Date)
# -------- Code Chank 21 --------
ts_info(US_indicators_zoo)
head(US_indicators_zoo)
# -------- Code Chank 22 --------
is.regular(US_indicators_zoo, strict = FALSE)
# -------- Code Chank 23 --------
data(Michigan_CS)

ts_info(Michigan_CS)

class(Michigan_CS)
# -------- Code Chank 24 --------
class(index(Michigan_CS))

frequency(Michigan_CS)

is.regular(Michigan_CS, strict = TRUE)
# -------- Code Chank 24 --------
head(Michigan_CS)
# -------- Code Chank 25 --------
library(xts)

US_indicators_xts <- xts(x = US_indicators[,c("Vehicle Sales", "Unemployment Rate")], 
                         frequency = 12,
                         order.by = US_indicators$Date)

head(US_indicators_xts)
# -------- Code Chank 25 --------
periodicity(Michigan_CS)
# -------- Code Chank 26 --------
indexClass(Michigan_CS)
# -------- Code Chank 27 --------
Michigan_CS <-  convertIndex(Michigan_CS, "Date")

indexClass(Michigan_CS)
# -------- Code Chank 28 --------
head(.indexmon(Michigan_CS), 12)
# -------- Code Chank 29 --------
indexFormat(Michigan_CS) <- "%m-%d-%Y"

head(Michigan_CS)
# -------- Code Chank 30 --------
Vehicle_Sales_xts1 <- US_indicators_xts$`Vehicle Sales`[1:12]

ts_info(Vehicle_Sales_xts1)
# -------- Code Chank 31 --------
Vehicle_Sales_xts2 <- US_indicators_xts$`Vehicle Sales`["1976"]

ts_info(Vehicle_Sales_xts2)
# -------- Code Chank 32 --------
Vehicle_Sales_xts3 <- US_indicators_xts$`Vehicle Sales`["1976-01/06"]
ts_info(Vehicle_Sales_xts3)
# -------- Code Chank 33 --------
indexClass(Michigan_CS)
class(index(EURO_Brent))
# -------- Code Chank 34 --------
ts_info(Michigan_CS)
ts_info(EURO_Brent)
# -------- Code Chank 35 --------
xts_merge_outer <- merge.xts(Michigan_CS = Michigan_CS, 
                             EURO_Brent = EURO_Brent, 
                             join = "outer" )

ts_info(xts_merge_outer)
# -------- Code Chank 36 --------
head(xts_merge_outer["1987"])
# -------- Code Chank 37 --------
xts_merge_inner <- merge.xts(Michigan_CS = Michigan_CS, 
                             EURO_Brent = EURO_Brent, 
                             join = "inner" )

ts_info(xts_merge_inner)
# -------- Code Chank 38 --------
head(xts_merge_inner)
# -------- Code Chank 39 --------
EURO_Brent_3ma <- rollapply(EURO_Brent, 
                            width = 3,
                            FUN = mean)
# -------- Code Chank 40 --------
ts_info(EURO_Brent_3ma)
# -------- Code Chank 41 --------
EURO_Brent_lag3 <-  lag(EURO_Brent, k = -3)

EURO_Brent_merge <- merge.zoo(EURO_Brent, EURO_Brent_lag3)

head(EURO_Brent_merge)
# -------- Code Chank 42 --------
USgas_zoo_qtr <-  aggregate(USgas_zoo, 
                            by = as.yearqtr, 
                            FUN = sum)
# -------- Code Chank 43 --------
ts_info(USgas_zoo)
# -------- Code Chank 44 --------
library(lubridate)

USgas_zoo_yr <-  aggregate(USgas_zoo, 
                           by = year, 
                           FUN = sum)

head(USgas_zoo_yr)
# -------- Code Chank 45 --------
plot.zoo(EURO_Brent, 
         main = "Crude Oil Prices: Brent - Europe",
         ylab = "USD per Barrel",
         col = "blue")

# -------- Code Chank 46 --------
plot.zoo(US_indicators_zoo,
         main = "Monthly Vehicle Sales and Unemployment Rate in the US",
         ylab = c("Vehicle Sales (Thousands of Units)", "Unemployment Rate (%)"),
         col = c("blue", "red"))
# -------- Code Chank 47 --------
plot.xts(Michigan_CS)
# -------- Code Chank 48 --------
plot.xts(Michigan_CS,
         subset = "2010/",
         main = "University of Michigan Consumer Sentiment Index",
         col = "blue",
         grid.ticks.on = "years",
         minor.ticks = "years")
# -------- Code Chank 49 --------
plot.xts(US_indicators_xts,
         multi.panel = 2,
         yaxis.same = FALSE, 
         grid.ticks.on = "years",
         minor.ticks = FALSE,
         main = "Monthly Vehicle Sales and Unemployment Rate in the US")
# -------- Code Chank 50 --------
USgas_xts <- as.xts(USgas) 

indexClass(USgas_xts)
# -------- Code Chank 51 --------
USgas_xts_ma <- rollapply(USgas_xts, 
                          width = 12, 
                          FUN = mean)
# -------- Code Chank 52 --------
USgas_merge <- merge.xts(USgas = USgas_xts, 
                         USgas_Smooth = USgas_xts_ma)
# -------- Code Chank 53 --------
USgas_month_diff <- 100 * (USgas_xts / lag(USgas_xts, n = 1) - 1)
USgas_yoy_diff <- 100 * (USgas_xts / lag(USgas_xts, n = 12) - 1)
# -------- Code Chank 54 --------
plot.xts(USgas_merge, 
         main = "US Natural Gas Consumption Summary",
         multi.panel = FALSE,
         col = c("black", "blue"),
         ylim = c(1400, 3700)) 
# -------- Code Chank 55 --------
lines(USgas_month_diff , 
      col = "red", 
      type = "h", 
      on = NA, 
      main = "Monthly Difference (%)")

lines(USgas_yoy_diff , 
      col = "purple", 
      type = "h", 
      on = NA, 
      main = "YoY Growth (%)")
# -------- Code Chank 56 --------
addLegend("topleft", 
          on=1, 
          legend.names = c("Gas Consumption", "Moving Average", "Monthly Diff. (%)", "YoY Change (%)"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "blue", "red", "purple"))
