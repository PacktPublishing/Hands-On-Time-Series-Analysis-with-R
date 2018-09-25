# Chapter 3 Code

# -------- Code Chank 1 --------
library(Quandl)

NGC <-Quandl(code = "FRED/NATURALGAS",
             collapse="quarterly",
             type = "ts",
             end_date = "2017-12-31")

class(NGC)
# -------- Code Chank 2 --------
plot.ts(NGC,
        main = "US Quarterly Natural Gas Consumption",
        ylab = "Billion of Cubic Feet"
)
# -------- Code Chank 3 --------
is.ts(NGC) # Test if the object is a "ts" class
length(NGC) # Get the number of observations

# -------- Code Chank 4 --------
NGC

# -------- Code Chank 5 --------
cycle(NGC)
time(NGC)

# -------- Code Chank 6 --------
frequency(NGC)
deltat(NGC)

# -------- Code Chank 7 --------
library(TSstudio)

data(Coffee_Prices)

class(Coffee_Prices)

head(Coffee_Prices)

# -------- Code Chank 8 --------
frequency(Coffee_Prices)
deltat(Coffee_Prices)
head(time(Coffee_Prices))
head(cycle(Coffee_Prices))

# -------- Code Chank 9 --------
my_ts <- ts(data = 1:60, # The series values
            start = c(2010, 1), # The time of the first observation
            end = c(2014, 12),  # The time of the last observation
            frequency = 12) # The Series frequency

class(my_ts)
my_ts
# -------- Code Chank 10 --------
# Define the start time with two integers vector
series_1 <- ts(data = 1:60,
               start = c(2010, 1),
               frequency = 12)
# Define the start time with a single integer
series_2 <- ts(data = 1:60,
               start = 2010,
               frequency = 12)
# Define the end time with two integers vector
series_3 <- ts(data = 1:60,
               end = c(2014, 12),
               frequency = 12)

# comparing all the three series to the my_ts series we defined above
identical(my_ts, series_1, series_2, series_3)

# -------- Code Chank 11 --------
library(TSstudio)

data(US_indicators)

str(US_indicators)
# -------- Code Chank 12 --------
tvs <- US_indicators[, c("Date", "Vehicle Sales")] 
str(tvs)

# -------- Code Chank 13 --------
library(dplyr)
tvs <- tvs %>% arrange(Date)
head(tvs)

# -------- Code Chank 14 --------
library(lubridate)

tvs$year <- year(tvs$Date)
tvs$month <- month(tvs$Date)

head(tvs)

# -------- Code Chank 15 --------
first_cycle_number <- tvs$year[which.min(tvs$Date)]
first_cycle_unit <- tvs$month[which.min(tvs$Date)]

print(c(first_cycle_number, first_cycle_unit))

# -------- Code Chank 16 --------
tvs_ts <- ts(data = tvs$`Vehicle Sales`,
             start = c(first_cycle_number, first_cycle_unit),
             frequency = 12)

# -------- Code Chank 17 --------
head(tvs$Date, 5)
head(time(tvs_ts), 5)

# -------- Code Chank 18 --------
head(tvs$`Vehicle Sales`)
head(tvs_ts)
identical(tvs$`Vehicle Sales`, as.numeric(tvs_ts))

# -------- Code Chank 19 --------
US_indicators <- US_indicators %>% arrange(Date)

US_indicators_ts <- ts(data = US_indicators[, c("Vehicle Sales", "Unemployment Rate")],
                       start = c(year(min(tvs$Date)), month(min(tvs$Date))), 
                       frequency = 12)
class(US_indicators_ts)

head(US_indicators_ts)
# -------- Code Chank 20 --------
start(NGC)
end(NGC)
frequency(NGC)
# -------- Code Chank 21 --------
window(NGC, start = c(2005,1), end = c(2005, 4))

# -------- Code Chank 22 --------
window(NGC, start = c(2000, 3), frequency = 1)

# -------- Code Chank 23 --------
window(NGC, start = c(2006, 3), end = c(2012, 3),frequency = 1)

# -------- Code Chank 24 --------
NGC_year <- aggregate(NGC, nfrequency = 1, FUN = "sum")

# -------- Code Chank 25 --------
NGC_year

# -------- Code Chank 26 --------
plot.ts(tvs_ts, 
        main = "US Monthly Total Vehicle Sales",
        ylab = "Thousands of Vehicle",
        xlab = "Time"
)

# -------- Code Chank 27 --------
plot.ts(US_indicators_ts,
        plot.type = "multiple",
        main = "US Monthly Vehicle Sales vs. Unemployment Rate",
        xlab = "Time")
# -------- Code Chank 28 --------
library(dygraphs)  

dygraph(tvs_ts, 
        main = "US Monthly Total Vehicle Sales",
        ylab = "Thousands of Vehicle") %>% 
  dyRangeSelector()

# -------- Code Chank 29 --------
dygraph(US_indicators_ts,
        main = "US Monthly Vehicle Sales vs. Unemployment Rate") %>%
  dyAxis("y", label = "Vehicle Sales") %>%
  dyAxis("y2", label = "Unemployment Rate") %>%
  dySeries("Vehicle Sales", axis = 'y', color = "green") %>%
  dySeries("Unemployment Rate", axis = 'y2', color = "red") %>%
  dyLegend(width = 400)

# -------- Code Chank 30 --------
library(TSstudio)

ts_plot(tvs_ts,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of Vehicle",
        slider = TRUE
)
# -------- Code Chank 31 --------
ts_plot(US_indicators_ts, 
        title = "US Monthly Vehicle Sales vs. Unemployment Rate",
        type = "multiple")
