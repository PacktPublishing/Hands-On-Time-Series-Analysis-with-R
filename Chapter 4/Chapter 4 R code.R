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
# -------- Code Chank 4 --------
head(index(EURO_Brent), 12)
# -------- Code Chank 5 --------
class(index(EURO_Brent))
# -------- Code Chank 6 --------
# Creating a sequence of dates
monthly_dates <- seq.Date(from = as.Date("2018-01-11"), length.out = 12, by = "month")

class(monthly_dates)

monthly_dates

# Converting the dates vector to yearmon format
yearmon_format <- as.yearmon(monthly_dates)

class(yearmon_format)

yearmon_format
# -------- Code Chank 7 --------
# Conveting the yearmon object back to Date object
as.Date(yearmon_format)
# -------- Code Chank 8 --------
# Loading the US_indicators dataset from the TSstudio package
data("US_indicators", package = "TSstudio")

class(US_indicators)

str(US_indicators)
# -------- Code Chank 9 --------
# Assigning the Vehicale Sales variable from the US_indicators data frame
zoo_obj <- zoo(x = US_indicators$`Vehicle Sales`)

class(zoo_obj)

head(zoo_obj)

head(index(zoo_obj))
# -------- Code Chank 10 --------
# Setting the date variable of the data frame as the series index
zoo_obj <- zoo(x = US_indicators$`Vehicle Sales`, 
               order.by = US_indicators$Date)

class(zoo_obj)

head(zoo_obj)

head(index(zoo_obj))
# -------- Code Chank 11 --------
# Setting both the order.by and frequency arguments 
zoo_obj <- zoo(x = US_indicators$`Vehicle Sales`, 
               order.by = US_indicators$Date,
               frequency = 12)

class(zoo_obj)

# Checking if the series is regular and strict
is.regular(zoo_obj, strict = FALSE)

is.regular(zoo_obj, strict = TRUE)
# -------- Code Chank 12 --------
# Creating a multiple time series object
zoo_mts_obj <- zoo(x = US_indicators[,2:3],  
                   order.by = as.yearmon(US_indicators$Date),
                   frequency = 12)

class(zoo_mts_obj)

head(zoo_mts_obj)
# -------- Code Chank 13 --------
plot.zoo(zoo_mts_obj,
         main = "Monthly Vehicle Sales and Unemployment Rate in the US ")
# -------- Code Chank 14 --------
# Converting a ts object ot a zoo object
# Loading the USVSales dataset form the TSstudio
data("USVSales", package = "TSstudio")

class(USVSales)

# Converting the dataset to a zoo object 
USVSales_zoo <- as.zoo(USVSales, frequency = frequency(USVSales))

class(USVSales_zoo)

head(USVSales)

head(USVSales_zoo)

# Checking if the zoo object is a regular time series
is.regular(USVSales_zoo, strict = FALSE)

# Checking if the zoo object is a strict regular time series
is.regular(USVSales_zoo, strict = TRUE)
# -------- Code Chank 15 --------
# Loading the xts package
library(xts)

# Loading the dataset from the TSstudio package
data("Michigan_CS", package = "TSstudio")

class(Michigan_CS)

head(Michigan_CS)
# -------- Code Chank 16 --------
plot.xts(xts_mts_obj,
         multi.panel = 2, # Use separate the plots, as the two series have different scale 
         yaxis.same = FALSE, # For the same reason, adjust the yaxis to the scale of each series
         grid.ticks.on = "years",  # Setting the grid ticks period to draw
         main = "Monthly Vehicle Sales and Unemployment Rate in the US ")
# -------- Code Chank 17 --------
start(xts_mts_obj)

end(xts_mts_obj)

frequency(xts_mts_obj)
# -------- Code Chank 18 --------
# Getting the series frequency/time interval and the starting and ending dates
periodicity(xts_mts_obj)
# -------- Code Chank 19 --------
# Working with the xts index
head(index(xts_mts_obj))

indexClass(xts_mts_obj)

# Modifing the series index from yearmon to Date
xts_mts_obj <- convertIndex(xts_mts_obj, "Date")

indexClass(xts_mts_obj)

head(xts_mts_obj)
# -------- Code Chank 20 --------
# Extracting the first 12 months of the vehicle sales dataset
class(xts_mts_obj)

names(xts_mts_obj)

# Extracting the first 12 months of the vehicle sales dataset
# using the column name and the row index
vehical_first_12m <- xts_mts_obj$`Vehicle Sales`[1:12]

vehical_first_12m
# -------- Code Chank 21 --------
# Checking the new series attributes
class(vehical_first_12m)

periodicity(vehical_first_12m)

is.regular(vehical_first_12m)
# -------- Code Chank 22 --------
# Extracing the first 12 months of the unemployment rate
# and assign it as a new variable in the xts object
vehical_first_12m$Unemployment.Rate <- xts_mts_obj[1:12, 2]

vehical_first_12m

class(vehical_first_12m)
# -------- Code Chank 23 --------
# Extracting all the observations during the year 1976
US_indicators_xts1 <- xts_mts_obj["1976"]

class(US_indicators_xts1)

US_indicators_xts1
# -------- Code Chank 24 --------
# Extracting all the observations of the year 1976
US_indicators_xts2 <- xts_mts_obj["1976-01/1976-12"]

# A more concise method 
US_indicators_xts3 <- xts_mts_obj["197601/12"]

# Which eventually yield the same results:
identical(US_indicators_xts1, US_indicators_xts2, US_indicators_xts3)
# -------- Code Chank 25 --------
data("USgas", package = "TSstudio")
periodicity(USgas)
class(USgas)

periodicity(xts_mts_obj)
class(xts_mts_obj)
# -------- Code Chank 26 --------
# Merging the xts_mts_obj and the USgas series
# By default, the merge function applying an outer join
US_ind <- merge(x = xts_mts_obj, USgas = USgas)
class(US_ind)
periodicity(US_ind)
# -------- Code Chank 27 --------
head(US_ind)
# -------- Code Chank 28 --------
# Applying inner join
US_ind_inner <- merge(x = xts_mts_obj, USgas = USgas, join = "inner")
class(US_ind_inner)
periodicity(US_ind_inner)
head(US_ind_inner)
tail(US_ind_inner)
# -------- Code Chank 29 --------
plot.xts(US_ind_inner,
         multi.panel = 3, # Use separate the plots, as the two series have different scale 
         yaxis.same = FALSE, # For the same reason, adjust the yaxis to the scale of each series
         grid.ticks.on = "years", # Setting the grid ticks period to draw 
         main = "Monthly Vehicle Sales, Unemployment Rate and Natural Gas Consumption in the US ")
# -------- Code Chank 31 --------
# Loading the US vehicle sales series
data(USVSales, package = "TSstudio")

# Applying a moving average of 3 months rolling window 
USVSales_smooth <- rollapply(USVSales, FUN = mean, width = 3)

# Merging the two series
# To apply the xts merge method, we will convert the objects to xts
USVSales_merged <- merge(Normal = as.xts(USVSales), Smooth = as.xts(USVSales_smooth), join = "inner")

plot.xts(USVSales_merged, 
         multi.panel = 2,
         grid.ticks.on = "years",
         main = "The US Vehicale Sales Before and After Smoothing")