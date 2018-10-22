# Chapter 4 Code

# -------- Code Chank 1 --------
# Loading the Brent Crude Oil Prices dataset
data("EURO_Brent", package = "TSstudio")

# Loading the zoo package
library(zoo)

# Lets check the first 10 observations of the series
head(EURO_Brent, 10)

# Plotting the series
plot.zoo(EURO_Brent, 
         main = "Brent Crude Oil Prices",
         ylab = "USD per Barrel")
# -------- Code Chank 2 --------
class(EURO_Brent)
# -------- Code Chank 3 --------
# Using the stats package functions to extract the series attributes
frequency(EURO_Brent)
start(EURO_Brent)
end(EURO_Brent)
head(cycle(EURO_Brent), 12)
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

# -------- Code Chank 1 --------