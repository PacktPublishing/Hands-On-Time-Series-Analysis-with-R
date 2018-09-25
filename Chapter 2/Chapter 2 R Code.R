# -------- Code Chank 1 --------
# The Sys.Date function get the current date from the system 
date <- Sys.Date() 
date
class(date)

# The Sys.time function get the current date and time from the system 
# The defualt format is POSIXct
time_ct <- Sys.time() 
time_ct
class(time_ct)

# Converting the POSIXct object to POSIXlt 
time_lt <- as.POSIXlt(time_ct)
time_lt
class(time_lt)

# -------- Code Chank 2 --------
# Unclass the POSIXct object
unclass(time_ct)
# Unclass the POSIXlt object
unclass(time_lt)

# -------- Code Chank 3 --------
# Quering for the second value 
unclass(time_lt)$sec
# Quering for the day of the year value
unclass(time_lt)$yday

# -------- Code Chank 4 --------
date <- as.Date("2014-5-12")
date
class(date)

# Creating POSIXct and POSIXlt objects
# Setting the time zone to EST
time_ct <- as.POSIXct("2014-5-12 20:05:35", tz = "EST")
time_ct
class(time_ct)

time_lt <- as.POSIXlt("2014-5-12 20:05:35", tz = "EST")
time_lt
class(time_lt)


# -------- Code Chank 5 --------
url <- "https://raw.githubusercontent.com/RamiKrispin/Hands-On-Time-Series-Analysis-with-R/master/dates_formats.csv"

dates_df <- read.csv(url, stringsAsFactors = FALSE)

# -------- Code Chank 6 --------
str(dates_df)

# -------- Code Chank 7 --------
dates_df$Japanese_format_new <- as.Date(dates_df$Japanese_format)

# -------- Code Chank 8 --------
head(dates_df[, c("Japanese_format", "Japanese_format_new")])

# -------- Code Chank 9 --------
identical(dates_df$Japanese_format, dates_df$Japanese_format_new)
class(dates_df$Japanese_format)
class(dates_df$Japanese_format_new)

# -------- Code Chank 10 --------
as.Date("31-01-2018") 

# -------- Code Chank 11 --------
as.Date("31-01-2018", format = "%d-%m-%Y")

# -------- Code Chank 12 --------
# Excel US date format
dates_df$US_format[1]  
dates_df$US_format_new <- as.Date(dates_df$US_format, 
                                  format = "%m/%d/%Y") 

# Excel US date long format 
dates_df$US_long_format[1] 
dates_df$US_long_format_new <- as.Date(dates_df$US_long_format, 
                                       format = "%A, %B %d, %Y")

# Excel Canada format
dates_df$CA_mix_format[1] 
dates_df$CA_mix_format_new <- as.Date(dates_df$CA_mix_format, 
                                      format = "%B %d, %Y")

# Excel South Africa format
dates_df$SA_mix_format[1] 
dates_df$SA_mix_format_new <- as.Date(dates_df$SA_mix_format, 
                                      format = "%d %B %Y") 

# Excel South Africa format
dates_df$NZ_format[1] 
dates_df$NZ_format_new <- as.Date(dates_df$NZ_format, 
                                  format = "%d/%m/%Y") 

# -------- Code Chank 13 --------
date1 <- as.Date("1970-01-01")
date2 <- Sys.Date()

print(c(date1, date2))

# -------- Code Chank 14 --------
date2 - date1

# -------- Code Chank 15 --------
as.numeric(date1)
as.numeric(date2)

# -------- Code Chank 16 --------
head(dates_df$Excel_Numeric_Format)

# -------- Code Chank 17 --------
dates_df$Excel_Numeric_Format_new <- as.Date(dates_df$Excel_Numeric_Format, 
                                             origin = as.Date("1899-12-30"))
head(dates_df$Excel_Numeric_Format_new)

# -------- Code Chank 18 --------
str(dates_df)

# -------- Code Chank 19 --------
# Storing a time object as a string
time_str <- "2018-12-31 23:59:59"
class(time_str)

# Converting the time object to POSIXct object
time_posix_ct1 <- as.POSIXct(time_str)
class(time_posix_ct1)

# Lets see how the two objects looks like
time_str
time_posix_ct1

# Test if the two objects are identical 
identical(time_str, time_posix_ct1)

# -------- Code Chank 20 --------
# Storing a numeric representation of the time object
time_numeric <- 1546318799
class(numeric)
# Converting the numeric time object to a POSIXct object
time_posix_ct2  <- as.POSIXct(time_numeric, origin = "1970-01-01")

# Let's print the two objects
print(c(time_posix_ct1, time_posix_ct2))
# Let's check if the two objects are identical
identical(time_posix_ct1, time_posix_ct2)

# -------- Code Chank 21 --------
# Store the time object as a string input
time_US_str <- "Monday, December 31, 2018 11:59:59 PM"

# Convert the time input to POSIXct format
time_posix_ct3 <- as.POSIXct(time_US_str,
                             format = "%A, %B %d, %Y %I:%M:%S %p")

time_posix_ct3
# Check if the 3 conversion are identicals
identical(time_posix_ct1, time_posix_ct2, time_posix_ct3)

# -------- Code Chank 22 --------
# Returning the location
Sys.timezone()
# or returning the abbreviation
Sys.timezone(location = FALSE)

# -------- Code Chank 23 --------
# Let's use again the same time object as earlier
time_str <- "2018-12-31 23:59:59"
# Converting the object without declaring about the time zone
time_default_tz <- as.POSIXct(time_str)
# Converting the object but declaring about the time zone
time_assign_tz <- as.POSIXct(time_str, tz = "GMT")

# Those object are not identical
print(c(time_default_tz, time_assign_tz))
identical(time_default_tz, time_assign_tz)

# -------- Code Chank 24 --------
head(OlsonNames(), 20)

# -------- Code Chank 25 --------
# Creating daily index with date object
daily_index <- seq.Date(from = as.Date("2016-01-01"), # Setting the starting date
                        to = as.Date("2018-12-31"),  # Setting the end date
                        by = "day" # Setting the time increment
)

head(daily_index)

# -------- Code Chank 26 --------
# Creating an index with 3 days interval space
daily_3_index <- seq.Date(from = as.Date("2016-01-01"), # Setting the starting date
                          to = as.Date("2018-12-31"),  # Setting the end date
                          by = "3 days" # Setting the time increment
)

head(daily_3_index)

# -------- Code Chank 27 --------
hourly_index <- seq.POSIXt(from = as.POSIXct("2018-06-01"), by = "hours", length.out = 48)

str(hourly_index)

# -------- Code Chank 28 --------
# Store the time object as a string input
time_US_str <- "Monday, December 31, 2018 11:59:59 PM"
class(time_US_str)
time_US_str

# -------- Code Chank 29 --------
# Time object conversion with the "base" package
time_base <- as.POSIXct(time_US_str,
                        format = "%A, %B %d, %Y %I:%M:%S %p")
class(time_base)
time_base

# -------- Code Chank 30 --------
# Time object conversion with the "lubridate" package
library(lubridate) 

time_lubridate <- mdy_hms(time_US_str, tz = "EST")

class(time_lubridate)
time_lubridate

# -------- Code Chank 31 --------
url <- "https://raw.githubusercontent.com/RamiKrispin/Hands-On-Time-Series-Analysis-with-R/master/dates_formats.csv"

dates_df <- read.csv(url, stringsAsFactors = FALSE)
str(dates_df)

dates_df$Japanese_format_new <- ymd(dates_df$Japanese_format)
dates_df$US_format_new <- mdy(dates_df$US_format)
dates_df$US_long_format_new <- mdy(dates_df$US_long_format)
dates_df$CA_mix_format_new <- mdy(dates_df$CA_mix_format)
dates_df$SA_mix_format_new <- dmy(dates_df$SA_mix_format)
dates_df$NZ_format_new <- dmy(dates_df$NZ_format)

# -------- Code Chank 32 --------
dates_df$Excel_Numeric_Format_new <- as_date(dates_df$Excel_Numeric_Format, origin = ymd("1899-12-30"))

str(dates_df)
# -------- Code Chank 33 --------
time_obj <- mdy_hms(time_US_str, tz = "EST")
class(time_obj)
time_obj 
yday(time_obj) # Extract the day of the year
qday(time_obj) # Extract the day of the quarter
day(time_obj) # Extract the day of the month
wday(time_obj, label = TRUE) # Extract the day of the week

# -------- Code Chank 34 --------
hour(time_obj) <- 11 # Modifying the time from 11 p.m to 11 a.m
time_obj

# -------- Code Chank 35 --------
time_obj <- mdy_hms(time_US_str, tz = "EST")
time_obj
round_date(time_obj, unit = "minute") # Rounding by the minute 
floor_date(time_obj, unit = "hour") # Rounding down by the hour
ceiling_date(time_obj, unit = "day") # Rounding up by the day