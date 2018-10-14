# Chapter 6 Code

# -------- Code Chank 1 --------
library(TSstudio)
data(USgas)

ts_info(USgas)
# -------- Code Chank 2 --------
ts_plot(USgas, 
        title = "US Monthly Total Natural Gas Consumption", 
        Ytitle = "Billion Cubic Feet",
        Xgrid = TRUE,
        Ygrid = TRUE) 
# -------- Code Chank 3 --------
library(UKgrid)
UKgrid <- extract_grid(type = "xts", 
                       columns = "ND",
                       aggregate = "hourly",
                       na.rm = TRUE)

ts_info(UKgrid)

# -------- Code Chank 4 --------
ts_plot(UKgrid, 
        title = "National Hourly Demand UK Grid", 
        Ytitle = "Megawatts",
        Xgrid = TRUE,
        Ygrid = TRUE) 


# -------- Code Chank 5 --------
# Transforming the ts object to data.frame object
USgas_df <- data.frame(year = floor(time(USgas)),  
                       month = cycle(USgas), 
                       USgas = as.numeric(USgas))

# Setting the month abbreviation and transfroming it to a factor
USgas_df$month <- factor(month.abb[USgas_df$month], levels = month.abb)

head(USgas_df)
# -------- Code Chank 6 --------

# -------- Code Chank 7 --------

# -------- Code Chank 8 --------

# -------- Code Chank 9 --------