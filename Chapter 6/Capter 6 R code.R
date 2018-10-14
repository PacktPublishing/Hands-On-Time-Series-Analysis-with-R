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

# -------- Code Chank 1 --------



# -------- Code Chank 1 --------