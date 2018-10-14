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
# Summarized the series by its frequency units
library(dplyr)

USgas_summary <- USgas_df %>% 
  group_by(month) %>%
  summarise(mean = mean(USgas),
            sd = sd(USgas))

USgas_summary
# -------- Code Chank 7 --------
library(plotly)

plot_ly(data = USgas_summary,
        x = ~ month,
        y = ~ mean,
        type = "bar",
        name = "Mean") %>%
  layout(title = "USgas - Monthly Average",
         yaxis = list(title = "Mean", range = c(1500, 2700)))
# -------- Code Chank 8 --------
library(xts)
UKgrid_df <- data.frame(time = index(UKgrid), UKgrid = as.numeric(UKgrid))
str(UKgrid_df)
# -------- Code Chank 9 --------
library(lubridate)
UKgrid_df$hour <- hour(UKgrid_df$time)
UKgrid_df$weekday <- wday(UKgrid_df$time, 
                          label = TRUE,
                          abbr = TRUE)
UKgrid_df$month <- factor(month.abb[month(UKgrid_df$time)], levels = month.abb)

head(UKgrid_df)
# -------- Code Chank 10 --------
UKgrid_hourly <- UKgrid_df %>% 
  dplyr::group_by(hour) %>%
  dplyr::summarise(mean = mean(UKgrid, na.rm = TRUE),
                   sd = sd(UKgrid, na.rm = TRUE)) 
# -------- Code Chank 11 --------
# Plotting the mean and the standard deviation
plot_ly(UKgrid_hourly) %>%
  add_lines(x = ~ hour, y = ~ mean, name = "Mean") %>%
  add_lines(x = ~ hour, y = ~ sd, name = "Standard Deviation", yaxis = "y2",
            line = list(color = "red", dash = "dash", width = 3)) %>%
  layout(
    title = "The UK Grid National Demand - Hourly Average vs. Standard Deviation", 
    yaxis = list(title = "Mean"),
    yaxis2 = list(overlaying = "y",  
                  side = "right",
                  title = "Standard Deviation"
    ),
    xaxis = list(title="Hour of the day"),
    legend = list(x = 0.05, y = 0.9),
    margin = list(l = 50, r = 50)
  )
# -------- Code Chank 12 --------
UKgrid_weekday <- UKgrid_df %>% 
  dplyr::filter(hour == 3 | hour == 9) %>%
  dplyr::group_by(hour, weekday) %>%
  dplyr::summarise(mean = mean(UKgrid, na.rm = TRUE),
                   sd = sd(UKgrid, na.rm = TRUE)) 

UKgrid_weekday$hour <- factor(UKgrid_weekday$hour)

plot_ly(data = UKgrid_weekday, x = ~ weekday, y = ~ mean, type = "bar",color = ~ hour) %>%
  layout(title = "The Hourly Average Demand by Weekday",
         yaxis = list(title = "Mean", range = c(30000, 75000)), 
         xaxis = list(title = "Weekday"))
# -------- Code Chank 13 --------
UKgrid_month <- UKgrid_df %>% 
  dplyr::filter(hour == 3 | hour == 9) %>%
  dplyr::group_by(hour, month) %>%
  dplyr::summarise(mean = mean(UKgrid, na.rm = TRUE),
                   sd = sd(UKgrid, na.rm = TRUE)) 

UKgrid_month$hour <- factor(UKgrid_month$hour)

plot_ly(data = UKgrid_month, x = ~ month, y = ~ mean, type = "bar",color = ~ hour) %>%
  layout(title = "The Hourly Average Demand by Weekday",
         yaxis = list(title = "Mean", range = c(30000, 75000)), 
         xaxis = list(title = "Month"))
# -------- Code Chank 14 --------
# Plotting the density of each freqeuncy unit
library(ggplot2)
ggplot(USgas_df, aes(x = USgas)) + 
  geom_density(aes(fill = month)) + 
  ggtitle("USgas - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(as.factor(month)))
# -------- Code Chank 10 --------
# -------- Code Chank 10 --------
# -------- Code Chank 10 --------
# -------- Code Chank 10 --------
# -------- Code Chank 10 --------