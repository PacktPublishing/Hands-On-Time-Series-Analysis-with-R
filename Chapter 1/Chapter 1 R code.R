# Chapter 1 Code

# -------- Code Chank 1 --------
library(plotly)
library(dplyr)


url <- ("https://github.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R/blob/master/Chapter%201/newark_pas.rds")
download.file(url,"newark_pas.rds", method="curl")
newark_pas <- readRDS("./chapter 1/newark_pas.rds")

url <- ("https://github.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R/blob/master/Chapter%201/sp500.rds")
download.file(url,"sp500.rds", method="curl")
sp500 <-  readRDS("./chapter 1/sp500.rds")

url <- ("https://github.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R/blob/master/Chapter%201/un_rate.rds")
download.file(url,"un_rate.rds", method="curl")
un_rate <-  readRDS("./chapter 1/un_rate.rds")

url <- ("https://github.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R/blob/master/Chapter%201/uknd.rds")
download.file(url,"uknd.rds", method="curl")
uknd <-  readRDS("./chapter 1/uknd.rds")

subplot(uknd, sp500, un_rate, newark_pas, nrows = 2, margin = 0.07,
        shareY = FALSE, titleY = T) %>% hide_legend()
# -------- Code Chank 2 --------