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
library(TSstudio)
data(USgas)

ts_plot(USgas, line.mode = "lines+markers") %>% 
  layout(title = "Monthly Natural Gas Consumption",
         yaxis = list(title = "Billions Cubic Feet"))

# -------- Code Chank 3 --------
data("USVSales")

ts_plot(USVSales, color = "green", line.mode = "lines+markers") %>% 
  layout(title = "Monthly Total Vehicle Sales",
         yaxis = list(title = "Thousands of Units"))
# -------- Code Chank 4 --------
data("USUnRate")

ts_plot(USUnRate, 
        color = "red", 
        line.mode = "lines+markers") %>% 
  layout(title = "Monthly US Unemployment Rate",
         yaxis = list(title = "Percent"))
# -------- Code Chank 5 --------
# Assigning values to new variable
str <- "Hello World!"
int <- 10
vec <- c(1,2,3,4)
# Printing the variable values
print(c(str, int)) 
print(vec)
# -------- Code Chank 6 --------
# Lets assign values to new variables
x <- 10
y <- 2
x + y # Addition
x/ 2.5 # Division
y ^ 3 # Exponentiation (alternatively you can use the ** operator)
# -------- Code Chank 7 --------
# The following are reserved names in R for Boolean objects:
# TRUE, FALSE or their shortcut T and F
a <- TRUE
b <- FALSE

a & b # The operator AND
a | b # The operator OR
!a # The operator NOT



if(a & b){ # The operator AND, will return TRUE only if both a and b are TRUE
  print("a AND b are true")
} else print("a AND b are flase")

if(a | b){ # The operator OR, will return FALSE only if both a and b are FALSE
  print("a OR b are true")
} else print("a OR b are flase")

# We can also test if a Boolean object is TRUE or FALSE
isTRUE(a) 
isTRUE(b)
# -------- Code Chank 8 --------
a <- b <- 5 # assign for variables a and b the value 5
c <- 7

if(a == b){
  print("a is equal to b")
} else{
  print("a is not equal to b")
}

d <- ifelse(a >= c, "a is greater or equal to c", "a is smaller than c" ) # this is another way to use if else stetment
print(d)
# -------- Code Chank 9 --------
# Each package must have documentation for each function
# To access the function documentation use the ? or the help(function) 
?assignOps
?Arithmetic
?Logic
?Comparison 
# -------- Code Chank 10 --------
# Installing the forecast package:
install.packages("forecast")

# You may install more than one package at once:
install.packages(c("TSstudio", "xts", "zoo"))
# -------- Code Chank 11 --------
# Check for the forecast package version:
packageVersion("forecast")

# Create a list of packages that updates are available:
old.packages()

# Update all packages that updates are available:
update.packages()

# To update a specific package use the install.package() function:
install.packages("lubridate")

# Remove a package:
# Before removing a package make sure that the package is not loaded!
remove.packages("forecast")
# -------- Code Chank 12 --------
if("package:TSstudio" %in% search()){
  detach("package:TSstudio", unload=TRUE)  
}
# The search() function provides an overview about the loaded packages within your environment:
search()
# -------- Code Chank 13 --------
# Lets load the package TSstudio:
library(TSstudio)

# Lets check again for the changes in the global environment after loading the three packages:
search()
# -------- Code Chank 14 --------
# Unloading the TSstudio package from the global environment
detach("package:TSstudio", unload=TRUE)  

search()
# -------- Code Chank 15 --------
file_url <- "https://raw.githubusercontent.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R/master/Chapter%201/TOTALNSA.csv" # alternatively it could be a path on your hard-drive
?read.csv # The read.csv documentation 

df1 <- read.csv(file = file_url, stringsAsFactors = FALSE) 

# Parameteres
# file - define the file full name with the path of the file 
# stringsAsFactors - if TRUE will convert any non numeric value into factor

class(df1) # Testing the class of the object

str(df1) # Getting the object structure
# -------- Code Chank 16 --------
# We will use the Quandl package to connect to the Quandle API
library(Quandl) 

?Quandl # The Quandl function documentation  


df2 <- Quandl(code = "FRED/TOTALNSA", 
              type = "raw", 
              collapse = "monthly", 
              order = "asc",
              end_date="2017-12-31") 

# Parameters
# code - set the database (FRED) and the dataset (TOTALNSA) codes
# type - define the data type (data.frame, ts, xts, zoo, etc.)
# collapse - provide the ability to aggregate the data when applicable
# order - set the date order (descend or ascend)

class(df2) # Similarly to the df1 the class of df2 is data.frame 

str(df2) # Note that the attribute of the varaible here is Date

# -------- Code Chank 17 --------
# If the package is not installed on your machine:
install.packages("TSstudio")
# -------- Code Chank 18 --------
# Loading the dataset from the TSstudio package
data("USVSales", package = "TSstudio")

# Parameters
# package - define the package which the data is loaded from
#           not required if the package is already loaded

# Checking the class of the dataset
class(USVSales)
# Get the first observations in the data
head(USVSales)
# Get the time attribute of the data of the first observations
head(time(USVSales))
# -------- Code Chank 19 --------

# Loading dataset from datasets package
data("iris", package = "datasets")

# Getting the data frame structure 
str(iris)
# -------- Code Chank 20 --------
# Summary statistics for the iris data frame
summary(iris)
# -------- Code Chank 21 --------
iris[1:5, 2]
# -------- Code Chank 22 --------
iris$Sepal.Width[1:5]
# -------- Code Chank 23 --------
iris[1:5, "Sepal.Width"]
# -------- Code Chank 24 --------
iris[1:5, which(colnames(iris) == "Sepal.Width")]
# -------- Code Chank 25 --------
Setosa_df1 <- subset(x = iris, iris$Species == "setosa")
head(Setosa_df1)
# -------- Code Chank 26 --------
Setosa_df2 <- iris[which(iris$Species == "setosa"), ]
head(Setosa_df2)
# -------- Code Chank 27 --------
identical(Setosa_df1, Setosa_df2)
# -------- Code Chank 28 --------
summary(Setosa_df1)