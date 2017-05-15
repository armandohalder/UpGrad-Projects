# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(Hmisc)   # describe

# ***************************************************************************
#                   PROCs ----
# ***************************************************************************
nweek <- function(x, format="%Y-%m-%d", origin){
  if(missing(origin)){
    as.integer(format(strptime(x, format=format), "%W"))
  }else{
    x <- as.Date(x, format=format)
    o <- as.Date(origin, format=format)
    w <- as.integer(format(strptime(x, format=format), "%w"))
    2 + as.integer(x - o - w) %/% 7
  }
}




# ***************************************************************************
#                   LOAD DATA ---- Transaction Data ----
# ***************************************************************************
# Make sure you are in current directory as in R-file is in. Should I do a commit?yes..

ce_data <- read.csv('./data/ConsumerElectronics.csv',stringsAsFactors = FALSE)

str(ce_data)

# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************

head(ce_data)

# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
ce_data$order_date <- format(as.POSIXct(ce_data$order_date,format='%Y-%m-%d'),
                             format='%Y-%m-%d')
ce_data$order_date <- as.Date(ce_data$order_date, format = "%Y-%m-%d")

ce_data <- subset(ce_data, order_date > "2015-6-30" & order_date < "2016-7-1")

#NA Values
sapply(ce_data, function(x) sum(is.na(x)))
#Removed NA values from GMV
ce_data <- na.omit(ce_data)
#Removed MRP = 0
ce_data <- subset(ce_data, product_mrp != 0)

# Lets add a couple of variables to the CE data

#....List Price variable
ce_data$List_Price <- as.integer(ce_data$gmv / ce_data$units)

#....Promotion Variable
ce_data$Promotion <- as.integer((ce_data$product_mrp - ce_data$List_Price) / ce_data$product_mrp)
str(ce_data$product_mrp)


# ***************************************************************************
#                   FEATURE ENGINEERING ----
# ***************************************************************************

# create week,  week numbers start from min 'order date'
# . . . . Week Numbers ----
dates <- as.Date(
  gsub(" .*","",ce_data$order_date)
)
ce_data$week <- nweek(dates,origin = as.Date("2015-07-01"))


# . . . . Days, weeks, Month ----
# will compute Month, week, and no.of days per week (month, week)
# 
dys <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=dys, Month = month(dys), 
                       week = nweek(dys,origin = as.Date("2015-07-01")),
                       nweek = rep(1,length(dys)))
weekdays <- data.frame(weekdays %>% group_by(Month,week) %>% summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7


# . . . . Strip Spaces ----
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)
