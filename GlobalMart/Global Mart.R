
# *****************************************************************************
#   Task:       RETAIL SALES FORECASTING CASE STUDY
#   Course:     PGDDA Course, IIIT- BANGALORE
#   Time Line:  Oct - Dec 2016
#   Team:         AtchiReddy Chavva (atchireddi@gmail.com)
#                 Ananth Bommakanti (ananth.bommakanti@gmail.com)
#                 Armando Halder    (armando.halder@gmail.com)
#                 Apurva Nagoree    (nagoree@yahoo.com)
#
# *****************************************************************************


#
#   ____  ______  _____                       ____              ___  ______              ___ 
#  /        |    |     \   |          |      |    \  |   |  |  |       |       |       /    \
# |         |    |     |   |       ___|___   \____   |___|  |  |__     |     __|___   |      |
# \         |    | ---<    |          |           \  |   |  |  |       |       |      |      |
#  \____    |    |     |   |_____     |      \____/  |   |  |  |       |       |       \____/
#
#
#   IN R-STDIO PRESS    CTRL + SHIFT + O  For  CODE  OUTLINE/OVERVIEW
#   IN R-STDIO PRESS    CTRL + SHIFT + O  For  CODE  OUTLINE/OVERVIEW
#   IN R-STDIO PRESS    CTRL + SHIFT + O  For  CODE  OUTLINE/OVERVIEW
#   IN R-STDIO PRESS    CTRL + SHIFT + O  For  CODE  OUTLINE/OVERVIEW
#   IN R-STDIO PRESS    CTRL + SHIFT + O  For  CODE  OUTLINE/OVERVIEW
#
#
#


# **********************************************************************
# BUSINESS UNDERSTANDING ----
# **********************************************************************

#   “Global Mart” is an online store super giant having worldwide operations. 
# It takes orders and delivers across the globe and deals with all the major 
# product categories - consumer, corporate & home office.
# 
#   Now as a sales/operations manager, you want to finalise the plan for the 
# next 6 months.  So, you want to forecast the sales and the demand for the 
# next 6 months, that would help you manage the revenue and inventory accordingly.

#   The store caters to 7 different market segments and in 3 major categories. 
# You want to forecast at this granular level, so you subset your data into 
# 21 (7*3) buckets before analysing these data.

#   But not all of these 21 market buckets are important from the store’s 
# point of view. So you need to find out 5 most profitable (and consistent) 
# segment from these 21 and forecast the sales and demand for these segments.




# ***********************************************************************
#       DATA DICTIONARY ----
# ***********************************************************************

#   The data currently has the transaction level data, where each row represents 
# a particular order made on the online store. There are 24 attributes related 
# to each such transaction. The “Market” attribute has 7-factor levels representing 
# the market sector that the customer belongs to. The “Segment” attribute represents 
# the 3 segment that the product belongs to. You will find the complete data dictionary 
# for the dataset from the link below.

#  ___________________________________________________________________________
# |    Attributes,    |       Description                                     |
# |-------------------|-------------------------------------------------------|
# |    Order ID,      |   Unique ID of the transaction                        |
# |    Order Date,    |   Date on which the order was placed                  |
# |    Ship Date,     |   Date on which the shipment was made                 |
# |    Ship Mode,     |   The mode of shipment (category)                     |
# |    Customer ID,   |   The unique ID of the customer                       |
# |    Customer Name, |   Name of the customer                                |
# |    Segment,       |   The market segment to which the customer belongs    |
# |    City,          |   City of the delivery address                        |
# |    State,         |   State of the delivery address                       |
# |    Country,       |   Country of the delivery address                     |
# |    Postal Code,   |   Postal code of the delivery address                 |
# |    Market,        |   Market segment to which the customer belongs        |
# |    Region,        |   Geographical region of the customer                 |
# |    Product ID,    |   Unique ID of the product                            |
# |    Category,      |   Category of the product                             |
# |    Sub-Category,  |   Sub-category of the product                         |
# |    Product Name,  |   Name of the product                                 |
# |    Sales,         |   Total sales value of the transaction                |
# |    Quantity,      |   Quantity of the product ordered                     |
# |    Discount,      |   Discount percentage offered on the product          |
# |    Profit,        |   Profit made on the transaction                      |
# |    Shipping Cost, |   Shipping cost incured on the transaction            |
# |    Order Priority,|   Priority assigned to the order                      |
# |___________________|_______________________________________________________|



# *************************************************************************
#                LOAD LIBRARIES ----
# *************************************************************************

library(MASS)
library(car)
library(plyr)
library(lubridate)
library(raster)
library(zoo)
library(stats)
library(dplyr)
library(forecast)
require(graphics)
library(ggplot2)



# *************************************************************************
#                SET WORKING DIRECTORY ----
# *************************************************************************
setwd("~/R")
setwd("C:/Users/atchirc/SLAPG/PGDDA/Course4_PredictiveAnalytics_II/CaseStudy/code")


# *************************************************************************
#                LOAD DATASET ----
# *************************************************************************

superstore_full<- read.csv("../input/GlobalSuperstore.csv", stringsAsFactors = FALSE)

str(superstore_full)
summary(superstore_full)



# *************************************************************************
#                DATA CLEANING ----
# *************************************************************************

# . . . . De-Duplication ----
sum(duplicated(superstore_full))      #There are no duplicates in the data


# . . . . Missing Values ----
sum(is.na(superstore_full))           #41296 missing values

summary(superstore_full)              #Clearly all the NA values are in Postal.Code column


# . . . . Data Preparation ----

#Remove the Postal.Code column as we cannot do anything to treat it. 
# Also since we have all other region data we dont need it
superstore_full<-superstore_full[,-12]

#Since we have the Order.Id & Order.Dates we don't need the Row.Id column
superstore_full<-superstore_full[,-1]
str(superstore_full)

#The date columns (Order.Dates & Ship.Dates) have variable type 'char'.Change it to 'date'
superstore_full$Order.Date<- as.Date(superstore_full$Order.Date, "%d-%m-%Y")
superstore_full$Ship.Date<- as.Date(superstore_full$Ship.Date, "%d-%m-%Y")
str(superstore_full)              #They look perfect now




# *************************************************************************
#                DATA PREPARATION ----
# *************************************************************************

#Combine 2 columns to create Market Segment
summary(superstore_full)

superstore_full$Market.Segment <- paste(superstore_full$Market, superstore_full$Segment, sep = " ")

#Create another dataset for rest of the data prep & modelling
superstore<-superstore_full


# . . . . Aggregate Daily Data Over Month ----

#Add another column for Month.Year this will help us in aggregating monthly data
superstore$Year.Month <- format(as.Date(superstore$Order.Date), "%Y-%m")


# . . . . Create 21 Market segments ----

#Subset the data in 21 "Market Segments"
invisible(lapply(split(superstore, superstore$Market.Segment), function(x) {assign(paste0("Market.Segment", x$Market.Segment[1]), x, pos = .GlobalEnv)}))


#Aggregating the data for the Atribute: Sales, Profit & Qty compared on PI & Coeff of Variation
PI_1 <- superstore %>%                                                    # Use dplyr for Aggregation
  group_by(Market.Segment) %>% 
  summarise(., sum(Sales),                           # Total Sales Per Segment Per Market
            sum(Profit),                          # Total Profit Per segment Per Market
            sum(Quantity),                        # Total Quantity per segment per Market
            sd(Profit)*100/mean(Profit),              # Profit Co-efficient of Variation
            sum(Sales)/(sum(Sales)-sum(Profit)))   # Profitability Index, (selling cost/actual cost)



#Aggregate the Data monthly on each atribute
PI_2 <- superstore %>%                                                    # Use dplyr for Aggregation
  group_by(Market.Segment, Year.Month) %>% 
  summarise(., sum(Sales),                           # Total Sales Per Segment Per Market
            sum(Profit),                          # Total Profit Per segment Per Market
            sum(Quantity),                        # Total Quantity per segment per Market
            sd(Profit)*100/mean(Profit),              # Profit Co-efficient of Variation
            sum(Sales)/(sum(Sales)-sum(Profit)))   # Profitability Index, (selling cost/actual cost)


View(PI_2)

# Exploratory DATA Analysis -----

# . . . . Categorical ----

pa_segments <- superstore[,c("Profit","Sales","Market","Segment","Quantity")] %>%   # Use dplyr for Aggregation
  group_by(Market,Segment) %>% 
  dplyr::summarise(., sum(Sales),                           # Total Sales Per Segment Per Market
                   sum(Profit)                          # Total Profit Per segment Per Market
  )

colnames(pa_segments) = c("Market","Segment","Sales","Profit")

ggplot(pa_segments, aes( Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")



#Since data for Canada market is not available for all the 4 years (48 months),
#so we have ignored Canada from Top 5 Mkt Segments

#Top 5 Mkt Segments based on CoV are:
# 1. APAC Consumer
# 2. APAC Home Office
# 3. APAC Corporate
# 4. EU Consumer
# 5. EU Corporate


#Subsetting the PI_2 to these 5 market segments
top_5 <- subset(PI_2, Market.Segment == "EU Consumer" | Market.Segment == "EU Corporate" | 
                  Market.Segment == "APAC Home Office" | Market.Segment == "APAC Consumer" | 
                  Market.Segment == "APAC Corporate")
str(top_5)

#Change column names
names(top_5) <- c("Mkt_Segment", "Yr_Month", "Sales", "Profit", "Quantity", "Coeff_of_Var", "PI")


#Subset the data in Top 5 "Market Segments"
invisible(lapply(split(top_5, top_5$Mkt_Segment), function(x) {assign(paste0("Mkt_Segment", x$Mkt_Segment[1]), x, pos = .GlobalEnv)}))





# *************************************************************************
#                MODELING ----
# *************************************************************************

 
APAC_Consumer <- `Mkt_SegmentAPAC Consumer`[,c(2:5)]
APAC_Corporate <- `Mkt_SegmentAPAC Corporate`[,c(2:5)]
APAC_HO <- `Mkt_SegmentAPAC Home Office`[,c(2:5)]
EU_Consumer <- `Mkt_SegmentEU Consumer`[,c(2:5)]
EU_Corporate <- `Mkt_SegmentEU Corporate`[,c(2:5)]



# **************************************************************
#                 APAC CONSUMER ----
# **************************************************************

# . . . . APAC_consumer_sales ----

#Conversion of APAC sales data to time series..
APAC_consumer_sales_ts <- ts(APAC_Consumer$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_consumer_sales_ts)


# Smootheing of the time series
Smoothed_APAC_consumer_sales <- stats::filter(APAC_consumer_sales_ts,                # Here had to use stats::filter due tp plyr issue#
                                              filter=rep(1/3,3,method='convolution',
                                                         sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_consumer_sales_df <- data.frame(cbind(APAC_Consumer$Yr_Month,
                                           Smoothed_APAC_consumer_sales))

#Renaming the coloumns
colnames(APAC_consumer_sales_df) <- c("month","sales")

APAC_consumer_sales_df$sales <- as.numeric(as.character((APAC_consumer_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_consumer_sales_df$sales[3] - APAC_consumer_sales_df$sales[2]
APAC_consumer_sales_df$sales[1] <- APAC_consumer_sales_df$sales[2]-diff_1

diff_2 <- APAC_consumer_sales_df$sales[46] - APAC_consumer_sales_df$sales[45]
APAC_consumer_sales_df$sales[47] <- APAC_consumer_sales_df$sales[46]+ diff_2

diff_3 <- APAC_consumer_sales_df$sales[47] - APAC_consumer_sales_df$sales[46]
APAC_consumer_sales_df$sales[48] <- APAC_consumer_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_APAC_consumer_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_consumer_sales.ts <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                             end=c(2014,12))

APAC_Consumer_sales_hw <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                              end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Consumer_sales_hw)
plot(APAC_Consumer_sales_hw)
HoltWinters(APAC_Consumer_sales_hw)$fitted
APAC_Consumer_sales_hw_hw<-HoltWinters(APAC_Consumer_sales_hw)
plot(APAC_Consumer_sales_hw)
lines(predict(APAC_Consumer_sales_hw_hw,n.ahead=48),col=2)
predict(APAC_Consumer_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
# Jan      Feb      Mar      Apr      May      Jun
# 74889.15 69135.22 65191.81 69070.63 75428.15 72743.61


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_consumer_sales.ts)-ntest
train.ts <- window(APAC_consumer_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy

# FOR LINEAR MODEL MAPE IS 10.69


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=0 or 1 as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 200 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)

# from the summaries of both the models using pdq of 2,0,1 was giving a better MAPE vale ( MAPE =218 vs MAPE=190)

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_consumer_combined_accuracy

#Accuracy of the model decreased. Mape is 9.9


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 25.74
#  The Linear model is better than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(APAC_consumer_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales

train.lm.model <- tslm(APAC_consumer_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")


# . . . . APAC_consumer_Quantity ----

#Conversion of APAC qunatity data to time series..
APAC_consumer_qunatity_ts <- ts(APAC_Consumer$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_consumer_qunatity_ts)


#Smootheing of the time series

Smoothed_APAC_consumer_quantity <- stats::filter(APAC_consumer_qunatity_ts,                # Here had to use stats::filter due tp plyr issue#
                                                 filter=rep(1/3,3,method='convolution',
                                                            sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_consumer_quantity_df <- data.frame(cbind(APAC_Consumer$Yr_Month,
                                              Smoothed_APAC_consumer_quantity))

##Renaming the coloumns
colnames(APAC_consumer_quantity_df) <- c("month","quantity")

APAC_consumer_quantity_df$quantity <- as.numeric(as.character((APAC_consumer_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_consumer_quantity_df$quantity[3] - APAC_consumer_quantity_df$quantity[2]
APAC_consumer_quantity_df$quantity[1] <- APAC_consumer_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_consumer_quantity_df$quantity[46] - APAC_consumer_quantity_df$quantity[45]
APAC_consumer_quantity_df$quantity[47] <- APAC_consumer_quantity_df$quantity[46]+ diff_2

diff_3 <- APAC_consumer_quantity_df$quantity[47] - APAC_consumer_quantity_df$quantity[46]
APAC_consumer_quantity_df$quantity[48] <- APAC_consumer_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_APAC_consumer_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_consumer_quantity.ts <- ts(APAC_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                                end=c(2014,12))
APAC_Consumer_quantity_hw <- ts(APAC_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                                end=c(2014,12))

APAC_consumer_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Consumer_quantity_hw)
plot(APAC_Consumer_quantity_hw)
HoltWinters(APAC_Consumer_quantity_hw)$fitted
APAC_Consumer_quantity_hw_hw<-HoltWinters(APAC_Consumer_quantity_hw)
plot(APAC_Consumer_quantity_hw)
lines(predict(APAC_Consumer_quantity_hw_hw,n.ahead=48),col=2)
predict(APAC_Consumer_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#      Jan      Feb      Mar      Apr      May      Jun
# 2015 713.4561 616.5936 582.6012 646.8259 751.6196 740.7689


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_consumer_quantity.ts)-ntest
train.ts <- window(APAC_consumer_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy
# FOR LINEAR MODEL MAPE IS 10.69


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=3 as there are 3  significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 203 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,3
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,3))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 144.6

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 190.00

# from the summaries of both the models using pdq of 2,0,3 was giving a better MAPE vale

#plot the ACF of AR(2,0,3)
acf(train.res.arima.pred1$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred1$mean),valid.ts)
APAC_consumer_combined_accuracy

#Accuracy of the model is 11.09, so Linear model is beter


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 25.34
#  so manual linear model is better than auto arima model


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it
# autoarima_combined <- auto.arima(APAC_consumer_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales
train.lm.model <- tslm(APAC_consumer_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

APAC_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_consumer_final_df) <- c("Sales","Qty")





# **************************************************************
#                 APAC CORPORATE ----
# **************************************************************

# **************************************************************
#                 APAC Corporate ----
# **************************************************************

# . . . . APAC_Corporate_sales ----

#Conversion of APAC sales data to time series..
APAC_Corporate_sales_ts <- ts(APAC_Corporate$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Corporate_sales_ts)


# Smootheing of the time series
Smoothed_APAC_Corporate_sales <- stats::filter(APAC_Corporate_sales_ts,                # Here had to use stats::filter due tp plyr issue#
                                               filter=rep(1/3,3,method='convolution',
                                                          sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_Corporate_sales_df <- data.frame(cbind(APAC_Corporate$Yr_Month,
                                            Smoothed_APAC_Corporate_sales))

#Renaming the coloumns
colnames(APAC_Corporate_sales_df) <- c("month","sales")

APAC_Corporate_sales_df$sales <- as.numeric(as.character((APAC_Corporate_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_Corporate_sales_df$sales[3] - APAC_Corporate_sales_df$sales[2]
APAC_Corporate_sales_df$sales[1] <- APAC_Corporate_sales_df$sales[2]-diff_1

diff_2 <- APAC_Corporate_sales_df$sales[46] - APAC_Corporate_sales_df$sales[45]
APAC_Corporate_sales_df$sales[47] <- APAC_Corporate_sales_df$sales[46]+ diff_2

diff_3 <- APAC_Corporate_sales_df$sales[47] - APAC_Corporate_sales_df$sales[46]
APAC_Corporate_sales_df$sales[48] <- APAC_Corporate_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_APAC_Corporate_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_Corporate_sales.ts <- ts(APAC_Corporate_sales_df$sales,frequency=12,start=c(2011,1),
                              end=c(2014,12))

APAC_Corporate_sales_hw <- ts(APAC_Corporate_sales_df$sales,frequency=12,start=c(2011,1),
                              end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Corporate_sales_hw)
plot(APAC_Corporate_sales_hw)
HoltWinters(APAC_Corporate_sales_hw)$fitted
APAC_Corporate_sales_hw_hw<-HoltWinters(APAC_Corporate_sales_hw)
plot(APAC_Corporate_sales_hw)
lines(predict(APAC_Corporate_sales_hw_hw,n.ahead=48),col=2)
predict(APAC_Corporate_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#      Jan      Feb      Mar      Apr      May      Jun
# 2015 37328.61 27477.05 23498.11 28989.13 35732.73 34674.85


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_Corporate_sales.ts)-ntest
train.ts <- window(APAC_Corporate_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Corporate_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_Corporate_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
APAC_Corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_Corporate_accuracy

# FOR LINEAR MODEL MAPE IS 11.44


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=0 or 1 as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 200 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                         #MAPE - 318.9

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)                        # MAPE- 174.9

# from the summaries of both the models using pdq of 2,0,1 was giving a better MAPE vale ( MAPE =318 vs MAPE=174)

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
APAC_Corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_Corporate_combined_accuracy

#Accuracy of the model decreased. Mape is 13.44.
# The manual linear model is better 


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 18.4
#  The Linear model is better than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(APAC_Corporate_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales

train.lm.model <- tslm(APAC_Corporate_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")


# . . . . APAC_Corporate_Quantity ----

#Conversion of APAC qunatity data to time series..
APAC_Corporate_qunatity_ts <- ts(APAC_Corporate$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Corporate_qunatity_ts)


#Smootheing of the time series

Smoothed_APAC_Corporate_quantity <- stats::filter(APAC_Corporate_qunatity_ts,                # Here had to use stats::filter due tp plyr issue#
                                                  filter=rep(1/3,3,method='convolution',
                                                             sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_Corporate_quantity_df <- data.frame(cbind(APAC_Corporate$Yr_Month,
                                               Smoothed_APAC_Corporate_quantity))

##Renaming the coloumns
colnames(APAC_Corporate_quantity_df) <- c("month","quantity")

APAC_Corporate_quantity_df$quantity <- as.numeric(as.character((APAC_Corporate_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_Corporate_quantity_df$quantity[3] - APAC_Corporate_quantity_df$quantity[2]
APAC_Corporate_quantity_df$quantity[1] <- APAC_Corporate_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_Corporate_quantity_df$quantity[46] - APAC_Corporate_quantity_df$quantity[45]
APAC_Corporate_quantity_df$quantity[47] <- APAC_Corporate_quantity_df$quantity[46]+ diff_2

diff_3 <- APAC_Corporate_quantity_df$quantity[47] - APAC_Corporate_quantity_df$quantity[46]
APAC_Corporate_quantity_df$quantity[48] <- APAC_Corporate_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_APAC_Corporate_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_Corporate_quantity.ts <- ts(APAC_Corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                                 end=c(2014,12))
APAC_Corporate_quantity_hw <- ts(APAC_Corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                                 end=c(2014,12))

APAC_Corporate_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Corporate_quantity_hw)
plot(APAC_Corporate_quantity_hw)
HoltWinters(APAC_Corporate_quantity_hw)$fitted
APAC_Corporate_quantity_hw_hw<-HoltWinters(APAC_Corporate_quantity_hw)
plot(APAC_Corporate_quantity_hw)
lines(predict(APAC_Corporate_quantity_hw_hw,n.ahead=48),col=2)
predict(APAC_Corporate_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#      Jan      Feb      Mar      Apr      May      Jun
# 2015 346.3824 251.6362 262.8900 330.3660 407.8282 381.4570


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_Corporate_quantity.ts)-ntest
train.ts <- window(APAC_Corporate_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Corporate_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_Corporate_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
APAC_Corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_Corporate_accuracy
# FOR LINEAR MODEL MAPE IS 9.69


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=1 as there are 1 significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 203 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,2))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 144.6

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 190.00

# from the summaries of both the models using pdq of 2,0,1 curve fitting even if the MAPE VALUE IS Abit higher

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
APAC_Corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_Corporate_combined_accuracy

#Accuracy of the model is 11.12, so Linear model is beter


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 9.72
#  so manual linear model is better than auto arima model


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it
# autoarima_combined <- auto.arima(APAC_Corporate_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales
train.lm.model <- tslm(APAC_Corporate_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

APAC_Corporate_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_Corporate_final_df) <- c("Sales","Qty")




#***********************************************************
#            APAC HOME OFFICE ----
#***********************************************************


# . . . . APAC_HO_sales ----

#Conversion of APAC sales data to time series..
APAC_HO_sales_ts <- ts(APAC_HO$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_HO_sales_ts)


# Smootheing of the time series
Smoothed_APAC_HO_sales <- stats::filter(APAC_HO_sales_ts,                # Here had to use stats::filter due tp plyr issue#
                                        filter=rep(1/3,3,method='convolution',
                                                   sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_HO_sales_df <- data.frame(cbind(APAC_HO$Yr_Month,
                                     Smoothed_APAC_HO_sales))

#Renaming the coloumns
colnames(APAC_HO_sales_df) <- c("month","sales")

APAC_HO_sales_df$sales <- as.numeric(as.character((APAC_HO_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_HO_sales_df$sales[3] - APAC_HO_sales_df$sales[2]
APAC_HO_sales_df$sales[1] <- APAC_HO_sales_df$sales[2]-diff_1

diff_2 <- APAC_HO_sales_df$sales[46] - APAC_HO_sales_df$sales[45]
APAC_HO_sales_df$sales[47] <- APAC_HO_sales_df$sales[46]+ diff_2

diff_3 <- APAC_HO_sales_df$sales[47] - APAC_HO_sales_df$sales[46]
APAC_HO_sales_df$sales[48] <- APAC_HO_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_APAC_HO_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_HO_sales.ts <- ts(APAC_HO_sales_df$sales,frequency=12,start=c(2011,1),
                       end=c(2014,12))

APAC_HO_sales_hw <- ts(APAC_HO_sales_df$sales,frequency=12,start=c(2011,1),
                       end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_HO_sales_hw)
plot(APAC_HO_sales_hw)
HoltWinters(APAC_HO_sales_hw)$fitted
APAC_HO_sales_hw_hw<-HoltWinters(APAC_HO_sales_hw)
plot(APAC_HO_sales_hw)
lines(predict(APAC_HO_sales_hw_hw,n.ahead=48),col=2)
predict(APAC_HO_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#     Jan      Feb      Mar      Apr      May      Jun
#2015 31088.93 32318.63 31331.00 33963.89 32053.45 33882.31


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_HO_sales.ts)-ntest
train.ts <- window(APAC_HO_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_HO_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_HO_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
APAC_HO_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_HO_accuracy

# FOR LINEAR MODEL MAPE IS  25.49


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=2 or 1 as there are 2 significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 201 and then one more model with pdq as 202, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,1
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                         #MAPE - 115.6

#Model AR and plot it with pdq = 2,0,2
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,2))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)                        # MAPE- 120.0

# from the summaries of both the models using pdq of 2,0,1 was giving a better MAPE vale ( MAPE =115 vs MAPE=120)

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred1$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
APAC_HO_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred1$mean),valid.ts)
APAC_HO_combined_accuracy

#Accuracy of the model decreased. Mape is 26.44.
# The manual linear model is a shade better 


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 46.4
#  The Linear model is better than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(APAC_HO_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales

train.lm.model <- tslm(APAC_HO_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")


# . . . . APAC_HO_Quantity ----

#Conversion of APAC qunatity data to time series..
APAC_HO_qunatity_ts <- ts(APAC_HO$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_HO_qunatity_ts)


#Smootheing of the time series

Smoothed_APAC_HO_quantity <- stats::filter(APAC_HO_qunatity_ts,                # Here had to use stats::filter due tp plyr issue#
                                           filter=rep(1/3,3,method='convolution',
                                                      sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_HO_quantity_df <- data.frame(cbind(APAC_HO$Yr_Month,
                                        Smoothed_APAC_HO_quantity))

##Renaming the coloumns
colnames(APAC_HO_quantity_df) <- c("month","quantity")

APAC_HO_quantity_df$quantity <- as.numeric(as.character((APAC_HO_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_HO_quantity_df$quantity[3] - APAC_HO_quantity_df$quantity[2]
APAC_HO_quantity_df$quantity[1] <- APAC_HO_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_HO_quantity_df$quantity[46] - APAC_HO_quantity_df$quantity[45]
APAC_HO_quantity_df$quantity[47] <- APAC_HO_quantity_df$quantity[46]+ diff_2

diff_3 <- APAC_HO_quantity_df$quantity[47] - APAC_HO_quantity_df$quantity[46]
APAC_HO_quantity_df$quantity[48] <- APAC_HO_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_APAC_HO_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_HO_quantity.ts <- ts(APAC_HO_quantity_df$quantity,frequency=12,start=c(2011,1),
                          end=c(2014,12))
APAC_HO_quantity_hw <- ts(APAC_HO_quantity_df$quantity,frequency=12,start=c(2011,1),
                          end=c(2014,12))

APAC_HO_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_HO_quantity_hw)
plot(APAC_HO_quantity_hw)
HoltWinters(APAC_HO_quantity_hw)$fitted
APAC_HO_quantity_hw_hw<-HoltWinters(APAC_HO_quantity_hw)
plot(APAC_HO_quantity_hw)
lines(predict(APAC_HO_quantity_hw_hw,n.ahead=48),col=2)
predict(APAC_HO_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#     Jan      Feb      Mar      Apr      May      Jun
#2015 350.2486 368.9342 355.4030 396.6651 383.0083 396.4567


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_HO_quantity.ts)-ntest
train.ts <- window(APAC_HO_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_HO_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_HO_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
APAC_HO_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_HO_accuracy
# FOR LINEAR MODEL MAPE IS 21.1


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 3 as there are 3 significant peaks in the PACF plot (AR=2)
# q= 2 as there are atleast 2 significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 303 and then one more model with pdq as 302, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 3,0,3
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(3,0,3))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 144.6

#Model AR and plot it with pdq = 3,0,2
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(3,0,2))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 190.00

# from the summaries of both the models using pdq of 3,0,2 curve fitting even though the MAPE Stats are same for both

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
APAC_HO_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_HO_combined_accuracy

#Accuracy of the model is 21.1, its equivalent to the liner model


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 24.02
#  so manual linear model is better than auto arima model


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it
# autoarima_combined <- auto.arima(APAC_HO_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales
train.lm.model <- tslm(APAC_HO_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

APAC_HO_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_HO_final_df) <- c("Sales","Qty")




# *****************************************************************
#       EU CONSUMER ----
# *****************************************************************

# . . . . EU_consumer_sales ----

#Conversion of EU sales data to time series..
EU_consumer_sales_ts <- ts(EU_Consumer$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_consumer_sales_ts)


# Smootheing of the time series
Smoothed_EU_consumer_sales <- stats::filter(EU_consumer_sales_ts,                # Here had to use stats::filter due tp plyr issue#
                                            filter=rep(1/3,3,method='convolution',
                                                       sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
EU_consumer_sales_df <- data.frame(cbind(EU_Consumer$Yr_Month,
                                         Smoothed_EU_consumer_sales))

#Renaming the coloumns
colnames(EU_consumer_sales_df) <- c("month","sales")

EU_consumer_sales_df$sales <- as.numeric(as.character((EU_consumer_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- EU_consumer_sales_df$sales[3] - EU_consumer_sales_df$sales[2]
EU_consumer_sales_df$sales[1] <- EU_consumer_sales_df$sales[2]-diff_1

diff_2 <- EU_consumer_sales_df$sales[46] - EU_consumer_sales_df$sales[45]
EU_consumer_sales_df$sales[47] <- EU_consumer_sales_df$sales[46]+ diff_2

diff_3 <- EU_consumer_sales_df$sales[47] - EU_consumer_sales_df$sales[46]
EU_consumer_sales_df$sales[48] <- EU_consumer_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_EU_consumer_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
EU_consumer_sales.ts <- ts(EU_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                           end=c(2014,12))

EU_Consumer_sales_hw <- ts(EU_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                           end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(EU_Consumer_sales_hw)
plot(EU_Consumer_sales_hw)
HoltWinters(EU_Consumer_sales_hw)$fitted
EU_Consumer_sales_hw_hw<-HoltWinters(EU_Consumer_sales_hw)
plot(EU_Consumer_sales_hw)
lines(predict(EU_Consumer_sales_hw_hw,n.ahead=48),col=2)
predict(EU_Consumer_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#     Jan      Feb      Mar      Apr      May      Jun
#2015 39765.19 34295.02 34971.50 40213.98 47569.97 48272.34


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(EU_consumer_sales.ts)-ntest
train.ts <- window(EU_consumer_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_consumer_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_EU_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy

# FOR LINEAR MODEL MAPE IS 9.26


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 1 as there is only one significant peak in the PACF plot (AR=1)
# q=1 or 2 as there is only 2 significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 101 and then one more model with pdq as 102, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 1,0,1
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(1,0,1))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                            # MAPE - 201

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(1,0,2))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)                            # MAPE 167

# from the summaries of both the models using pdq of 1,0,2 was giving a better MAPE vale ( MAPE =201 vs MAPE=167)

#plot the ACF of AR(1,0,2)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
EU_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
EU_consumer_combined_accuracy

#Accuracy of the model decreased. Mape is 10.57, so the linear model is better


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 22.91
#  The Linear model is better than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(EU_consumer_sales)
# future_forecast_EU_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_EU_sales

train.lm.model <- tslm(EU_consumer_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")


# . . . . EU_consumer_Quantity ----

#Conversion of EU qunatity data to time series..
EU_consumer_qunatity_ts <- ts(EU_Consumer$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_consumer_qunatity_ts)


#Smootheing of the time series

Smoothed_EU_consumer_quantity <- stats::filter(EU_consumer_qunatity_ts,                # Here had to use stats::filter due tp plyr issue#
                                               filter=rep(1/3,3,method='convolution',
                                                          sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
EU_consumer_quantity_df <- data.frame(cbind(EU_Consumer$Yr_Month,
                                            Smoothed_EU_consumer_quantity))

##Renaming the coloumns
colnames(EU_consumer_quantity_df) <- c("month","quantity")

EU_consumer_quantity_df$quantity <- as.numeric(as.character((EU_consumer_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- EU_consumer_quantity_df$quantity[3] - EU_consumer_quantity_df$quantity[2]
EU_consumer_quantity_df$quantity[1] <- EU_consumer_quantity_df$quantity[2]-diff_1

diff_2 <- EU_consumer_quantity_df$quantity[46] - EU_consumer_quantity_df$quantity[45]
EU_consumer_quantity_df$quantity[47] <- EU_consumer_quantity_df$quantity[46]+ diff_2

diff_3 <- EU_consumer_quantity_df$quantity[47] - EU_consumer_quantity_df$quantity[46]
EU_consumer_quantity_df$quantity[48] <- EU_consumer_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_EU_consumer_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
EU_consumer_quantity.ts <- ts(EU_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                              end=c(2014,12))
EU_Consumer_quantity_hw <- ts(EU_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                              end=c(2014,12))

EU_consumer_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(EU_Consumer_quantity_hw)
plot(EU_Consumer_quantity_hw)
HoltWinters(EU_Consumer_quantity_hw)$fitted
EU_Consumer_quantity_hw_hw<-HoltWinters(EU_Consumer_quantity_hw)
plot(EU_Consumer_quantity_hw)
lines(predict(EU_Consumer_quantity_hw_hw,n.ahead=48),col=2)
predict(EU_Consumer_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#     Jan      Feb      Mar      Apr      May      Jun
#2015 531.9166 515.5751 536.4693 565.9137 604.7563 600.9954


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(EU_consumer_quantity.ts)-ntest
train.ts <- window(EU_consumer_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_consumer_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_EU_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy
# FOR LINEAR MODEL MAPE IS 10.33


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are two singnificant peaks in the PACF (AR=2)
# q=1 as there is only one  significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 200 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 138.6

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 141.00

# from the summaries of both the models though the MAPE value is less of pdq of 200, 201 gives a better graph

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
EU_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
EU_consumer_combined_accuracy

#Accuracy of the model is 10.63, so Linear model is a shade better


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 13.34
#  so manual linear model is better than auto arima model


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it
# autoarima_combined <- auto.arima(EU_consumer_sales)
# future_forecast_EU_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_EU_sales
train.lm.model <- tslm(EU_consumer_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

EU_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(EU_consumer_final_df) <- c("Sales","Qty")
# ****************************************************
#               EU CORPORATE ----
# ****************************************************


# . . . . EU_Corporate_sales ----

#Conversion of EU sales data to time series..
EU_Corporate_sales_ts <- ts(EU_Corporate$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Corporate_sales_ts)


# Smootheing of the time series
Smoothed_EU_Corporate_sales <- stats::filter(EU_Corporate_sales_ts,                # Here had to use stats::filter due tp plyr issue#
                                             filter=rep(1/3,3,method='convolution',
                                                        sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
EU_Corporate_sales_df <- data.frame(cbind(EU_Corporate$Yr_Month,
                                          Smoothed_EU_Corporate_sales))

#Renaming the coloumns
colnames(EU_Corporate_sales_df) <- c("month","sales")

EU_Corporate_sales_df$sales <- as.numeric(as.character((EU_Corporate_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- EU_Corporate_sales_df$sales[3] - EU_Corporate_sales_df$sales[2]
EU_Corporate_sales_df$sales[1] <- EU_Corporate_sales_df$sales[2]-diff_1

diff_2 <- EU_Corporate_sales_df$sales[46] - EU_Corporate_sales_df$sales[45]
EU_Corporate_sales_df$sales[47] <- EU_Corporate_sales_df$sales[46]+ diff_2

diff_3 <- EU_Corporate_sales_df$sales[47] - EU_Corporate_sales_df$sales[46]
EU_Corporate_sales_df$sales[48] <- EU_Corporate_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_EU_Corporate_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
EU_Corporate_sales.ts <- ts(EU_Corporate_sales_df$sales,frequency=12,start=c(2011,1),
                            end=c(2014,12))

EU_Corporate_sales_hw <- ts(EU_Corporate_sales_df$sales,frequency=12,start=c(2011,1),
                            end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(EU_Corporate_sales_hw)
plot(EU_Corporate_sales_hw)
HoltWinters(EU_Corporate_sales_hw)$fitted
EU_Corporate_sales_hw_hw<-HoltWinters(EU_Corporate_sales_hw)
plot(EU_Corporate_sales_hw)
lines(predict(EU_Corporate_sales_hw_hw,n.ahead=48),col=2)
predict(EU_Corporate_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#           Jan      Feb      Mar      Apr      May      Jun
#      2015 27843.43 26186.66 27638.48 25466.21 32985.68 31671.05


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(EU_Corporate_sales.ts)-ntest
train.ts <- window(EU_Corporate_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Corporate_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_EU_Corporate_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
EU_Corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_Corporate_accuracy

# FOR LINEAR MODEL MAPE IS 18.44


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 1 as there is one singnificant peak in the PACF plot (AR=1)
# q=0 or 1 as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 100 and then one more model with pdq as 001, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq =1,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(1,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                         #MAPE - 141.3

#Model AR and plot it with pdq = 0,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(1,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)                        # MAPE- 142.3

# from the summaries of both the models using pdq of 1,0,0 was giving a better MAPE vale ( MAPE =141 vs MAPE=142)

#plot the ACF of AR(1,0,0)
acf(train.res.arima.pred1$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
EU_Corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred1$mean),valid.ts)
EU_Corporate_combined_accuracy

#Accuracy of the model decreased. Mape is 19.76305.
# The manual linear model is better 


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 18.5
#  The Linear model is better shade than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(EU_Corporate_sales)
# future_forecast_EU_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_EU_sales

train.lm.model <- tslm(EU_Corporate_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")


# . . . . EU_Corporate_Quantity ----

#Conversion of EU qunatity data to time series..
EU_Corporate_qunatity_ts <- ts(EU_Corporate$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Corporate_qunatity_ts)


#Smootheing of the time series

Smoothed_EU_Corporate_quantity <- stats::filter(EU_Corporate_qunatity_ts,                # Here had to use stats::filter due tp plyr issue#
                                                filter=rep(1/3,3,method='convolution',
                                                           sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
EU_Corporate_quantity_df <- data.frame(cbind(EU_Corporate$Yr_Month,
                                             Smoothed_EU_Corporate_quantity))

##Renaming the coloumns
colnames(EU_Corporate_quantity_df) <- c("month","quantity")

EU_Corporate_quantity_df$quantity <- as.numeric(as.character((EU_Corporate_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- EU_Corporate_quantity_df$quantity[3] - EU_Corporate_quantity_df$quantity[2]
EU_Corporate_quantity_df$quantity[1] <- EU_Corporate_quantity_df$quantity[2]-diff_1

diff_2 <- EU_Corporate_quantity_df$quantity[46] - EU_Corporate_quantity_df$quantity[45]
EU_Corporate_quantity_df$quantity[47] <- EU_Corporate_quantity_df$quantity[46]+ diff_2

diff_3 <- EU_Corporate_quantity_df$quantity[47] - EU_Corporate_quantity_df$quantity[46]
EU_Corporate_quantity_df$quantity[48] <- EU_Corporate_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_EU_Corporate_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
EU_Corporate_quantity.ts <- ts(EU_Corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                               end=c(2014,12))
EU_Corporate_quantity_hw <- ts(EU_Corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                               end=c(2014,12))

EU_Corporate_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(EU_Corporate_quantity_hw)
plot(EU_Corporate_quantity_hw)
HoltWinters(EU_Corporate_quantity_hw)$fitted
EU_Corporate_quantity_hw_hw<-HoltWinters(EU_Corporate_quantity_hw)
plot(EU_Corporate_quantity_hw)
lines(predict(EU_Corporate_quantity_hw_hw,n.ahead=48),col=2)
predict(EU_Corporate_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#            Jan      Feb      Mar      Apr      May      Jun
#       2015 353.5463 341.7310 354.2060 331.8580 420.3798 390.1997


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(EU_Corporate_quantity.ts)-ntest
train.ts <- window(EU_Corporate_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Corporate_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_EU_Corporate_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',flty=2,col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
EU_Corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_Corporate_accuracy
# FOR LINEAR MODEL MAPE IS 13.4238


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 0r 3 as there are 2 significant peaks in the PACF plot (AR=2)
# q=3 as there are atleast 3 significant peaks in the ACF plot (MA3)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 202 and then one more model with pdq as 303, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,2
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,2))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 139.2

#Model AR and plot it with pdq = 3,0,3
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(3,0,3))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 139

# from the summaries of both the models using pdq of 3,0,3 curve fitting even if the MAPE VALUES are same

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
EU_Corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
EU_Corporate_combined_accuracy

#Accuracy of the model is 15.35606, so Linear model is beter


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 9.93
#  So autoarima model is better


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it

train.lm.model <- auto.arima(EU_Corporate_quantity.ts)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

EU_Corporate_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(EU_Corporate_final_df) <- c("Sales","Qty")

