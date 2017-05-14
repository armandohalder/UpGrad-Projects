# *****************************************************************************
#   Task:       TELECOM CHURN RATE CASE STUDY
#   Course:     PGDDA Course, IIIT- BANGALORE
#   Time Line:  October 2016
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
# Problem Statement:
#   You are working for the telecom company 'Firm X'. It has a customer base set 
# across the country. In a city 'Y', which is a significant revenue base for the 
# company, due to heavy marketing and promotion schemes by other companies, your 
# company is losing customers i.e. the customers are churning. Whether a customer 
# will churn or not will depend on data from the following three buckets:
#    Demographic Information
#    Services Availed by the customer
#    Overall Expenses
#
#   The data are provided in three separate data files given at the end of the 
# page. The aim is to automate the process of predicting if a customer would churn 
# or not and to find the factors affecting the churn. The collated data dictionary 
# for the variables in the 3 data frames is given below:
#  
#
#      _______________________________________________________________________________________________________
#     |         |                        |                                                                    |
#     |  S.No.	|  Variable Name 	       |    Meaning                                                         |
#     |_________|________________________|____________________________________________________________________|
#     |         |                        |                                                                    |
#     |   1.	  |   CustomerID 	         |   The unique ID of each customer                                   |
#     |   2.	  |   Gender 	             |   The gender of a person                                           |
#     |   3.	  |   SeniorCitizen	       |   Whether a customer can be classified as a senior citizen.        |
#     |   4.	  |   Partner 	           |   If a customer is married/ in a live-in relationship.             |
#     |   5.	  |   Dependents	         |   If a customer has dependents (children/ retired parents)         |
#     |   6.	  |   Tenure	             |   The time for which a customer has been using the service.        |
#     |   7.	  |   PhoneService  	     |   Whether a customer has a landline phone service along with       |
#     |         |                        |            the internet service.                                   |
#     |   8.	  |   MultipleLines	       |   Whether a customer has multiple lines of internet connectivity.  |
#     |   9.	  |   InternetService 	   |   The type of internet services chosen by the customer.            |
#     |   10.	  |   OnlineSecurity 	     |   Specifies if a customer has online security.                     |
#     |   11.	  |   OnlineBackup 	       |   Specifies if a customer has online backup.                       |
#     |   12.	  |   DeviceProtection 	   |   Specifies if a customer has opted for device protection.         |
#     |   13.	  |   TechSupport 	       |   Whether a customer has opted for tech support of not.            |
#     |   14.	  |   StreamingTV	         |   Whether a customer has an option of TV streaming.                |
#     |   15.	  |   StreamingMovies 	   |   Whether a customer has an option of Movie streaming.             |
#     |   16.	  |   Contract  	         |   The type of contract a customer has chosen.                      |
#     |   17.	  |   PaperlessBilling  	 |   Whether a customer has opted for paperless billing.              |
#     |   18.	  |   PaymentMethod 	     |   Specifies the method by which bills are paid.                    |
#     |   19.	  |   MonthlyCharges 	     |   Specifies the money paid by a customer each month.               |
#     |   20.	  |   TotalCharges 	       |   The total money paid by the customer to the company.             |
#     |   21.	  |   Churn  	             |   This is the target variable which specifies if a customer has    |
#     |         |                        |                churned or not.                                     |
#     |_________|________________________|____________________________________________________________________|
#
#
#  OBJECTIVE :
#  
#  You are required to develop predictive models using each of the 4 models namely 
#      --- K-NN, 
#      --- Naive Bayes, 
#      --- Logistic Regression
#      --- SVM
#
#  Note: Wherever required, set the VIF threshold to 2.  
#
# NOTE:
#  Please make sure the below points are to be followed strictly for evaluation purpose:
#  Store the collated dataset into "churn" object.
#  Divide the dataset into 70:30 ratio and set seed to 100 for the reference. 
#  It should be renamed as "train" and "test" respectively.



# *************************************************************************----
#                FLOW ----
# *************************************************************************----
#
#
#     ______________      ________________       _________________
#    |              |    |                |     |                 |
#    |  Customer    |    |  Churn Data    |     |   Internet      |
#    |    Data      |    |                |     |       Data      |
#    |______________|    |________________|     |_________________|
#          |                     |                       |
#          |                     |                       |
#          |                     V                       |
#          |         ___________________________         |
#          |        |                           |        |
#          |------->|   Merge Data and Store in | <------|
#                   |       "churn"             |     
#                   |___________________________|     
#                                | 
#                                V
#                    ________________________________         
#                   |   DATA CLEANING                |        
#                   |       Missing Values Treatment | 
#                   |       Correct Data Types       |     
#                   |________________________________|     
#                                | 
#                                V
#                    ________________________________         
#                   |   Exploratory DATA Analaysis   |        
#                   |       Categorical Vs Response  | 
#                   |       Numerical Vs Response    |     
#                   |________________________________|     
#                                | 
#                                V
#                    _________________________________________         
#                   |         Feature Engineering             |
#                   |          Create two versions            | 
#                   |                ______                   | 
#                   | ALL Variables |      |  ALL Variables   |
#                   |   to Numeric  |      |   To Categorical |
#                   | "churn_num"   |      |  "churn_cat"     |
#                   |_______________|      |__________________|     
#                             |                     |
#                             V                     V
#                      ________________       _________________
#                     |                |     |                 |
#                     |  Create Train  |     |   Create Train  |
#                     |   and Test     |     |    and Test     |
#                     |________________|     |_________________|
#                             |                     |
#                             V                     V
#                      ________________       ____________________
#                     |                |     |                    |
#                     |  Run KNN, SVM, |     |   Run Naive Bayes  |
#                     |   Logit        |     |                    |
#                     |________________|     |____________________|
#


# *************************************************************************----
#                LOAD LIBRARIES ----
# *************************************************************************----

library(MASS)
library(car)
library(e1071)      # svm
library(Hmisc)      # Describe-EDA
library(ROCR)       # ROC Curve
library(caret)      # confusion Matrix
library(class)      # knn algorithm
library(cowplot)    # Plot Grid
library(caTools)    # sample.split
library(class)      # knn function



# *************************************************************************----
#                PROCs ----
# *************************************************************************----

# Function to generate univariate plots for Numerical variables
# InPuts    :   DataFrame of Numerical Variables  
# OutPuts   :   Boxplot,HIstogram, Q-Q plots saves to ../output
#
# Flow   : 
#        1. re-format(collate) Numerical Variables
#        2.  -- Compute Skewness & Kurtosis
#            -- Compute slope & Intercept for Q-Q plot
#        3. Consolidated boxplots
#        4. Individual boxplots
#        5. Individual Histplots
#        6. Q-Q plots
#        7. Save plots
numerical_uni_plots  <- function(df,prefix) {
  nrows       <- nrow(df)
  df_num      <- data.frame()
  df_slp_int  <- data.frame()
  for (i in colnames(df)) {
    
    #   1. . . . Collate Variables ---
    df_num <- rbind(data.frame(var=rep(i,nrows),
                               val=df[[i]]
    ),
    df_num)
    
    #  2. . . . Caliculate slope & intercept ---
    # Find the slope and intercept of the line that passes through the 1st and 3rd
    # quartile of the normal q-q plot
    
    y     <- quantile(df[[i]], c(0.25, 0.75)) # Find the 1st and 3rd quartiles
    x     <- qnorm( c(0.25, 0.75))            # Find the matching normal values on the x-axis
    slope <- diff(y) / diff(x)                # Compute the line slope
    intr   <- y[1] - slope * x[1]             # Compute the line intercept
    mn     <- mean(df[[i]])                   # source for annotation co-ordinates
    sd     <- sd(df[[i]])                     # source for annotation co-ordinates
    df_slp_int <- rbind(data.frame(var=i,slp=slope,intcpt=intr,                   # slope & Intercept for qqline
                                   skw=sprintf("Sk=%.2f",skewness(df[[i]])),      # Skewness
                                   kurt=sprintf("Ku=%.2f",kurtosis(df[[i]])),     # Kurtosis
                                   sky=mn+sd,               # y-cord for skewness annotation
                                   kuy=mn-sd),              # y-cord for Kurtosis Annotation
                        df_slp_int)
  }
  
  df_num$var <- as.factor(df_num$var) # convert variables to factors
  
  # 3. . . . Combined Boxplot ---
  pcb <- ggplot(df_num,
                aes(var,val)
  )
  pcb <- pcb + geom_boxplot(colour="blue",
                            outlier.color = "red"
  )
  # pcb <- pcb + stat_summary(fun.data = give.n, geom = "text") 
  pcb <- pcb + ggtitle("BOXPLOTs (Numerical variables)")
  pcb <- pcb + theme(text=element_text(size = 10),          # title, labels
                     axis.text.x=element_text(angle = 45,   
                                              vjust = 0.5), # rotate & align x-axis labels
                     axis.text = element_text(size = 8)     # axis tick labels
  )
  pcb <- pcb + labs(x="",y="")
  
  
  # 4. . . . Individual Boxplots ---
  pib <- ggplot(df_num,
                aes(val,val)
  )
  pib <- pib + geom_boxplot(colour="blue",
                            outlier.colour = "red")
  pib <- pib + facet_wrap(~var,
                          scales = "free")
  pib <- pib + ggtitle(" BOXPLOTs (Numerical Variables) ") 
  pib <- pib + theme(axis.text.x=element_text(angle = 90), # x-axis labels rotated 90deg
                     axis.text=element_text(size = 8),     # axis tick labels
                     text=element_text(size = 10)          # title, labels
  )
  pib <- pib + labs(x="",y="")
  
  # 5. . . . Individual Histplots ---
  pih <- ggplot(df_num,
                aes(val)
  )
  pih <- pih + geom_histogram(fill="darkgreen",aes(y=..density..))
  pih <- pih + geom_density(colour="darkred")
  pih <- pih + facet_wrap(~var,
                          scales = "free")
  pih <- pih + ggtitle("DISTRIBUTION (Numerical Variables) ") 
  pih <- pih + theme(axis.text.x=element_text(angle = 90), # x-axis labels rotated 90deg
                     axis.text=element_text(size = 8),     # axis tick labels
                     text=element_text(size = 10)          # title, labels
  )
  pih <- pih + geom_text(data = df_slp_int, 
                         aes(label=skw), 
                         x=Inf,
                         y=Inf,
                         hjust = 1.2,
                         vjust = 2.2,
                         parse = T,
                         size=3)                          # skew annotation
  pih <- pih + geom_text(data = df_slp_int, 
                         aes(label=kurt),
                         x=Inf,
                         y=Inf,
                         hjust = 1.2,
                         vjust = 1.2,
                         parse = T,
                         size=3)                          # Kurt annotation
  
  pih <- pih + labs(x="",y="")
  
  
  # 6. . . . Combined Q-Q plots ---
  pqq <- ggplot(df_num) + stat_qq(aes(sample=val),
                                  color="red")            # qqplot 
  pqq <- pqq + facet_wrap(~var,
                          scales = "free")                # facet_wrap around Var
  pqq <- pqq + geom_abline(data = df_slp_int,
                           aes(intercept=intcpt,slope=slp),
                           color="blue")                  # qqline 
  pqq <- pqq + geom_text(data = df_slp_int, 
                         aes(label=skw), 
                         x=-Inf,
                         y=Inf,
                         hjust = -0.2,
                         vjust = 2.2,
                         parse = T,
                         size=3)                          # skew annotation
  pqq <- pqq + geom_text(data = df_slp_int, 
                         aes(label=kurt),
                         x=-Inf,
                         y=Inf,
                         hjust = -0.2,
                         vjust = 1.2,
                         parse = T,
                         size=3)                          # Kurt annotation
  pqq <- pqq + theme(text=element_text(size=10),          # title, labels
                     axis.text=element_text(size = 8)     # axis tick labels
  )
  pqq <- pqq + ggtitle("Q-Q Plots (Numerical variables)")
  pqq <- pqq + labs(x="",y="")
  
  
  # 7. . . . Save Plots ---
  # pdf("../output/univariate_numerical.pdf",paper = 'USr',width = 11)
  png(filename = paste("../output/",prefix,"_univariate_numerical%02d.png",sep = ""), 
      width = 11.69, 
      height = 8.27, 
      units = "in",
      res = 288)
  # par(mfrow=c(3,1))  # Not Working
  print(plot_grid(pcb,pib))
  print(plot_grid(pib,pih))
  print(pqq)
  dev.off()
  
  # can't find a way to impose boxplot on histogram
}



# Plots MOdel performance, provided model and ref.labels
#  mdl -->  Model
#  ref -->  reference labels
#  clk -->  Line Colour
#  ad  -->  Should performance curve to be added to existing Plot
plot_model_perf <- function(mdl,ref,clr,ad) {
  pred_prob <- predict(mdl, type = "response")
  mdl_score <- prediction(pred_prob, ref)
  perf      <- performance(mdl_score, "tpr", "fpr")
  plot(perf,col=clr,add=ad)
}



# Greps Model R2 Values
getModelR2 <- function(mdl) {
  mdl_sumry <- capture.output(summary(mdl))
  print(mdl_sumry[grepl("R-squared",mdl_sumry)])
}



# Extracts Coefficients from Model Summary
mdlsmry2df <- function(smry) {
  # extract Coefficints sections
  # df_coeff <- data.frame()
  start_coeff <- grep("Intercept",smry)         # starting coeff
  end_coeff   <- grep("---",smry)               # end coeff
  coeff       <- smry[start_coeff:end_coeff-1]  # extract coeff
  coeff       <- gsub("< ","<",coeff[-1])       # clean-up
  #df_coeff    <- as.data.frame(strsplit(coeff,"\\s+"),ncol=6)  # create dataFrame
  # Create unqual list of list to DataFrame 
  coeff      <- strsplit(coeff,"\\s+")
  df_coeff   <- as.data.frame(do.call(rbind,lapply(coeff,'length<-',6)))  
  colnames(df_coeff) <- c("var","Estimate","Std.Error",
                          "t-value","Pr(>|t|)","Significance") # add proper colnames
  return(df_coeff)
}



# function to consolidate model summary and model vif,
# shows merged output as table
# Inputs   : linear model
# Outputs  :  model summary and vif returned as df
model_sumry_vif <- function(mdl) {
  
  # Model VIF
  print("....Generating Model VIF ....")
  mdl_vif   <- vif(mdl)
  df_vif    <-  data.frame(var=names(mdl_vif),
                           vif=as.numeric(mdl_vif))
  
  # Model Summary
  print("....Generating Model Summary ....")
  mdl_sumry <- capture.output(summary(mdl))
  df_sumry  <- mdlsmry2df(mdl_sumry)
  
  # Merger Summary and VIF  by variable
  print(".... Merging Summary and VIF ....")
  df <- merge(df_sumry, df_vif, by="var")
  return(df)
}



# Binning Model Year, 3 bins
#  2011-2015, 2006-2010, <2006
year_bin <- function(year) {
  year <- as.numeric(year)
  if(year>2010) {
    return("2011_2015")
  } else if (year>2005) {
    return("2006_2010")
  } else {
    return("<2006")
  }
}



# Convert 2-level Categorical Variable to numeric type
cat_lv2_2num <- function(df) {
  for (i in colnames(df)) {
    levels(df[[i]]) <- c(1,0)
    df[[i]] <- as.numeric(levels(df[[i]]))[df[[i]]]
  }
  return(df)
}



# COnvert categorical dependant variable to numeric type
# "Yes" <- 1
# "No"  <- 0
YesNo2num <- function(v) {
  v <- as.character(v)
  v[v=="Yes"] <- "1"
  v[v=="No"]  <- "0"
  v <- as.numeric(v)
  return(v)
}



# Dimensional Variable Univariate(barlot) Analysis plots
dim_uni_qplot <- function(data,dim) {
  v <- data[[ dim ]]
  p <- qplot(x=reorder(v,v,function(x)-length(x))) + 
    theme(axis.text.x=element_text(angle = 45,hjust = 1)) + 
    labs(x="",title=dim)
  p
  print(p)
}



dim_multi_vsResponse <- function(data, dim, resp) {
  # since reorder with aes_string not working, will create local df
  # and used hardcode colnames for plotting
  
  local_df <- data[c(dim,resp)]
  colnames(local_df) <- c("dim", "resp")
  
  
  # barplot
  b <- ggplot(local_df, aes(x=reorder(dim,dim,function(x)-length(x)))) + geom_bar(aes(y=(..count..))) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
    geom_text(aes(y=(..count..), 
                  label = scales::percent( (..count..)/sum(..count..))),
              stat = "count",
              vjust = -0.5
    ) +
    labs(x="", title=dim) 
  # percent barplot
  pb <- ggplot(local_df, aes(x=reorder(dim,dim,function(x)-length(x)),fill=resp)) + geom_bar(position = "fill") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
    labs(x="", title=dim)
  # gridplot
  plot_grid(b,pb)
}



# percent barplot of multi-variate categorical variables
dim_multi_percent_barplot <- function(data,dim,col) {
  ggplot(data, aes_string(dim)) +
    geom_bar(aes_string(fill = col), position = "fill") + 
    theme(axis.text.x=element_text(angle = 45,vjust = 0.5))
}



# Replace missing values with Mean
rep_mean <- function(v) {
  v[is.na(v)] <- mean(v,na.rm = T)
  return(v)
}



# Replace missing values with Median
rep_median <- function(v) {
  v[is.na(v)] <- median(v,na.rm = T)
  return(v)
}



# Replace missing values with Zero
rep_zero <- function(v) {
  v[is.na(v)] <- 0
  return(v)
}



# Mark missing values as "uncategorized"
rep_uncategorized <- function(v) {
  v <- as.character(v)
  v[is.na(v)] <- "uncategorized"
  v <- as.factor(v)
  return(v)
}



# Replace value of  Factor variable
rep_dim_factor <- function(v, oldval, newval) {
  v <- as.character(v)
  v[v==oldval] <- newval
  v <- as.factor(v)
  return(v)
}



# Replace missing Categorical variable with Mode
rep_dim_mode <- function(v) {
  mod <- names(sort(-table(v)))[1]
  v[is.na(v)] <- mod
  return(v)
}



# Cap/floor outliers @ 25th and 95th percentile
cap_outliers <- function(v) {
  # make sure NO missing values
  quantiles <- quantile( v, c(.05, .95 ) )
  v[ v < quantiles[1] ] <- quantiles[1]
  v[ v > quantiles[2] ] <- quantiles[2]
  return(v)
}



# Summarize NA's
n.a.summary <- function(df) {
  obs <- nrow(df)
  cs <- colSums(is.na(df))  # NA's per column
  cls <- sapply(df,class)
  df <- data.frame(Vars = names(cs), NAS = cs, class=cls, row.names = NULL)
  df <- df[order(df$NAS),]
  df$perNAS <- 100*(df$NAS/obs)
  df
}



# *************************************************************************----
#                WORKING DIR ----
# *************************************************************************----
# Set Working Directory ----
#  Directory Structure : 
#
#     ..../CaseStudy
#             |
#             |---> code         # R-Code
#             |---> input        # contains input Dataset, DataDictionary
#             |---> output       # gifs/plots written by R-Code
# 
#setwd("C:/Users/atchirc/SLAPG/PGDDA/Course3_PredictiveAnalytics_I/CaseStudy/code")



# *************************************************************************----
#                LOAD DATA ----
# *************************************************************************----

# . . . . Load Data ----
# Data required for analyzing Telecom customer Churn is available in 3 different 
# .csv files, Load and Merge  3 datasets and create one Master database for
# Further analysis

# . . . . . . . . Customer Data ----
cust_data <- read.csv("../input/customer_data.csv")

str(cust_data)

summary(cust_data)

# OBSERVATIONS : 
#          7043  observations  of 5 Variables
#      *** Granularity   :  CustomerID
#          gender        : 2-level categorical variable
#          SeniorCitizen :    -do-
#          Partner       :    -do-
#          Dependents    :    -do-
#
#         NO Missing Values
#         CustomerID Unique
 
# . . . . . . . . Churn Data ----
churn_data <- read.csv("../input/churn_data.csv")

str(churn_data)

summary(churn_data)

# OBSERVATIONS :
#         7043 Observations of 5 variables
#     *** Granularity   :  CustomerID
#         tenure        :  integer
#         PhoneService  :   2-level Categorical Variable
#         Contract      :   3-level Categorical Variable
#         PaperlessBil  :   2-level Categorical Variable
#         PaymentMethod :   4-level Categorical Variable
#         MonthlyCharges:   Numeric
#         TotalCharges  :   Numeric
#         Churn         :   2-level Dependant Variable
#
#         11 Missing Values
#         CustomerID Unique

# . . . . . . . . Internet Data ----
web_data <- read.csv("../input/internet_data.csv")

str(web_data)

summary(web_data)

# OBSERVATIONS :
#         7043 Observations of 9 Variables
#         Granularity   :   CustomerID
#         MultipleLines :   3-level Categorical Variable
#         InternetService :     -do-
#         OnlineSecurity  :     -do-
#         OnlineBackup    :     -do-
#         DeviceProtection:     -do-
#         TechSupport     :     -do-
#         streamingTV     :     -do-
#         StreamingMovies :     -do-
#
#         NO Missing Values
#         CustomerID Unique


# . . . . Merge Data ----
#  
#   All 3 datasets are at "CustomerID" Granularity, will create Master file
# merging all by "CustomerID

# Merging  Customer and Churn Data sets
cust_chur_data <- merge(cust_data, churn_data, by="customerID")

# Create Master file merging web_data and Cust_chur_data
churn <- merge(cust_chur_data, web_data, by="customerID")


# . . . . Structure ----
# Understand the structure of the collated file.

str(churn)
# 7043 observations of 21 variables
 


# *************************************************************************----
#                DATA CLEANING ----
# *************************************************************************----

# . . . . De-Duplication ----
sum(duplicated(churn))   # No Duplicated Observations found

# . . . . Missing Values ----
sum(is.na(churn))        # 11 Missing values, let omit them

churn <- na.omit(churn)

# cross check duplicated/missing values
sum(duplicated(churn))   # No Duplicated Observations found

sum(is.na(churn))        # No Missing Values found

# . . . . Homonyms and Synonyms ----
# No Homonymes or synonyms found

# . . . . Correct Data Types ----
# SeniorCitizen is integer with levels (0,1), having categorical features
#   lets change it to categorical

churn$SeniorCitizen <- as.factor(as.character(churn$SeniorCitizen))


# "CustomerID" is a unique ID for each Customer, will omit for consideration
churn <- churn[,-1]  # "CustomerID" index is 1



str(churn)


# *************************************************************************----
#                EXPLORATORY ANALYSIS ----
# *************************************************************************----

# . . . . Multivariate Categorical Variables ----
# Make bar charts to find interesting relationships between variables.
# "dim_multi_vsRespone" functions defined in Procs section has both 
# Univariate and Multivariate Plots
# 
#
genderVsChurn           <- dim_multi_vsResponse(churn, "gender","Churn")
SeniorCitizenVsChurn    <- dim_multi_vsResponse(churn, "SeniorCitizen","Churn")
partnerVsChurn          <- dim_multi_vsResponse(churn, "Partner","Churn")
DependantsVsChurn       <- dim_multi_vsResponse(churn, "Dependents","Churn")
PhoneServiceVsChurn     <- dim_multi_vsResponse(churn, "PhoneService","Churn")
contractVsChurn         <- dim_multi_vsResponse(churn, "Contract","Churn")
billingVsChurn          <- dim_multi_vsResponse(churn, "PaperlessBilling","Churn")
paymentVsChurn          <- dim_multi_vsResponse(churn, "PaymentMethod","Churn")
multipleLinesVsChurn    <- dim_multi_vsResponse(churn, "MultipleLines","Churn")
InternetServiceVsChurn  <- dim_multi_vsResponse(churn, "InternetService","Churn")
onlineSecurityVsChurn   <- dim_multi_vsResponse(churn, "OnlineSecurity","Churn")
onlinebackupVsChurn     <- dim_multi_vsResponse(churn, "OnlineBackup","Churn")
deviceprotectionVsChurn <- dim_multi_vsResponse(churn, "DeviceProtection","Churn")
techsupportVsChurn      <- dim_multi_vsResponse(churn, "TechSupport","Churn")
streamingtvVsChurn      <- dim_multi_vsResponse(churn, "StreamingTV","Churn")
streamingMoviesVsChurn  <- dim_multi_vsResponse(churn, "StreamingMovies","Churn")

plot_grid(InternetServiceVsChurn,contractVsChurn,
          deviceprotectionVsChurn,onlinebackupVsChurn)

plot_grid(onlineSecurityVsChurn,paymentVsChurn,
          streamingMoviesVsChurn,techsupportVsChurn)


# . . . . Univariate Numerical Analysis ----
# Make Box plots for numeric variables to look for outliers. 

num_var <- c("tenure","MonthlyCharges","TotalCharges")

numerical_uni_plots(churn[num_var], "Initial")


# . . . . Numerical Vs Respone Variable ----

MonthlyChargesVsChurn <- ggplot(churn, aes(MonthlyCharges, col=Churn)) + geom_density()
TotalchargesVsChurn   <- ggplot(churn, aes(TotalCharges, col=Churn)) + geom_density()
tenureVsChurn         <- ggplot(churn, aes(tenure, col=Churn)) + geom_density()
 
plot_grid(MonthlyChargesVsChurn, 
          TotalchargesVsChurn,
          tenureVsChurn)


# *************************************************************************----
#                FEATURE ENGINEERING ----
# *************************************************************************----

#    Dateset have mix variable types, for the classification algorithms, variables 
# should be of same type. will create two version of Dataset, one having numerical
# variable type(churn_num) and the Other Categorical Variables(churn_cat)
#

# . . . . Feature Conversion(All Variabes to Numerical Types) ----

# Categorical to numerical
cat_var_lvgt2 <- c("Contract","PaymentMethod","MultipleLines","InternetService",
                  "OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport",
                  "StreamingTV","StreamingMovies")
cat_var_lv2  <- c("gender","SeniorCitizen","Partner","Dependents",
                  "PhoneService","PaperlessBilling")

# . . . . . . . . Format Values(Prune Spaces...) ----
# prune blankspaces and braces from values
for (i in cat_var_lvgt2) {
  churn[[ i ]] <- as.factor(
                      gsub(" +|\\(|\\)","_",churn[[ i ]])
                      )
}
for (i in cat_var_lv2) {
  churn[[ i ]] <- as.factor(
    gsub(" +|\\(|\\)","_",churn[[ i ]])
  )
}

  
# . . . . . . . . Dummy Variables ----
cat_var_lv2_df <- cat_lv2_2num(churn[cat_var_lv2])

dummy_var_df <- as.data.frame(
                      model.matrix(
                        ~Contract+PaymentMethod+MultipleLines+InternetService+
                          OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+
                          StreamingTV+StreamingMovies,
                        churn
                      )
                )[, -1]   # remove intercept

# . . . . . . . . Consolidated DataSet ----
# Consolidated Dataset of Numerical Datatypes
churn_num <- cbind(churn["Churn"],    # dependant variable
                   churn[num_var],    # Normalized Numeric Variables
                   cat_var_lv2_df,    # 2 level Categorial variables converted to Numeric type
                   dummy_var_df)      # Dummy Variables for categorical variables with more than 2-Levels


# . . . . Feature Normalization ----
churn_num[num_var] <- scale(churn_num[num_var])

# Numerical variables quartile plots
numerical_uni_plots(churn_num[num_var], "postTransform")


# . . . . Feature Converstion(All Variables to Categorical Types) ----

# Make a copy of Dataset
churn_cat <- churn

# convert numerical variable to categorical through equal binning, we choose to
# bin numerical variables into 4 categories. boundaries are choosen w.r.t PDF crossings
# for Total Charges and average values for Monthly Charges. Refer to EDA plots
# create new variables "MonthlyCharges_grp" & "TotalCharges_grp" and omit "MonthlyCharges"
# and "TotalCharges"

# . . . . . . . . Binning Monthly Charges ----
churn_cat$MonthlyCharges_grp <- discretize(churn_cat$MonthlyCharges,
                                           categories=4, labels = c("low", "belowAvg", "AboveAvg", "high"))

# . . . . . . . . Binning Total Charges ----
churn_cat$TotalCharges_grp <- discretize(churn_cat$TotalCharges,
                                         categories=4, labels = c("lt_2k", "2_4K", "4_6k" ,"gt_6K"))

# . . . . . . . . Binning Tenure ----
churn_cat$tenure_grp <- discretize(churn_cat$tenure,
                                         categories=4, labels = c("lt_mnth", "1_2month", "2_3mnth" ,"gt_3mnth"))

churn_cat <- churn_cat[,-c(10,11,5)]



# *************************************************************************----
#                CREATE TRAIN & TEST DATASET ----
# *************************************************************************----
#
#  "churn" contains actual dataset
#  "churn_cat" All variables transformed as categorical
#  "churn_num" All variables(except dependant variable) transformed as numerical
#
# *****************************************************************************

set.seed(100)

# Convert dependant variable to int (0, 1) and use as reference to split
# dataset to have equal ratio of +ve in train and test data
num_resp <- as.character(churn$Churn)
num_resp[num_resp == "Yes"] <- "1"
num_resp[num_resp == "No"] <- "0"
num_resp <- as.numeric(num_resp)


# compute indices to be used for training, make sure train and test
# sets have equal fraction of +ve observations
indices <- sample.split(num_resp, SplitRatio = 0.7)

resp_indx_num <- 1
resp_indx_cat <- 9

# . . . . Categorical Type DataSet ----

# splitting numerical type dataset
train_num <- churn_num[indices,]
test_num  <- churn_num[!indices,]

traindata_num <- train_num[,-resp_indx_num]
testdata_num  <- test_num[,-resp_indx_num]

labels_train_num <- train_num[,resp_indx_num]
labels_test_num  <- test_num[,resp_indx_num]


# . . . . Numerical Type DataSet ----

# splitting Categorical type dataset
train_cat <- churn_cat[indices,]
test_cat  <- churn_cat[!indices,]

traindata_cat <- train_cat[,-resp_indx_cat]
testdata_cat <- test_cat[,-resp_indx_cat]

labels_train_cat <- train_cat[,resp_indx_cat]
labels_test_cat  <- test_cat[,resp_indx_cat]


# verify data sets
print("*****Labels Length: ******")
length(labels_train_cat)
length(labels_test_cat)
length(labels_train_num)
length(labels_test_num)
print("*****Labels Str: ******")
str(labels_train_cat)
str(labels_test_cat)
str(labels_train_num)
str(labels_test_num)
print("***** Churn DataSet : *****")
dim(churn)
str(churn)
print("***** Churn_num DataSet : *****")
dim(churn_num)
str(churn_num)
print("***** Churn_cat DataSet : *****")
dim(churn_cat)
str(churn_cat)
print("***** train_num DataSet : *****")
dim(train_num)
str(train_num)
print("***** test_num DataSet : *****")
dim(test_num)
str(test_num)
print("***** train_cat DataSet : *****")
dim(train_cat)
str(train_cat)
print("***** test_cat DataSet : *****")
dim(test_cat)
str(test_cat)
print("***** traindata_num DataSet : *****")
dim(traindata_num)
str(traindata_num)
print("***** testdata_num DataSet : *****")
dim(testdata_num)
str(testdata_num)
print("***** traindata_cat DataSet : *****")
dim(traindata_cat)
str(traindata_cat)
print("***** testdata_cat DataSet : *****")
dim(testdata_cat)
str(testdata_cat)





# **************************************************************************----
#                K-NN ----
# **************************************************************************----


# . . . . Data Preparation ----
train <- train_num   # refer to "Create Train & Test Dataset" Section
test  <- test_num

train.data <- train[,-1] # strip dependant variable "Churn"
test.data <-  test[, -1]

train.labels <- train[,1]
test.labels  <- test[,1]


# . . . . 1-NN ----
knn1.mod <- knn(train.data,
            test.data,
            train.labels,
            k=1,
            prob = TRUE)

knn1.cm <- confusionMatrix(knn1.mod,
                test.labels,
                positive = "No")

knn1.pred <- prediction(attr(knn1.mod,"prob"), test.labels , label.ordering = c("Yes","No"))
knn1.perf <- performance(knn1.pred,"tpr","fpr")

# . . . . 3-NN ----
knn3.mod <- knn(train.data,
                test.data,
                train.labels,
                k=3,
                prob = TRUE)

confusionMatrix(knn3.mod,
                test.labels,
                positive = "No")

knn3.pred <- prediction(attr(knn3.mod,"prob"), test.labels , label.ordering = c("Yes","No"))
knn3.perf <- performance(knn3.pred,"tpr","fpr")


# . . . . 5-NN ----
knn5.mod <- knn(train.data,
                test.data,
                train.labels,
                k=5,
                prob = TRUE)

confusionMatrix(knn5.mod,
                test.labels,
                positive = "No")

knn5.pred <- prediction(attr(knn5.mod,"prob"), test.labels , label.ordering = c("Yes","No"))
knn5.perf <- performance(knn5.pred,"tpr","fpr")


# . . . . 7-NN ----
knn7.mod <- knn(train.data,
                test.data,
                train.labels,
                k=7,
                prob = TRUE)

confusionMatrix(knn7.mod,
                test.labels,
                positive = "No")

knn7.pred <- prediction(attr(knn7.mod,"prob"), test.labels , label.ordering = c("Yes","No"))
knn7.perf <- performance(knn7.pred,"tpr","fpr")



# . . . . Optimal-K ----
# Find the optimal k using train() funtion from 'caret' package (cross validation)
# Report the optimal k and the performance metrics.
knn.optk <- train(
  Churn~., 
  data=train_num,
  method='knn',
  tuneGrid=expand.grid(.k=1:100),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15
  )
)

plot(knn.optk)
title("KNN - Optimal K")


knn.opt.mod <- knn(train.data,
                test.data,
                train.labels,
                k=36,
                prob = TRUE)

knn.opt.cm <- confusionMatrix(knn.opt.mod,
                test.labels,
                positive = "No")

knn.opt.pred <- prediction(attr(knn.opt.mod,"prob"), test.labels , label.ordering = c("Yes","No"))
knn.opt.perf <- performance(knn.opt.pred,"tpr","fpr")
knn.auc      <- performance(knn.opt.pred,"auc")


# . . . . Plot ROC ----
plot(knn1.perf,col="black",lty=3, lwd=3)
plot(knn3.perf,col="red",lty=3, lwd=3, add=TRUE)
plot(knn5.perf,col="blue",lty=3, lwd=3, add=TRUE)
plot(knn7.perf,col="green",lty=3, lwd=3, add=TRUE)
plot(knn.opt.perf,col="magenta",lty=3, lwd=3, add=TRUE)

legend(0.8, 0.6,
       c("1-NN","3-NN","5-NN","7-NN","opt-k"), 
       lty = c(1,1), lwd = c(1,1),
       col=c("black", "red", "blue", "green","magenta")
)

title("K-NN  ROC values ")



# *************************************************************************----
#                NAIVE BAYES ----
# *************************************************************************----

# . . . . Data Preparation ----
train <- train_cat   # refer to "Create Train & Test Dataset" Section
test  <- test_cat

train.data <- train[,-9] # strip dependant variable "Churn"
test.data <-  test[, -9]

train.labels <- train[,9]
test.labels  <- test[,9]


# . . . . Model ----

nb.mdl <- naiveBayes(Churn~., data = train)

pred <- predict(nb.mdl,test.data)
pred_raw <- predict(nb.mdl,test.data,type = "raw")

pred_prob <- pred_raw[,1]
realvec <- ifelse(test.labels=="Yes",1,0)

# contingency table using table function
table(pred,test.labels)

nb.cm <- confusionMatrix(pred,test.labels)
nb.cm
# ROC Curve
nb.prediction <- prediction(pred_prob,realvec, label.ordering = c(1,0))
nb.perf <- performance(nb.prediction, "tpr","fpr")
plot(nb.perf)
title("NaiveBayes - ROC")
nb.auc <- performance(nb.prediction,"auc")
nb.auc@y.values[[1]]

# *************************************************************************----
#                LOGISTIC REGRESSION ----
# *************************************************************************----

# . . . . Data Preparation ----

# . . . . Data Preparation ----
train <- train_num   # refer to "Create Train & Test Dataset" Section
test  <- test_num

train$Churn <- YesNo2num(train$Churn)  # Convert dependant variable to binary
test$Churn  <- YesNo2num(test$Churn)


train.data <- train[,-1] # strip dependant variable "Churn"
test.data <-  test[, -1]

train.labels <- train[,1]
test.labels  <- test[,1]



# . . . . Model Development ----

# . . . . . . . . Model 1 (RD-4102, AIC-4150)----
lr.mod1 <- glm(Churn~., train, family = "binomial")

summary(lr.mod1)

# . . . . . . . .Step Wise Reduction (RD-4105, AIC-4139) ----
stp <- step(lr.mod1, direction = "both")

summary(stp)
View(model_sumry_vif(stp))

# . . . . . . . .Model 2 (Rmvd : TotalCharges (RD-4119, AIC-4152) -----
lr.mod2 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
      SeniorCitizen + PhoneService + PaperlessBilling + ContractOne_year + 
      ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
      InternetServiceFiber_optic + InternetServiceNo + OnlineBackupYes + 
      DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
    family = "binomial", data = train)

summary(lr.mod2)
View(model_sumry_vif(lr.mod2))

# . . . . . . . . Model 3 (Rmvd : PhoneService (RD-4130, AIC-4160))-----
lr.mod3 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
              InternetServiceFiber_optic + InternetServiceNo + OnlineBackupYes + 
              DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
            family = "binomial", data = train)

summary(lr.mod3)
View(model_sumry_vif(lr.mod3))

# . . . . . . . . Model 4 (Rmvd : OnlineBackupYes (RD-4131, AIC-4159))-----
lr.mod4 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
              InternetServiceFiber_optic + InternetServiceNo + 
              DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
            family = "binomial", data = train)

summary(lr.mod4)
View(model_sumry_vif(lr.mod4))

# . . . . . . . . Model 5 (Rmvd : InternetServiceFiber_optic +(RD-4206, AIC-4232))-----
lr.mod5 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
               InternetServiceNo + 
              DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes, 
            family = "binomial", data = train)

summary(lr.mod5)
View(model_sumry_vif(lr.mod5))

# . . . . . . . . Model 6 (Rmvd : DeviceProtectionYes (RD-4206, AIC-4230))-----
lr.mod6 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
              InternetServiceNo + 
              StreamingTVYes + StreamingMoviesYes, 
            family = "binomial", data = train)

summary(lr.mod6)
View(model_sumry_vif(lr.mod6))


# . . . . . . . . Model 7 (Rmvd : StreamingTVYes  (RD-4208, AIC-4230))-----
lr.mod7 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
              InternetServiceNo + 
              StreamingMoviesYes, 
            family = "binomial", data = train)

summary(lr.mod7)
View(model_sumry_vif(lr.mod7))

# . . . . . . . . Model 8 (Rmvd : StreamingMoviesYes  (RD-4211, AIC-4231))-----
lr.mod8 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check + MultipleLinesYes + 
              InternetServiceNo, 
            family = "binomial", data = train)

summary(lr.mod8)
View(model_sumry_vif(lr.mod8))

# . . . . . . . . Model 9 (Rmvd : MultipleLinesYes  (RD-4216, AIC-4234))-----
lr.mod9 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check +  
              InternetServiceNo, 
            family = "binomial", data = train)

summary(lr.mod9)
View(model_sumry_vif(lr.mod9))

# . . . . . . . . Model 10 (Rmvd : InternetServiceNo  (RD-4221, AIC-4237))-----
lr.mod10 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
              SeniorCitizen + PaperlessBilling + ContractOne_year + 
              ContractTwo_year + PaymentMethodElectronic_check, 
            family = "binomial", data = train)

summary(lr.mod10)
View(model_sumry_vif(lr.mod10))


# . . . . . . . . Model 11 (Rmvd : SeniorCitizen  (RD-4230, AIC-4244))-----
lr.mod11 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
               PaperlessBilling + ContractOne_year + 
               ContractTwo_year + PaymentMethodElectronic_check, 
             family = "binomial", data = train)

summary(lr.mod11)
View(model_sumry_vif(lr.mod11))

# . . . . . . . . Model 12 (Rmvd : PaperlessBilling (RD-4254, AIC-4266))-----
lr.mod12 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
               ContractOne_year + 
               ContractTwo_year + PaymentMethodElectronic_check, 
             family = "binomial", data = train)

summary(lr.mod12)
View(model_sumry_vif(lr.mod12))

# . . . . . . . . Model 13 (Rmvd : PaymentMethodElectronic_check (RD-4292, AIC-4302))-----
lr.mod13 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
               ContractOne_year + 
               ContractTwo_year , 
             family = "binomial", data = train)

summary(lr.mod13)
View(model_sumry_vif(lr.mod13))


# . . . . Model Evaluation ----

# . . . . . . . . C-Statistic ----
# Predict Churn probabilities for train dataset
train$pred_prob <- predict(lr.mod13, type = "response")

rcorr.cens(train$pred_prob, train$Churn)


# Predict Churn probabilities for test dataset
test$pred_prob <- predict(lr.mod13, type = "response", newdata = test)

rcorr.cens(test$pred_prob, test$Churn)


# . . . . . . . . KS-Statistic ----

train.model.score <- prediction(train$pred_prob, train$Churn)
train.perf <- performance(train.model.score, "tpr", "fpr")

ks_table_train <- attr(train.perf, "y.values")[[1]] -
  attr(train.perf, "x.values")[[1]]

# max values corresponds to KS
ks_train <- max(ks_table_train)

# find the poistion of KS
ks_pos <- which(ks_table_train==ks_train)

# Compute to which decile KS falls in
ks_pos/nrow(train_num)
# KS values observed in 5th Decile


test.model.score <- prediction(test$pred_prob, test$Churn)
test.perf <- performance(test.model.score, "tpr", "fpr")
lr.perf <- test.perf
lr.auc <- performance(test.model.score, "auc")

ks_table_test <- attr(test.perf, "y.values")[[1]] -
  attr(test.perf, "x.values")[[1]]

# max values corresponds to KS
ks_test <- max(ks_table_test)
# find the poistion of KS
ks_pos <- which(ks_table_test==ks_test)

# Compute to which decile KS falls in
ks_pos/nrow(test)
# KS values observed in 5th Decile



# . . . . THRESHOLD ----

# . . . . . . . . Training Set ----
 
confusionMatrix(as.numeric(train$pred_prob > 0.3),
                train$Churn, positive = "0")



# . . . . . . . . Test Set ----

 
confusionMatrix(as.numeric(test$pred_prob > 0.3),
                test$Churn, positive = "0")


confusionMatrix(as.numeric(test$pred_prob > 0.5),
                         test$Churn, positive = "0")


confusionMatrix(as.numeric(test$pred_prob > 0.7),
                         test$Churn, positive = "0")




# *************************************************************************----
#                SVM ----
# *************************************************************************----

# svm requires dependant variable to be factor type 

# . . . . Data Preparation ----
train <- train_num   # refer to "Create Train & Test Dataset" Section
test  <- test_num

train.data <- train[,-1] # strip dependant variable "Churn"
test.data <-  test[, -1]

# . . . . Model ----
# Implement the SVM algorithm using the optimal cost.

# . . . . . . . . model 0 with cost = 0.1 ----
model_svm = svm(Churn ~., data = train, kernel = "linear", cost = 0.1, scale = F, prob=TRUE)  
# plot(model_svm, train.data)
summary(model_svm)


# . . . . . . . . model 0 with cost = 10 ----
model_svm = svm(Churn ~., data = train, kernel = "linear", cost = 10, scale = F)  
# plot(model_svm, train.data)
summary(model_svm)

# . . . . . . . . model 0 with cost = 100 ----
model_svm = svm(Churn ~., data = train, kernel = "linear", cost = 100, scale = F)  
# plot(model_svm, train.data)
summary(model_svm)

# . . . . . . . . model 0 with cost = 1000 ----
model_svm = svm(Churn ~., data = train, kernel = "linear", cost = 1000, scale = F)  
# plot(model_svm, train.data)
summary(model_svm)  # Reached Max Iterations


# . . . . Optimal Cost ----

tune.svm <- tune(svm, Churn ~ ., data = train, kernel = "linear", 
                 ranges = list(cost=c(0.001, 0.01, 0.1, 0.5, 1, 10, 100)))

summary(tune.svm)


best.mod = tune.svm$best.model


# . . . . Train Set Prediction ----

ypred = predict(best.mod, train.data, decision.values=TRUE)

ypred.prob <- attr(ypred, "decision.values")

table(predicted = ypred, truth= train$Churn)

# . . . . Confusion Matrix ----

confusionMatrix(ypred, train$Churn)

#plot(best.mod, train.data)



# . . . . Test Set Prediction ----

ypred = predict(best.mod, test.data, decision.values=TRUE)

ypred.prob <- attr(ypred, "decision.values")

table(predicted = ypred, truth= test$Churn)

# . . . . Confusion Matrix ----

svm.cm <- confusionMatrix(ypred, test$Churn)

#plot(best.mod, test.data)

# . . . . ROC ----
svm.prediction <- prediction(ypred.prob, test.labels)
svm.pref       <- performance(svm.prediction, "fpr", "tpr")
svm.auc        <- performance(svm.prediction, "auc")
plot(svm.pref)
title("SVM - ROC")


# . . . . Threshold ----
resp_bnry <- YesNo2num(test$Churn)

svm.cm <- confusionMatrix(as.numeric(ypred > 0.43),
                         resp_bnry, positive = "0")




# *************************************************************************----
#        Plotting K-NN  NB  LR SVM  - ROCs ----
# *************************************************************************----

plot(svm.pref,col="black",lty=3, lwd=3)
plot(nb.perf,col="red",lty=3, lwd=3, add=TRUE)
plot(knn.opt.perf,col="blue",lty=3, lwd=3, add=TRUE)
plot(lr.perf, col="green", lty=3, lwd=3, add=TRUE)

legend(0.8, 0.6,
       c("SVM","NB","KNN","LR"), 
       lty = c(1,1), lwd = c(1,1),
       col=c("black", "red","blue","green")
)



# *************************************************************************----
#        Plotting K-NN  NB  LR SVM  - COnfusionMatrix ----
# *************************************************************************----

# plot(svm.cm$table, main="SVM ConfusionMatrix")
svm.cm$table
nb.cm$table
lr.cm$table
knn.opt.cm$table

