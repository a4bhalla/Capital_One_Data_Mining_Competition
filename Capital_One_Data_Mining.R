################################################################################
##
## dmc_tutorial_code.R
## Code for Captial One Data Mining Cup
##
## Clubs: UW Statistics (2013), UW MFSA (2014)
## 
## Last Updated: Feb 13th, 2015
##
################################################################################

# Start fresh
rm(list=ls())

# Load relevant packages
library(ggplot2)
library(party)
library(randomForest)
library(plyr)
library(Hmisc)
library(MASS)
library(car)
library(boot)
library(sqldf)
library(bigmemory)
library(biganalytics)

#set working directory
setwd("/Users/amarbhalla/Desktop/DMC2015Data")

# Obtain the Data ---------------------------------------------------------
data.raw <- read.csv("SEM_DAILY_BUILD.csv",header=TRUE,row.names=NULL)
test.raw <- read.csv("SEM_DAILY_BUILD.csv", header=TRUE,row.names=NULL)
validation.data <- read.csv("SEM_DAILY_VALIDATION.csv", header=TRUE, row.names=NULL)


# Clean the Data ----------------------------------------------------------

# Create a copy for experimentation 
data.copy <- data.raw
# Create a copy with all NA's removed. 
data.clean <- subset(data.copy, !(CLICKS==0))
# Clean up the data up by turning NA's into 0's
data.clean$ZERO_QUALITY_SCORES<-with(data.clean, impute(TOTAL_QUALITY_SCORE,0))
data.clean$ZERO_VISITS<-with(data.clean, impute(VISITS,0))
data.clean$ZERO_APPLICATIONS<-with(data.clean, impute(APPLICATIONS,0))
data.clean$ZERO_APP_APPROVED<-with(data.clean, impute(APP_APPROVED,0))
data.clean$ZERO_APP_DECLINED<-with(data.clean, impute(APP_DECLINED,0))
data.clean$ZERO_APP_PENDING<-with(data.clean, impute(APP_PENDING,0))
data.clean$ZERO_APPS_PROD_1<-with(data.clean, impute(APPS_PROD_1,0))
data.clean$ZERO_APPS_PROD_2<-with(data.clean, impute(APPS_PROD_2,0))
data.clean$ZERO_APPS_PROD_3<-with(data.clean, impute(APPS_PROD_3,0))
data.clean$ZERO_APPS_PROD_4<-with(data.clean, impute(APPS_PROD_4,0))
data.clean$ZERO_APPS_PROD_5<-with(data.clean, impute(APPS_PROD_5,0))
data.clean$ZERO_APPS_PROD_6<-with(data.clean, impute(APPS_PROD_6,0))
data.clean$ZERO_PROD_1_APPROVED<-with(data.clean, impute(PROD_1_APPROVED,0))
data.clean$ZERO_PROD_2_APPROVED<-with(data.clean, impute(PROD_2_APPROVED,0))
data.clean$ZERO_PROD_3_APPROVED<-with(data.clean, impute(PROD_3_APPROVED,0))
data.clean$ZERO_PROD_4_APPROVED<-with(data.clean, impute(PROD_4_APPROVED,0))
data.clean$ZERO_PROD_5_APPROVED<-with(data.clean, impute(PROD_5_APPROVED,0))
data.clean$ZERO_PROD_6_APPROVED<-with(data.clean, impute(PROD_6_APPROVED,0))
data.clean$ZERO_PROD_1_REVENUE<-with(data.clean, impute(PROD_1_REVENUE,0))
data.clean$ZERO_PROD_2_REVENUE<-with(data.clean, impute(PROD_2_REVENUE,0))
data.clean$ZERO_PROD_3_REVENUE<-with(data.clean, impute(PROD_3_REVENUE,0))
data.clean$ZERO_PROD_4_REVENUE<-with(data.clean, impute(PROD_4_REVENUE,0))
data.clean$ZERO_PROD_5_REVENUE<-with(data.clean, impute(PROD_5_REVENUE,0))
data.clean$ZERO_PROD_6_REVENUE<-with(data.clean, impute(PROD_6_REVENUE,0))

data.copy$ZERO_QUALITY_SCORES<-with(data.copy, impute(TOTAL_QUALITY_SCORE,0))
data.copy$ZERO_VISITS<-with(data.copy, impute(VISITS,0))
data.copy$ZERO_APPLICATIONS<-with(data.copy, impute(APPLICATIONS,0))
data.copy$ZERO_APP_APPROVED<-with(data.copy, impute(APP_APPROVED,0))
data.copy$ZERO_APP_DECLINED<-with(data.copy, impute(APP_DECLINED,0))
data.copy$ZERO_APP_PENDING<-with(data.copy, impute(APP_PENDING,0))
data.copy$ZERO_APPS_PROD_1<-with(data.copy, impute(APPS_PROD_1,0))
data.copy$ZERO_APPS_PROD_2<-with(data.copy, impute(APPS_PROD_2,0))
data.copy$ZERO_APPS_PROD_3<-with(data.copy, impute(APPS_PROD_3,0))
data.copy$ZERO_APPS_PROD_4<-with(data.copy, impute(APPS_PROD_4,0))
data.copy$ZERO_APPS_PROD_5<-with(data.copy, impute(APPS_PROD_5,0))
data.copy$ZERO_APPS_PROD_6<-with(data.copy, impute(APPS_PROD_6,0))
data.copy$ZERO_PROD_1_APPROVED<-with(data.copy, impute(PROD_1_APPROVED,0))
data.copy$ZERO_PROD_2_APPROVED<-with(data.copy, impute(PROD_2_APPROVED,0))
data.copy$ZERO_PROD_3_APPROVED<-with(data.copy, impute(PROD_3_APPROVED,0))
data.copy$ZERO_PROD_4_APPROVED<-with(data.copy, impute(PROD_4_APPROVED,0))
data.copy$ZERO_PROD_5_APPROVED<-with(data.copy, impute(PROD_5_APPROVED,0))
data.copy$ZERO_PROD_6_APPROVED<-with(data.copy, impute(PROD_6_APPROVED,0))
data.copy$ZERO_PROD_1_REVENUE<-with(data.copy, impute(PROD_1_REVENUE,0))
data.copy$ZERO_PROD_2_REVENUE<-with(data.copy, impute(PROD_2_REVENUE,0))
data.copy$ZERO_PROD_3_REVENUE<-with(data.copy, impute(PROD_3_REVENUE,0))
data.copy$ZERO_PROD_4_REVENUE<-with(data.copy, impute(PROD_4_REVENUE,0))
data.copy$ZERO_PROD_5_REVENUE<-with(data.copy, impute(PROD_5_REVENUE,0))
data.copy$ZERO_PROD_6_REVENUE<-with(data.copy, impute(PROD_6_REVENUE,0))
# Exploratory Data Analysis -----------------------------------------------

# Before even applying transformations, we might want to get a high level
# look at the the data
summary(data.clean)

data.copy$ch <- with(data.copy, substr(AD_GRP_NM, 1, 10))
data.clean$ch <- with(data.clean, substr(AD_GRP_NM, 1, 10))

## gives us the aggregate sums for various columns

d1<-subset(data.copy, TOTAL_QUALITY_SCORE>1000)
d2<-subset(data.copy, TOTAL_QUALITY_SCORE<=1000)

TOTAL_CLICK_SUM<- sum(data.copy$CLICKS)
TOTAL_CONDITIONAL_IMPRESSIONS<- sum(data.copy$CONDITIONAL_IMPRESSIONS)
TOTAL_QUALITY_SCORE_SUM<- sum(d1$TOTAL_QUALITY_SCORE)+sum(d2$TOTAL_QUALITY_SCORE)
SPEND_SUM<- sum(data.copy$SPEND)
ZERO_VISITS_SUM<-sum(data.copy$ZERO_VISITS)
ZERO_APPLICATIONS_SUM<-sum(data.copy$ZERO_APPLICATION)
ZERO_APP_APPROVED_SUM<-sum(data.copy$ZERO_APP_APPROVED)
ZERO_APP_DECLINED_SUM<-sum(data.copy$ZERO_APP_DECLINED)
ZERO_APP_PENDING_SUM<-sum(data.copy$ZERO_APP_PENDING)

ZERO_APPS_PROD_1_SUM<-sum(data.copy$ZERO_APPS_PROD_1)
ZERO_APPS_PROD_2_SUM<-sum(data.copy$ZERO_APPS_PROD_2)
ZERO_APPS_PROD_3_SUM<-sum(data.copy$ZERO_APPS_PROD_3)
ZERO_APPS_PROD_4_SUM<-sum(data.copy$ZERO_APPS_PROD_4)
ZERO_APPS_PROD_5_SUM<-sum(data.copy$ZERO_APPS_PROD_5)

ZERO_APPS_PROD_6_SUM<-sum(data.copy$ZERO_APPS_PROD_6)
ZERO_PROD_1_APPROVED_SUM<-sum(data.copy$ZERO_PROD_1_APPROVED)
ZERO_PROD_2_APPROVED_SUM<-sum(data.copy$ZERO_PROD_2_APPROVED)
ZERO_PROD_3_APPROVED_SUM<-sum(data.copy$ZERO_PROD_3_APPROVED)
ZERO_PROD_4_APPROVED_SUM<-sum(data.copy$ZERO_PROD_4_APPROVED)
ZERO_PROD_5_APPROVED_SUM<-sum(data.copy$ZERO_PROD_5_APPROVED)
ZERO_PROD_6_APPROVED_SUM<-sum(data.copy$ZERO_PROD_6_APPROVED)

ZERO_PROD_1_REVENUE_SUM<-sum(data.copy$ZERO_PROD_1_REVENUE)
ZERO_PROD_2_REVENUE_SUM<-sum(data.copy$ZERO_PROD_2_REVENUE)
ZERO_PROD_3_REVENUE_SUM<-sum(data.copy$ZERO_PROD_3_REVENUE)
ZERO_PROD_4_REVENUE_SUM<-sum(data.copy$ZERO_PROD_4_REVENUE)
ZERO_PROD_5_REVENUE_SUM<-sum(data.copy$ZERO_PROD_5_REVENUE)
ZERO_PROD_6_REVENUE_SUM<-sum(data.copy$ZERO_PROD_6_REVENUE)

data.copy$TOTAL_REVENUE <- with(data.copy, ZERO_PROD_1_REVENUE+ZERO_PROD_2_REVENUE+ZERO_PROD_3_REVENUE+ZERO_PROD_4_REVENUE
       +ZERO_PROD_5_REVENUE+ZERO_PROD_6_REVENUE)

ZERO_PROD_1_REVENUE_SUM/ZERO_PROD_1_APPROVED_SUM
ZERO_PROD_2_REVENUE_SUM/ZERO_PROD_2_APPROVED_SUM
ZERO_PROD_3_REVENUE_SUM/ZERO_PROD_3_APPROVED_SUM
ZERO_PROD_4_REVENUE_SUM/ZERO_PROD_4_APPROVED_SUM
ZERO_PROD_5_REVENUE_SUM/ZERO_PROD_5_APPROVED_SUM
ZERO_PROD_6_REVENUE_SUM/ZERO_PROD_6_APPROVED_SUM

# Approval Rates

APPROVAL_RATE_PROD_1<-ZERO_PROD_1_APPROVED_SUM/ZERO_APPS_PROD_1_SUM
APPROVAL_RATE_PROD_2<-ZERO_PROD_2_APPROVED_SUM/ZERO_APPS_PROD_2_SUM
APPROVAL_RATE_PROD_3<-ZERO_PROD_3_APPROVED_SUM/ZERO_APPS_PROD_3_SUM
APPROVAL_RATE_PROD_4<-ZERO_PROD_4_APPROVED_SUM/ZERO_APPS_PROD_4_SUM
APPROVAL_RATE_PROD_5<-ZERO_PROD_5_APPROVED_SUM/ZERO_APPS_PROD_5_SUM
APPROVAL_RATE_PROD_6<-ZERO_PROD_6_APPROVED_SUM/ZERO_APPS_PROD_6_SUM

data.noclicks <- subset(data.copy, !(CLICKS==0))
data.noclicks$CONVERSION_RATE<-with(data.noclicks,ZERO_APPLICATIONS/CLICKS)
data.noclicks$q<-with(data.clean, exp(ZERO_QUALITY_SCORES/CONDITIONAL_IMPRESSIONS))

#FILTER OUT EACH AD GROUP
filter_AD10_LANG2<-subset(data.noclicks, (ch=="AD10_LANG2"))
filter_AD11_LANG2<-subset(data.noclicks, (ch=="AD11_LANG2"))
filter_AD2_LANG3<-subset(data.noclicks, (ch=="AD2_LANG3_"))
filter_AD2_LANG2<-subset(data.noclicks, (ch=="AD2_LANG2_"))
filter_AD3_LANG2<-subset(data.noclicks, (ch=="AD3_LANG2_"))
filter_AD4_LANG2<-subset(data.noclicks, (ch=="AD4_LANG2_"))
filter_AD4_LANG3<-subset(data.noclicks, (ch=="AD4_LANG3_"))
filter_AD5_LANG2<-subset(data.noclicks, (ch=="AD5_LANG2_"))
filter_AD5_LANG3<-subset(data.noclicks, (ch=="AD5_LANG3_"))
filter_AD6_LANG2<-subset(data.noclicks, (ch=="AD6_LANG2_"))
filter_AD6_LANG3<-subset(data.noclicks, (ch=="AD6_LANG3_"))
filter_AD8_LANG2<-subset(data.noclicks, (ch=="AD8_LANG2_"))
filter_AD9_LANG2<-subset(data.noclicks, (ch=="AD9_LANG2_"))
filter_AD11_LANG3<-subset(data.noclicks, (ch=="AD11_LANG3"))
filter_AD3_LANG3<-subset(data.noclicks, (ch=="AD3_LANG3_"))
filter_AD9_LANG3<-subset(data.noclicks, (ch=="AD9_LANG3_"))
filter_AD10_LANG3<-subset(data.noclicks, (ch=="AD10_LANG3"))
filter_AD8_LANG3<-subset(data.noclicks, (ch=="AD8_LANG3_"))

# Average revenue per bid by ad group. 

AD10_LANG2_AVG_REV=sum(filter_AD10_LANG2$TOTAL_REVENUE)/length(filter_AD10_LANG2$TOTAL_REVENUE)
AD11_LANG2_AVG_REV=sum(filter_AD11_LANG2$TOTAL_REVENUE)/length(filter_AD11_LANG2$TOTAL_REVENUE)
AD2_LANG3_AVG_REV=sum(filter_AD2_LANG3$TOTAL_REVENUE)/length(filter_AD2_LANG3$TOTAL_REVENUE)
AD2_LANG2_AVG_REV=sum(filter_AD2_LANG2$TOTAL_REVENUE)/length(filter_AD2_LANG2$TOTAL_REVENUE)
AD3_LANG2_AVG_REV=sum(filter_AD3_LANG2$TOTAL_REVENUE)/length(filter_AD3_LANG2$TOTAL_REVENUE)
AD4_LANG2_AVG_REV=sum(filter_AD4_LANG2$TOTAL_REVENUE)/length(filter_AD4_LANG2$TOTAL_REVENUE)
AD4_LANG3_AVG_REV=sum(filter_AD4_LANG3$TOTAL_REVENUE)/length(filter_AD4_LANG3$TOTAL_REVENUE)
AD5_LANG2_AVG_REV=sum(filter_AD5_LANG2$TOTAL_REVENUE)/length(filter_AD5_LANG2$TOTAL_REVENUE)
AD5_LANG3_AVG_REV=sum(filter_AD5_LANG3$TOTAL_REVENUE)/length(filter_AD5_LANG3$TOTAL_REVENUE)
AD6_LANG2_AVG_REV=sum(filter_AD6_LANG2$TOTAL_REVENUE)/length(filter_AD6_LANG2$TOTAL_REVENUE)
AD6_LANG3_AVG_REV=sum(filter_AD6_LANG3$TOTAL_REVENUE)/length(filter_AD6_LANG3$TOTAL_REVENUE)
AD8_LANG2_AVG_REV=sum(filter_AD8_LANG2$TOTAL_REVENUE)/length(filter_AD8_LANG2$TOTAL_REVENUE)
AD9_LANG2_AVG_REV=sum(filter_AD9_LANG2$TOTAL_REVENUE)/length(filter_AD9_LANG2$TOTAL_REVENUE)
AD11_LANG3_AVG_REV=sum(filter_AD11_LANG3$TOTAL_REVENUE)/length(filter_AD11_LANG3$TOTAL_REVENUE)
AD3_LANG3_AVG_REV=sum(filter_AD3_LANG3$TOTAL_REVENUE)/length(filter_AD3_LANG3$TOTAL_REVENUE)
AD9_LANG3_AVG_REV=sum(filter_AD9_LANG3$TOTAL_REVENUE)/length(filter_AD9_LANG3$TOTAL_REVENUE)
AD10_LANG3_AVG_REV=sum(filter_AD10_LANG3$TOTAL_REVENUE)/length(filter_AD10_LANG3$TOTAL_REVENUE)
AD8_LANG3_AVG_REV=sum(filter_AD8_LANG3$TOTAL_REVENUE)/length(filter_AD8_LANG3$TOTAL_REVENUE)

# Approval rates per bid by ad group 

AD10_LANG2_APP_RATE=sum(filter_AD10_LANG2$ZERO_APP_APPROVED)/sum(filter_AD10_LANG2$ZERO_APPLICATIONS)
AD11_LANG2_APP_RATE=sum(filter_AD11_LANG2$ZERO_APP_APPROVED)/sum(filter_AD11_LANG2$ZERO_APPLICATIONS)
AD2_LANG3_APP_RATE=sum(filter_AD2_LANG3$ZERO_APP_APPROVED)/sum(filter_AD2_LANG3$ZERO_APPLICATIONS)
AD2_LANG2_APP_RATE=sum(filter_AD2_LANG2$ZERO_APP_APPROVED)/sum(filter_AD2_LANG2$ZERO_APPLICATIONS)
AD3_LANG2_APP_RATE=sum(filter_AD3_LANG2$ZERO_APP_APPROVED)/sum(filter_AD3_LANG2$ZERO_APPLICATIONS)
AD4_LANG2_APP_RATE=sum(filter_AD4_LANG2$ZERO_APP_APPROVED)/sum(filter_AD4_LANG2$ZERO_APPLICATIONS)
AD4_LANG3_APP_RATE=sum(filter_AD4_LANG3$ZERO_APP_APPROVED)/sum(filter_AD4_LANG3$ZERO_APPLICATIONS)
AD5_LANG2_APP_RATE=sum(filter_AD5_LANG2$ZERO_APP_APPROVED)/sum(filter_AD5_LANG2$ZERO_APPLICATIONS)
AD5_LANG3_APP_RATE=sum(filter_AD5_LANG3$ZERO_APP_APPROVED)/sum(filter_AD5_LANG3$ZERO_APPLICATIONS)
AD6_LANG2_APP_RATE=sum(filter_AD6_LANG2$ZERO_APP_APPROVED)/sum(filter_AD6_LANG2$ZERO_APPLICATIONS)
AD6_LANG3_APP_RATE=sum(filter_AD6_LANG3$ZERO_APP_APPROVED)/sum(filter_AD6_LANG3$ZERO_APPLICATIONS)
AD8_LANG2_APP_RATE=sum(filter_AD8_LANG2$ZERO_APP_APPROVED)/sum(filter_AD8_LANG2$ZERO_APPLICATIONS)
AD9_LANG2_APP_RATE=sum(filter_AD9_LANG2$ZERO_APP_APPROVED)/sum(filter_AD9_LANG2$ZERO_APPLICATIONS)
AD11_LANG3_APP_RATE=sum(filter_AD11_LANG3$ZERO_APP_APPROVED)/sum(filter_AD11_LANG3$ZERO_APPLICATIONS)
AD3_LANG3_APP_RATE=sum(filter_AD3_LANG3$ZERO_APP_APPROVED)/sum(filter_AD3_LANG3$ZERO_APPLICATIONS)
AD9_LANG3_APP_RATE=sum(filter_AD9_LANG3$ZERO_APP_APPROVED)/sum(filter_AD9_LANG3$ZERO_APPLICATIONS)
AD10_LANG3_APP_RATE=sum(filter_AD10_LANG3$ZERO_APP_APPROVED)/sum(filter_AD10_LANG3$ZERO_APPLICATIONS)
AD8_LANG3_APP_RATE=sum(filter_AD8_LANG3$ZERO_APP_APPROVED)/sum(filter_AD8_LANG3$ZERO_APPLICATIONS)

# Linear Models

m.lm1 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD10_LANG2)
m.lm2 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD11_LANG2)
m.lm3 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD2_LANG3)
m.lm4 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD2_LANG2)
m.lm5 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD3_LANG2)
m.lm6 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD4_LANG2)
m.lm7 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD4_LANG3)
m.lm8 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD5_LANG2)
m.lm9 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD5_LANG3)
m.lm10 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD6_LANG2)
m.lm11 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD6_LANG3)
m.lm12 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD8_LANG2)
m.lm13 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD9_LANG2)
m.lm14 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD11_LANG3)
m.lm15 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD3_LANG3)
m.lm16 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD9_LANG3)
m.lm17 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD10_LANG3)
m.lm18 <- lm(CONVERSION_RATE ~ q + CONDITIONAL_IMPRESSIONS, data=filter_AD8_LANG3)

# Cleaning up data for the validation model 

validation.data$ZERO_IMPRESSIONS<-with(validation.data, impute(IMPRESSIONS,0))
validation.data$ZERO_CONDITIONAL_IMPRESSIONS<-with(validation.data, impute(CONDITIONAL_IMPRESSIONS,0))
validation.data$ZERO_TOTAL_QUALITY_SCORE<-with(validation.data, impute(TOTAL_QUALITY_SCORE,0))
validation.data$ZERO_VISITS<-with(validation.data, impute(VISITS,0))

validation.data$ch <- with(validation.data, substr(AD_GRP_NM, 1, 10))
# we create a function that consumes a type of ad-group, q and conditional impressions in defined
# in the 18 lm's created previously. Based on the AD group, the appropriate conversion rate will be produced 

conversion_rate_function <- function(ADGROUP, q, CONDITIONAL_IMPRESSIONS)
{
  if (ADGROUP=="AD10_LANG2")
  {
    return (1.535e-02 +q*(2.848e-06)+(-3.682e-06)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD11_LANG2")
  {
    return (3.388e-02 +q*(-5.747e-07)+(-2.013e-05)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD2_LANG3")
  {
    return (6.766e-02+q*(9.516e-05)+(-1.379e-04)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD2_LANG2_")
  {
    return (3.714e-02+q*(1.760e-06)+(7.932e-06)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD3_LANG2_")
  {
    return (2.557e-02 + q*(1.963e-06)+(-2.099e-05)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD4_LANG2_")
  {
    return (1.269e-01 + q*(1.357e-05)+(-3.865e-05)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD4_LANG3_")
  {
    return (6.996e-02 + q*(3.258e-05)+(1.739e-04)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD5_LANG2_")
  {
    return (1.825e-02 + q*(4.359e-06)+(-2.513e-06)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD5_LANG3_")
  {
    return (4.197e-02 + q*(-1.337e-05)+(-1.407e-03)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD6_LANG2_")
  {
    return (3.231e-02 + q*(-7.564e-07)+(-1.688e-05)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD6_LANG3_")
  {
    return (1.043e-01+(-2.009e-05)*q+(-1.711e-03)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD8_LANG2_")
  {
    return (1.758e-02 + (2.687e-06)*q+(-1.670e-06)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD9_LANG2_")
  {
    return (4.464e-02 + (1.196e-05)*q + (-6.251e-06)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD11_LANG3")
  {
    return (((1.535e-02 +q*(2.848e-06)+(-3.682e-06)*CONDITIONAL_IMPRESSIONS)+ 
    (3.388e-02 +q*(-5.747e-07)+(-2.013e-05)*CONDITIONAL_IMPRESSIONS)+
    (6.766e-02+q*(9.516e-05)+(-1.379e-04)*CONDITIONAL_IMPRESSIONS)+
    (3.714e-02+q*(1.760e-06)+(7.932e-06)*CONDITIONAL_IMPRESSIONS)+
    (2.557e-02 + q*(1.963e-06)+(-2.099e-05)*CONDITIONAL_IMPRESSIONS)+
    (1.269e-01 + q*(1.357e-05)+(-3.865e-05)*CONDITIONAL_IMPRESSIONS)+
    (6.996e-02 + q*(3.258e-05)+(1.739e-04)*CONDITIONAL_IMPRESSIONS)+
    (1.825e-02 + q*(4.359e-06)+(-2.513e-06)*CONDITIONAL_IMPRESSIONS)+
    (4.197e-02 + q*(-1.337e-05)+(-1.407e-03)*CONDITIONAL_IMPRESSIONS)+
    (3.231e-02 + q*(-7.564e-07)+(-1.688e-05)*CONDITIONAL_IMPRESSIONS)+
    (1.043e-01+(-2.009e-05)*q+(-1.711e-03)*CONDITIONAL_IMPRESSIONS)+
    (1.758e-02 + (2.687e-06)*q+(-1.670e-06)*CONDITIONAL_IMPRESSIONS)+
    (4.464e-02 + (1.196e-05)*q + (-6.251e-06)*CONDITIONAL_IMPRESSIONS)+
    (5.508e-02 + (5.128e-06)*q+ (-2.854e-05)*CONDITIONAL_IMPRESSIONS)+
    (5.508e-02 + (5.128e-06)*q + (-2.854e-05)*CONDITIONAL_IMPRESSIONS))/15)
  }
  if (ADGROUP=="AD3_LANG3_")
  {
    return (5.508e-02 + (5.128e-06)*q+ (-2.854e-05)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD9_LANG3_")
  {
    return (5.508e-02 + (5.128e-06)*q + (-2.854e-05)*CONDITIONAL_IMPRESSIONS)
  }
  if (ADGROUP=="AD10_LANG3")
  {
    return (((1.535e-02 +q*(2.848e-06)+(-3.682e-06)*CONDITIONAL_IMPRESSIONS)+ 
       (3.388e-02 +q*(-5.747e-07)+(-2.013e-05)*CONDITIONAL_IMPRESSIONS)+
       (6.766e-02+q*(9.516e-05)+(-1.379e-04)*CONDITIONAL_IMPRESSIONS)+
       (3.714e-02+q*(1.760e-06)+(7.932e-06)*CONDITIONAL_IMPRESSIONS)+
       (2.557e-02 + q*(1.963e-06)+(-2.099e-05)*CONDITIONAL_IMPRESSIONS)+
       (1.269e-01 + q*(1.357e-05)+(-3.865e-05)*CONDITIONAL_IMPRESSIONS)+
       (6.996e-02 + q*(3.258e-05)+(1.739e-04)*CONDITIONAL_IMPRESSIONS)+
       (1.825e-02 + q*(4.359e-06)+(-2.513e-06)*CONDITIONAL_IMPRESSIONS)+
       (4.197e-02 + q*(-1.337e-05)+(-1.407e-03)*CONDITIONAL_IMPRESSIONS)+
       (3.231e-02 + q*(-7.564e-07)+(-1.688e-05)*CONDITIONAL_IMPRESSIONS)+
       (1.043e-01+(-2.009e-05)*q+(-1.711e-03)*CONDITIONAL_IMPRESSIONS)+
       (1.758e-02 + (2.687e-06)*q+(-1.670e-06)*CONDITIONAL_IMPRESSIONS)+
       (4.464e-02 + (1.196e-05)*q + (-6.251e-06)*CONDITIONAL_IMPRESSIONS)+
       (5.508e-02 + (5.128e-06)*q+ (-2.854e-05)*CONDITIONAL_IMPRESSIONS)+
       (5.508e-02 + (5.128e-06)*q + (-2.854e-05)*CONDITIONAL_IMPRESSIONS))/15)
  }
  if (ADGROUP=="AD8_LANG3_")
  {
    return (((1.535e-02 +q*(2.848e-06)+(-3.682e-06)*CONDITIONAL_IMPRESSIONS)+ 
       (3.388e-02 +q*(-5.747e-07)+(-2.013e-05)*CONDITIONAL_IMPRESSIONS)+
       (6.766e-02+q*(9.516e-05)+(-1.379e-04)*CONDITIONAL_IMPRESSIONS)+
       (3.714e-02+q*(1.760e-06)+(7.932e-06)*CONDITIONAL_IMPRESSIONS)+
       (2.557e-02 + q*(1.963e-06)+(-2.099e-05)*CONDITIONAL_IMPRESSIONS)+
       (1.269e-01 + q*(1.357e-05)+(-3.865e-05)*CONDITIONAL_IMPRESSIONS)+
       (6.996e-02 + q*(3.258e-05)+(1.739e-04)*CONDITIONAL_IMPRESSIONS)+
       (1.825e-02 + q*(4.359e-06)+(-2.513e-06)*CONDITIONAL_IMPRESSIONS)+
       (4.197e-02 + q*(-1.337e-05)+(-1.407e-03)*CONDITIONAL_IMPRESSIONS)+
       (3.231e-02 + q*(-7.564e-07)+(-1.688e-05)*CONDITIONAL_IMPRESSIONS)+
       (1.043e-01+(-2.009e-05)*q+(-1.711e-03)*CONDITIONAL_IMPRESSIONS)+
       (1.758e-02 + (2.687e-06)*q+(-1.670e-06)*CONDITIONAL_IMPRESSIONS)+
       (4.464e-02 + (1.196e-05)*q + (-6.251e-06)*CONDITIONAL_IMPRESSIONS)+
       (5.508e-02 + (5.128e-06)*q+ (-2.854e-05)*CONDITIONAL_IMPRESSIONS)+
       (5.508e-02 + (5.128e-06)*q + (-2.854e-05)*CONDITIONAL_IMPRESSIONS))/15)
  }
  
}

max_bid_function <- function(ADGROUP, Conversion_Rate)
{
  if (ADGROUP=="AD10_LANG2")
  {
    return (AD10_LANG2_APP_RATE*Conversion_Rate*AD10_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD11_LANG2")
  {
    return (AD11_LANG2_APP_RATE*Conversion_Rate*AD11_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD2_LANG3")
  {
    return (AD2_LANG3_APP_RATE*Conversion_Rate*AD2_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD2_LANG2_")
  {
    return (AD2_LANG2_APP_RATE*Conversion_Rate*AD2_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD3_LANG2_")
  {
    return (AD3_LANG2_APP_RATE*Conversion_Rate*AD3_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD4_LANG2_")
  {
    return (AD4_LANG2_APP_RATE*Conversion_Rate*AD4_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD4_LANG3_")
  {
    return (AD4_LANG3_APP_RATE*Conversion_Rate*AD4_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD5_LANG2_")
  {
    return (AD5_LANG2_APP_RATE*Conversion_Rate*AD5_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD5_LANG3_")
  {
    return (AD5_LANG3_APP_RATE*Conversion_Rate*AD5_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD6_LANG2_")
  {
    return (AD6_LANG2_APP_RATE*Conversion_Rate*AD6_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD6_LANG3_")
  {
    return (AD6_LANG3_APP_RATE*Conversion_Rate*AD6_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD8_LANG2_")
  {
    return (AD8_LANG2_APP_RATE*Conversion_Rate*AD8_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD9_LANG2_")
  {
   return (AD9_LANG2_APP_RATE*Conversion_Rate*AD9_LANG2_AVG_REV)
  }
  if (ADGROUP=="AD11_LANG3")
  {
    (AD11_LANG3_APP_RATE*Conversion_Rate*AD11_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD3_LANG3_")
  {
    return (AD3_LANG3_APP_RATE*Conversion_Rate*AD3_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD9_LANG3_")
  {
    (AD9_LANG3_APP_RATE*Conversion_Rate*AD9_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD10_LANG3")
  {
    return (AD10_LANG3_APP_RATE*Conversion_Rate*AD10_LANG3_AVG_REV)
  }
  if (ADGROUP=="AD8_LANG3_")
  {
    (AD8_LANG3_APP_RATE*Conversion_Rate*AD8_LANG3_AVG_REV)
  }
  
}


validation.data$Conversion_Rate<-
  with(validation.data, 
       conversion_rate_function(ch,exp
                                (ZERO_TOTAL_QUALITY_SCORE/ZERO_CONDITIONAL_IMPRESSIONS),
                                CONDITIONAL_IMPRESSIONS))


validation.data$Maximum_Bid<-
  with(validation.data, max_bid_function(ch, Conversion_Rate))





