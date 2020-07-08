
#
# Andrew Disher
# 3/27/2020
# Climate Change in Connecticut
# Purpose: To create data sets for analysis
#

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

CT_2017_2018 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2017_2018.csv")
CT_2015_2016 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2015_2016.csv")
CT_2013_2014 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2013_2014.csv")
CT_2011_2012 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2011_2012.csv")
CT_2009_2010 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2009_2010.csv")
CT_2007_2008 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2007_2008.csv")
CT_2005_2006 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2005_2006.csv")
CT_2003_2004 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2003_2004.csv")
CT_2001_2002 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_2001_2002.csv")
CT_1999_2000 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Connecticut/CT_1999_2000.csv")
CT_2019 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/GHCN_Daily_Data_Base/Connecticut/CT_2019.csv")

# Bind data sets together to create a new dataset.
CT_Weather <- rbind(CT_2017_2018, CT_2015_2016, CT_2013_2014, CT_2011_2012, CT_2009_2010, CT_2007_2008,
                    CT_2005_2006, CT_2003_2004, CT_2001_2002, CT_1999_2000)
CT_Weather$DATE <- as.character.Date(CT_Weather$DATE)
CT_Weather$DATE <- as.Date(CT_Weather$DATE, format = "%Y-%m-%d")
CT_Weather <- CT_Weather[, -c(1, 2)]
CT_Weather <- aggregate(data = CT_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set. 
CT_Weather <- CT_Weather %>%
  dplyr::mutate(Year = lubridate::year(CT_Weather$DATE), 
                Month = lubridate::month(CT_Weather$DATE), 
                Day = lubridate::day(CT_Weather$DATE))

CT_Weather$NewDate <- format(CT_Weather$DATE, format = "%Y-%m")
CT_Weather$NewDate <- as.yearmon(CT_Weather$NewDate)
CT_Weather_Monthly <- aggregate(data = CT_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, mean, na.rm = TRUE, na.action = NULL)

# Sort data set by the NewDate column.
CT_Weather_Monthly <-  CT_Weather_Monthly[order(CT_Weather_Monthly$NewDate), ]
row.names(CT_Weather_Monthly) <- 1:nrow(CT_Weather_Monthly)

# Create a monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
CT_SNWD_SNOW_Monthly <- CT_Weather_Monthly[CT_Weather_Monthly$Month %in% Period_of_Interest, ]


# NOTE: There are no data values for TAVG from about 2005 to 2013, so when a time series plot of TAVG is 
# created there appears a large gap where the data should be. This also appears in the Rhode Island Data.
# UPDATE: Issue has been corrected. 


# Test Data Sets ----------------------------------------------------------


CT_Weather_Evaluation <- CT_2019

CT_Weather_Evaluation$DATE <- as.character.Date(CT_Weather_Evaluation$DATE)
CT_Weather_Evaluation$DATE <- as.Date(CT_Weather_Evaluation$DATE, format = "%Y-%m-%d")
CT_Weather_Evaluation <- CT_Weather_Evaluation[, -c(1, 2)]
CT_Weather_Evaluation <- aggregate(data = CT_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set for 2019. 
CT_Weather_Evaluation <- CT_Weather_Evaluation %>%
  dplyr::mutate(Year = lubridate::year(CT_Weather_Evaluation$DATE), 
                Month = lubridate::month(CT_Weather_Evaluation$DATE), 
                Day = lubridate::day(CT_Weather_Evaluation$DATE))

CT_Weather_Evaluation$NewDate <- format(CT_Weather_Evaluation$DATE, format = "%Y-%m")
CT_Weather_Evaluation$NewDate <- as.yearmon(CT_Weather_Evaluation$NewDate)
CT_Weather_Monthly_Evaluation <- aggregate(data = CT_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a 2019 monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
CT_SNWD_SNOW_Monthly_Evaluation <- CT_Weather_Monthly_Evaluation[CT_Weather_Monthly_Evaluation$Month %in% Period_of_Interest, ]




# Preliminary Graphs ------------------------------------------------------

# Average Temperature
(ggplot(data = CT_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Temperature (degrees Farenheit)")
  + ggtitle("Average Monthly Temperature in Connecticut")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snow Depth
(ggplot(data = CT_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snow Depth (inches)")
  + ggtitle("Average Monthly Snow Depth in Connecticut")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snowfall
(ggplot(data = CT_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snowfall (inches)")
  + ggtitle("Average Monthly Snowfall in Connecticut")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Precipitation
(ggplot(data = CT_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Rainfall (inches)")
  + ggtitle("Average Monthly Rainfall in Connecticut")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))








