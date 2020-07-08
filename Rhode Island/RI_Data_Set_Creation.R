
#
# Andrew Disher
# 3/26/2020
# Climate Change in Rhode Island
# Purpose: To create data sets for analysis
#

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

# Import the data for Rhode Island.
RI_2018 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2018.csv")
RI_2017 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2017.csv")
RI_2016 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2016.csv")
RI_2015 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2015.csv")
RI_2014 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2014.csv")
RI_2013 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2013.csv")
RI_2012 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2012.csv")
RI_2011 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2011.csv")
RI_2010 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2010.csv")
RI_2009 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2009.csv")
RI_2008 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2008.csv")
RI_2007 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2007.csv")
RI_2006 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2006.csv")
RI_2005 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2005.csv")
RI_2004 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2004.csv")
RI_2003 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2003.csv")
RI_2002 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2002.csv")
RI_2001 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2001.csv")
RI_2000 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_2000.csv")
RI_1999 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Rhode Island/RI_1999.csv")
RI_2019 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/GHCN_Daily_Data_Base/Rhode Island/RI_2019.csv")


# Bind data sets together to create a new dataset.
RI_Weather <- rbind(RI_1999, RI_2000, RI_2001, RI_2002, RI_2003, RI_2004, RI_2005, RI_2006, RI_2007, RI_2008, 
                    RI_2009, RI_2010, RI_2011, RI_2012, RI_2013, RI_2014, RI_2015, RI_2016, RI_2017, RI_2018)
RI_Weather$DATE <- as.character.Date(RI_Weather$DATE)
RI_Weather$DATE <- as.Date(RI_Weather$DATE, format = "%Y-%m-%d")
RI_Weather <- RI_Weather[, -c(1, 2)]
RI_Weather <- aggregate(data = RI_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set. 
RI_Weather <- RI_Weather %>%
  dplyr::mutate(Year = lubridate::year(RI_Weather$DATE), 
                Month = lubridate::month(RI_Weather$DATE), 
                Day = lubridate::day(RI_Weather$DATE))

RI_Weather$NewDate <- format(RI_Weather$DATE, format = "%Y-%m")
RI_Weather$NewDate <- as.yearmon(RI_Weather$NewDate)
RI_Weather_Monthly <- aggregate(data = RI_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, mean, na.rm = TRUE, na.action = NULL)

# Sort data set by the NewDate column.
RI_Weather_Monthly <-  RI_Weather_Monthly[order(RI_Weather_Monthly$NewDate), ]
row.names(RI_Weather_Monthly) <- 1:nrow(RI_Weather_Monthly)

# Create a monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
RI_SNWD_SNOW_Monthly <- RI_Weather_Monthly[RI_Weather_Monthly$Month %in% Period_of_Interest, ]


### Important NOTE: The average temperature variable in our monthly data set has many missing values because there 
### was not enough data in the orginal datasets to get a meaningful number when I first aggregated RI_Weather. 
### Many entries in TAVG ended up being NaN, which was creating issues when I aggregated again to acquire the 
### RI_Weather_Monthly data set, so we will be unable to use TAVG. All other variables are usable.
### UPDATE: Issue has been corrected.


# Test Data Sets ----------------------------------------------------------


RI_Weather_Evaluation <- RI_2019

RI_Weather_Evaluation$DATE <- as.character.Date(RI_Weather_Evaluation$DATE)
RI_Weather_Evaluation$DATE <- as.Date(RI_Weather_Evaluation$DATE, format = "%Y-%m-%d")
RI_Weather_Evaluation <- RI_Weather_Evaluation[, -c(1, 2)]
RI_Weather_Evaluation <- aggregate(data = RI_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set for 2019. 
RI_Weather_Evaluation <- RI_Weather_Evaluation %>%
  dplyr::mutate(Year = lubridate::year(RI_Weather_Evaluation$DATE), 
                Month = lubridate::month(RI_Weather_Evaluation$DATE), 
                Day = lubridate::day(RI_Weather_Evaluation$DATE))

RI_Weather_Evaluation$NewDate <- format(RI_Weather_Evaluation$DATE, format = "%Y-%m")
RI_Weather_Evaluation$NewDate <- as.yearmon(RI_Weather_Evaluation$NewDate)
RI_Weather_Monthly_Evaluation <- aggregate(data = RI_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a 2019 monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
RI_SNWD_SNOW_Monthly_Evaluation <- RI_Weather_Monthly_Evaluation[RI_Weather_Monthly_Evaluation$Month %in% Period_of_Interest, ]



# Preliminary Graphs ------------------------------------------------------

# Average Temperature
(ggplot(data = RI_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Average Temperature (degrees Farenheit)")
  + ggtitle("Average Monthly Temperature in Rhode Island")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snow Depth
(ggplot(data = RI_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snow Depth (inches)")
  + ggtitle("Average Monthly Snow Depth in Rhode Island")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snowfall
(ggplot(data = RI_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snowfall (inches)")
  + ggtitle("Average Monthly Snowfall in Rhode Island")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Precipitation
(ggplot(data = RI_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Rainfall (inches)")
  + ggtitle("Average Monthly Rainfall in Rhode Island")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))



