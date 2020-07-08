
#
# Andrew Disher
# 3/22/2020
# Climate Change in Vermont
# Purpose: To create data sets for analysis
#

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

# Import the data for Vermont.
VT_2018 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2018.csv")
VT_2017 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2017.csv")
VT_2016 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2016.csv")
VT_2015 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2015.csv")
VT_2014 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2014.csv")
VT_2013 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2013.csv")
VT_2012 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2012.csv")
VT_2011 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2011.csv")
VT_2010 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2010.csv")
VT_2009 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2009.csv")
VT_2008 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2008.csv")
VT_2007 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2007.csv")
VT_2006 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2006.csv")
VT_2005 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2005.csv")
VT_2004 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2004.csv")
VT_2003 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2003.csv")
VT_2002 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2002.csv")
VT_2001 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2001.csv")
VT_2000 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_2000.csv")
VT_1999 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Vermont/VT_1999.csv")
VT_2019 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/GHCN_Daily_Data_Base/Vermont/VT_2019.csv") 

# Bind data sets together to create a new dataset.
VT_Weather <- rbind(VT_1999, VT_2000, VT_2001, VT_2002, VT_2003, VT_2004, VT_2005, VT_2006, VT_2007, VT_2008, 
                    VT_2009, VT_2010, VT_2011, VT_2012, VT_2013, VT_2014, VT_2015, VT_2016, VT_2017, VT_2018)
VT_Weather$DATE <- as.character.Date(VT_Weather$DATE)
VT_Weather$DATE <- as.Date(VT_Weather$DATE, format = "%Y-%m-%d")
VT_Weather <- VT_Weather[, -c(1, 2)]
VT_Weather <- aggregate(data = VT_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set. 
VT_Weather <- VT_Weather %>%
  dplyr::mutate(Year = lubridate::year(VT_Weather$DATE), 
                Month = lubridate::month(VT_Weather$DATE), 
                Day = lubridate::day(VT_Weather$DATE))

VT_Weather$NewDate <- format(VT_Weather$DATE, format = "%Y-%m")
VT_Weather$NewDate <- as.yearmon(VT_Weather$NewDate)
VT_Weather_Monthly <- aggregate(data = VT_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, mean)
VT_Weather_Monthly <- VT_Weather_Monthly[order(VT_Weather_Monthly$NewDate),]
row.names(VT_Weather_Monthly) <- 1:nrow(VT_Weather_Monthly)

# Create a monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
VT_SNWD_SNOW_Monthly <- VT_Weather_Monthly[VT_Weather_Monthly$Month %in% Period_of_Interest, ]


# Test Data Sets ----------------------------------------------------------


VT_Weather_Evaluation <- VT_2019

VT_Weather_Evaluation$DATE <- as.character.Date(VT_Weather_Evaluation$DATE)
VT_Weather_Evaluation$DATE <- as.Date(VT_Weather_Evaluation$DATE, format = "%Y-%m-%d")
VT_Weather_Evaluation <- VT_Weather_Evaluation[, -c(1, 2)]
VT_Weather_Evaluation <- aggregate(data = VT_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set for 2019. 
VT_Weather_Evaluation <- VT_Weather_Evaluation %>%
  dplyr::mutate(Year = lubridate::year(VT_Weather_Evaluation$DATE), 
                Month = lubridate::month(VT_Weather_Evaluation$DATE), 
                Day = lubridate::day(VT_Weather_Evaluation$DATE))

VT_Weather_Evaluation$NewDate <- format(VT_Weather_Evaluation$DATE, format = "%Y-%m")
VT_Weather_Evaluation$NewDate <- as.yearmon(VT_Weather_Evaluation$NewDate)
VT_Weather_Monthly_Evaluation <- aggregate(data = VT_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a 2019 monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
VT_SNWD_SNOW_Monthly_Evaluation <- VT_Weather_Monthly_Evaluation[VT_Weather_Monthly_Evaluation$Month %in% Period_of_Interest, ]


# Preliminary Graphs ------------------------------------------------------

# Average Temperature
(ggplot(data = VT_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1)) 
  + geom_line(color = "#006633") + xlab("Date") + ylab("Average Temperature (degrees Farenheit)")
  + ggtitle("Average Monthly Temperature in Vermont")
  + theme_bw() + geom_point(col = "blue", pch = 16))

# Snow Depth
(ggplot(data = VT_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snow Depth (inches)")
  + ggtitle("Average Monthly Snow Depth in Vermont")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snowfall
(ggplot(data = VT_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snowfall (inches)")
  + ggtitle("Average Monthly Snowfall in Vermont")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Precipitation
(ggplot(data = VT_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Rainfall (inches)")
  + ggtitle("Average Monthly Rainfall in Vermont")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))






