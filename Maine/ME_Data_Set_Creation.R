
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

# Import the data for Maine.
ME_2018 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2018.csv")
ME_2017 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2017.csv")
ME_2016 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2016.csv")
ME_2015 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2015.csv")
ME_2014 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2014.csv")
ME_2013 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2013.csv")
ME_2012 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2012.csv")
ME_2011 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2011.csv")
ME_2010 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2010.csv")
ME_2009 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2009.csv")
ME_2008 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2008.csv")
ME_2007 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2007.csv")
ME_2006 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2006.csv")
ME_2005 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2005.csv")
ME_2004 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2004.csv")
ME_2003 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2003.csv")
ME_2002 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2002.csv")
ME_2001 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2001.csv")
ME_2000 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_2000.csv")
ME_1999 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Maine/ME_1999.csv")
ME_2019 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/GHCN_Daily_Data_Base/Maine/ME_2019.csv")


# Bind data sets together to create a new dataset.
ME_Weather <- rbind(ME_1999, ME_2000, ME_2001, ME_2002, ME_2003, ME_2004, ME_2005, ME_2006, ME_2007, ME_2008, 
                    ME_2009, ME_2010, ME_2011, ME_2012, ME_2013, ME_2014, ME_2015, ME_2016, ME_2017, ME_2018)
ME_Weather$DATE <- as.character.Date(ME_Weather$DATE)
ME_Weather$DATE <- as.Date(ME_Weather$DATE, format = "%Y-%m-%d")
ME_Weather <- ME_Weather[, -c(1, 2)]
ME_Weather <- aggregate(data = ME_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set. 
ME_Weather <- ME_Weather %>%
  dplyr::mutate(Year = lubridate::year(ME_Weather$DATE), 
                Month = lubridate::month(ME_Weather$DATE), 
                Day = lubridate::day(ME_Weather$DATE))

ME_Weather$NewDate <- format(ME_Weather$DATE, format = "%Y-%m")
ME_Weather$NewDate <- as.yearmon(ME_Weather$NewDate)
ME_Weather_Monthly <- aggregate(data = ME_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, mean)
ME_Weather_Monthly <- ME_Weather_Monthly[order(ME_Weather_Monthly$NewDate),]
row.names(ME_Weather_Monthly) <- 1:nrow(ME_Weather_Monthly)

# Create a monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
ME_SNWD_SNOW_Monthly <- ME_Weather_Monthly[ME_Weather_Monthly$Month %in% Period_of_Interest, ]


# Test Data Sets ----------------------------------------------------------


ME_Weather_Evaluation <- ME_2019

ME_Weather_Evaluation$DATE <- as.character.Date(ME_Weather_Evaluation$DATE)
ME_Weather_Evaluation$DATE <- as.Date(ME_Weather_Evaluation$DATE, format = "%Y-%m-%d")
ME_Weather_Evaluation <- ME_Weather_Evaluation[, -c(1, 2)]
ME_Weather_Evaluation <- aggregate(data = ME_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set for 2019. 
ME_Weather_Evaluation <- ME_Weather_Evaluation %>%
  dplyr::mutate(Year = lubridate::year(ME_Weather_Evaluation$DATE), 
                Month = lubridate::month(ME_Weather_Evaluation$DATE), 
                Day = lubridate::day(ME_Weather_Evaluation$DATE))

ME_Weather_Evaluation$NewDate <- format(ME_Weather_Evaluation$DATE, format = "%Y-%m")
ME_Weather_Evaluation$NewDate <- as.yearmon(ME_Weather_Evaluation$NewDate)
ME_Weather_Monthly_Evaluation <- aggregate(data = ME_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a 2019 monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
ME_SNWD_SNOW_Monthly_Evaluation <- ME_Weather_Monthly_Evaluation[ME_Weather_Monthly_Evaluation$Month %in% Period_of_Interest, ]


# Preliminary Graphs ------------------------------------------------------

# Average Temperature
(ggplot(data = ME_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Average Temperature (degrees Farenheit)")
  + ggtitle("Average Monthly Temperature in Maine")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))
 
# Snow Depth
(ggplot(data = ME_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snow Depth (inches)")
  + ggtitle("Average Monthly Snow Depth in Maine")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snowfall
(ggplot(data = ME_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snowfall (inches)")
  + ggtitle("Average Monthly Snowfall in Maine")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Precipitation
(ggplot(data = ME_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Rainfall (inches)")
  + ggtitle("Average Monthly Rainfall in Maine")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))








