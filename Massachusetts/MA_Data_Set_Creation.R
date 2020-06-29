
#
# Andrew Disher
# 3/22/2020
# Climate Change in Massachusetts
# Purpose: To create data sets for analysis
#

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

# Import the data for Massachusetts
MA_2018 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2018.csv")
MA_2017 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2017.csv")
MA_2016 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2016.csv")
MA_2015 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2015.csv")
MA_2014 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2014.csv")
MA_2013 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2013.csv")
MA_2012 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2012.csv")
MA_2011 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2011.csv")
MA_2010 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2010.csv")
MA_2009 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2009.csv")
MA_2008 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2008.csv")
MA_2007 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2007.csv")
MA_2006 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2006.csv")
MA_2005 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2005.csv")
MA_2004 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2004.csv")
MA_2003 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2003.csv")
MA_2002 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2002.csv")
MA_2001 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2001.csv")
MA_2000 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2000.csv")
MA_1999 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_1999.csv")
MA_1998 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_1998.csv")

# Import a 2019 dataset for model evaluation. 
MA_2019 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/Massachusetts/MA_2019.csv")


# Training Data Sets ------------------------------------------------------


# Bind data sets together to create a new dataset.
MA_Weather <- rbind(MA_1999, MA_2000, MA_2001, MA_2002, MA_2003, MA_2004, MA_2005, MA_2006, MA_2007, MA_2008, 
                    MA_2009, MA_2010, MA_2011, MA_2012, MA_2013, MA_2014, MA_2015, MA_2016, MA_2017, MA_2018)
MA_Weather$DATE <- as.character.Date(MA_Weather$DATE)
MA_Weather$DATE <- as.Date(MA_Weather$DATE, format = "%Y-%m-%d")
MA_Weather <- MA_Weather[, -c(1, 2)]
MA_Weather <- aggregate(data = MA_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set. 
MA_Weather <- MA_Weather %>%
  dplyr::mutate(Year = lubridate::year(MA_Weather$DATE), 
                Month = lubridate::month(MA_Weather$DATE), 
                Day = lubridate::day(MA_Weather$DATE))

MA_Weather$NewDate <- format(MA_Weather$DATE, format = "%Y-%m")
MA_Weather$NewDate <- as.yearmon(MA_Weather$NewDate)
MA_Weather_Monthly <- aggregate(data = MA_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)
MA_Weather_Monthly <- MA_Weather_Monthly[order(MA_Weather_Monthly$NewDate),]
row.names(MA_Weather_Monthly) <- 1:nrow(MA_Weather_Monthly)

# Create a monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
MA_SNWD_SNOW_Monthly <- MA_Weather_Monthly[MA_Weather_Monthly$Month %in% Period_of_Interest, ]

# Tis data set is not needed unless we choose to use total precipitation and total snow amounts for the graphs.
#
# Create a data set for only temperature measurements.
MA_Temp_Monthly <- aggregate(data = MA_Weather, cbind(TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)
MA_Temp_Monthly <- MA_Temp_Monthly[order(MA_Temp_Monthly$NewDate),]
row.names(MA_Temp_Monthly) <- 1:nrow(MA_Temp_Monthly)


# Test Data Sets ----------------------------------------------------------

MA_Weather_Evaluation <- MA_2019

MA_Weather_Evaluation$DATE <- as.character.Date(MA_Weather_Evaluation$DATE)
MA_Weather_Evaluation$DATE <- as.Date(MA_Weather_Evaluation$DATE, format = "%Y-%m-%d")
MA_Weather_Evaluation <- MA_Weather_Evaluation[, -c(1, 2)]
MA_Weather_Evaluation <- aggregate(data = MA_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set for 2019. 
MA_Weather_Evaluation <- MA_Weather_Evaluation %>%
  dplyr::mutate(Year = lubridate::year(MA_Weather_Evaluation$DATE), 
                Month = lubridate::month(MA_Weather_Evaluation$DATE), 
                Day = lubridate::day(MA_Weather_Evaluation$DATE))

MA_Weather_Evaluation$NewDate <- format(MA_Weather_Evaluation$DATE, format = "%Y-%m")
MA_Weather_Evaluation$NewDate <- as.yearmon(MA_Weather_Evaluation$NewDate)
MA_Weather_Monthly_Evaluation <- aggregate(data = MA_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a 2019 monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
MA_SNWD_SNOW_Monthly_Evaluation <- MA_Weather_Monthly_Evaluation[MA_Weather_Monthly_Evaluation$Month %in% Period_of_Interest, ]


# Preliminary Graphs ------------------------------------------------------


# Average Temperature
(ggplot(data = MA_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1)) 
  + geom_line(color = "blue") + xlab("Date") + ylab("Average Temperature (degrees Farenheit)")
  + ggtitle("Average Monthly Temperature in Massachusetts")
  + theme_bw() + geom_point(col = "red", pch = 16))

# Snow Depth
(ggplot(data = MA_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snow Depth (inches)")
  + ggtitle("Average Monthly Snow Depth in Massachusetts")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snowfall
(ggplot(data = MA_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snowfall (inches)")
  + ggtitle("Average Monthly Snowfall in Massachusetts")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Precipitation
(ggplot(data = MA_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Rainfall (inches)")
  + ggtitle("Average Monthly Rainfall in Massachusetts")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))






