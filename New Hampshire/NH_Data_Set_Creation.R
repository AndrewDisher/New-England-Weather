
#
# Andrew Disher
# 3/27/2020
# Climate Change in New Hampshire
# Purpose: To create data sets for analysis
#

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

NH_2017_2018 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2017_2018.csv")
NH_2015_2016 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2015_2016.csv")
NH_2013_2014 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2013_2014.csv")
NH_2011_2012 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2011_2012.csv")
NH_2009_2010 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2009_2010.csv")
NH_2007_2008 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2007_2008.csv")
NH_2005_2006 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2005_2006.csv")
NH_2003_2004 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2003_2004.csv")
NH_2001_2002 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_2001_2002.csv")
NH_1999_2000 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/New Hampshire/NH_1999_2000.csv")
NH_2019 <- read.csv("~/BSU Stuff/Summer ATP Project/Climate Datasets/GHCN_Daily_Data_Base/New Hampshire/NH_2019.csv")


# Training Data Sets ------------------------------------------------------


# Bind data sets together to create a new dataset.
NH_Weather <- rbind(NH_2017_2018, NH_2015_2016, NH_2013_2014, NH_2011_2012, NH_2009_2010, NH_2007_2008,
                    NH_2005_2006, NH_2003_2004, NH_2001_2002, NH_1999_2000)
NH_Weather$DATE <- as.character.Date(NH_Weather$DATE)
NH_Weather$DATE <- as.Date(NH_Weather$DATE, format = "%Y-%m-%d")
NH_Weather <- NH_Weather[, -c(1, 2)]
NH_Weather <- aggregate(data = NH_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set. 
NH_Weather <- NH_Weather %>%
  dplyr::mutate(Year = lubridate::year(NH_Weather$DATE), 
                Month = lubridate::month(NH_Weather$DATE), 
                Day = lubridate::day(NH_Weather$DATE))

NH_Weather$NewDate <- format(NH_Weather$DATE, format = "%Y-%m")
NH_Weather$NewDate <- as.yearmon(NH_Weather$NewDate)
NH_Weather_Monthly <- aggregate(data = NH_Weather, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, mean, na.rm = TRUE, na.action = NULL)

# Sort data set by the NewDate column.
NH_Weather_Monthly <-  NH_Weather_Monthly[order(NH_Weather_Monthly$NewDate), ]
row.names(NH_Weather_Monthly) <- 1:nrow(NH_Weather_Monthly)

# Create a monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
NH_SNWD_SNOW_Monthly <- NH_Weather_Monthly[NH_Weather_Monthly$Month %in% Period_of_Interest, ]


# Test Data Sets ----------------------------------------------------------


NH_Weather_Evaluation <- NH_2019

NH_Weather_Evaluation$DATE <- as.character.Date(NH_Weather_Evaluation$DATE)
NH_Weather_Evaluation$DATE <- as.Date(NH_Weather_Evaluation$DATE, format = "%Y-%m-%d")
NH_Weather_Evaluation <- NH_Weather_Evaluation[, -c(1, 2)]
NH_Weather_Evaluation <- aggregate(data = NH_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ DATE, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a Monthly Data Set for 2019. 
NH_Weather_Evaluation <- NH_Weather_Evaluation %>%
  dplyr::mutate(Year = lubridate::year(NH_Weather_Evaluation$DATE), 
                Month = lubridate::month(NH_Weather_Evaluation$DATE), 
                Day = lubridate::day(NH_Weather_Evaluation$DATE))

NH_Weather_Evaluation$NewDate <- format(NH_Weather_Evaluation$DATE, format = "%Y-%m")
NH_Weather_Evaluation$NewDate <- as.yearmon(NH_Weather_Evaluation$NewDate)
NH_Weather_Monthly_Evaluation <- aggregate(data = NH_Weather_Evaluation, cbind(PRCP, SNOW, SNWD, TAVG, TMAX, TMIN) ~ NewDate + Month, FUN = mean, na.rm = TRUE, na.action = NULL)

# Create a 2019 monthly data set for SNWD and SNOW from the beginning of October until the end of May (8 month period)
# of all 20 years.
Period_of_Interest <- c(1:5, 10:12)
NH_SNWD_SNOW_Monthly_Evaluation <- NH_Weather_Monthly_Evaluation[NH_Weather_Monthly_Evaluation$Month %in% Period_of_Interest, ]


# Preliminary Graphs ------------------------------------------------------


# Average Temperature
(ggplot(data = NH_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Average Temperature (degrees Farenheit)")
  + ggtitle("Average Monthly Temperature in New Hampshire")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snow Depth
(ggplot(data = NH_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snow Depth (inches)")
  + ggtitle("Average Monthly Snow Depth in New Hampshire")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Snowfall
(ggplot(data = NH_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Snowfall (inches)")
  + ggtitle("Average Monthly Snowfall in New Hampshire")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))

# Precipitation
(ggplot(data = NH_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1)) 
  + geom_line(color = "#99004C") + xlab("Date") + ylab("Rainfall (inches)")
  + ggtitle("Average Monthly Rainfall in New Hampshire")
  + theme_bw() + geom_point(col = "#00CCCC", pch = 16))






