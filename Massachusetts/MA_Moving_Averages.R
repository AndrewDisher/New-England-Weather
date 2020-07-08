
#
# Andrew Disher
# 3/27/2020
# Climate Change in Massachusetts
# Purpose: To Analyze Weather Data
#

library(zoo) # Includes the rollmean() function for finding the simple moving average of a time series.
library(ggplot2)
library(dplyr) # Includes cummean() function for finding the cumulative moving average of a time series. 


# Moving Averages ---------------------------------------------------------

# Obtain simple moving averages for various weather metrics and plot them against their respective time series.
(ggplot() 
 + geom_line(data = MA_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1, color = "Precipitation"), size = 1) 
 + geom_line(data = MA_Weather_Monthly[5:240, ], aes(x = NewDate, y = rollmean(MA_Weather_Monthly$PRCP, k = 5), group = 1, color = "Moving Average"), size = 1)
 + xlab("Date") + ylab("Precipitation (inches)") + ggtitle("Average Daily Precipitation in Massachusetts by Month")
 + theme_bw() + guides(color = guide_legend(title = "", title.position = "left"))
 + theme(legend.position = "bottom"))

(ggplot() 
  + geom_line(data = MA_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1, color = "Snowfall"), size = 1) 
  + geom_line(data = MA_Weather_Monthly[5:240, ], aes(x = NewDate, y = rollmean(MA_Weather_Monthly$SNOW, k = 5), group = 1, color = "Moving Average"), size = 1)
  + xlab("Date") + ylab("Snowfall") + ggtitle("Average Daily Snowfall in Massachusetts by Month")
  + theme_bw() + guides(color = guide_legend(title = "", title.position = "left"))
  + theme(legend.position = "bottom"))

(ggplot() 
  + geom_line(data = MA_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1, color = "Snow Depth"), size = 1) 
  + geom_line(data = MA_Weather_Monthly[3:240, ], aes(x = NewDate, y = rollmean(MA_Weather_Monthly$SNWD, k = 3), group = 1, color = "Moving Average"), size = 1)
  + xlab("Date") + ylab("Snow Depth") + ggtitle("Average Daily Snow Depth in Massachusetts by Month")
  + theme_bw() + guides(color = guide_legend(title = "", title.position = "left"))
  + theme(legend.position = "bottom"))

(ggplot() 
  + geom_line(data = MA_Temp_Monthly, aes(x = NewDate, y = TMAX, group = 1, color = "Max Temperature"), size = 1) 
  + geom_line(data = MA_Weather_Monthly[5:240, ], aes(x = NewDate, y = rollmean(MA_Weather_Monthly$TMAX, k = 5), group = 1, color = "Moving Average"), size = 1)
  + xlab("Date") + ylab("Temperature") + ggtitle("Average Daily Maximum Temperature in Massachusetts by Month")
  + theme_bw() + guides(color = guide_legend(title = "", title.position = "left"))
  + theme(legend.position = "bottom"))

(ggplot() 
  + geom_line(data = MA_Temp_Monthly   , aes(x = NewDate, y = TMIN, group = 1, color = "Min Temperature"), size = 1) 
  + geom_line(data = MA_Weather_Monthly[5:240, ], aes(x = NewDate, y = rollmean(MA_Weather_Monthly$TMIN, k = 5), group = 1, color = "Moving Average"), size = 1)
  + xlab("Date") + ylab("Temperature") + ggtitle("Average Daily Minimum Temperature in Massachusetts by Month")
  + theme_bw() + guides(color = guide_legend(title = "", title.position = "left"))
  + theme(legend.position = "bottom"))


