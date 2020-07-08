
#
# Andrew Disher
# 4/5/2020
# Summer ATP Project
# Massachusetts ARIMA Model for Minimum Temperature
#

library(ggplot2)
library(forecast)
library(car)
library(tseries)
library(dygraphs)
library(xts)

# Original SNWD Time Series -----------------------------------------------

# Create time series plot.
(ggplot(data = MA_Temp_Monthly, aes(x = NewDate, y = TMIN, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Minimum Daily Temperature in Massachusetts by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(MA_Temp_Monthly$TMIN, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(MA_Temp_Monthly$TMIN, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(MA_Temp_Monthly$TMIN ~ MA_Temp_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose no lambda


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
MA_TMIN_Seasonal <- MA_Temp_Monthly
MA_TMIN_Seasonal$TMIN <- c(array(NA, dim = c(12, 1)), diff(MA_TMIN_Seasonal$TMIN, 12))
MA_TMIN_Seasonal <- MA_TMIN_Seasonal[-c(1:12), ]

# Create time series plot for deseasonalized data.
(ggplot(data = MA_TMIN_Seasonal, aes(x = NewDate, y = TMIN, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Minimum Daily Temperature in Massachusetts by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(MA_TMIN_Seasonal$TMIN, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(MA_TMIN_Seasonal$TMIN, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(MA_TMIN_Seasonal$TMIN, null = "Level") # Stationary
kpss.test(MA_TMIN_Seasonal$TMIN, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(MA_TMIN_Seasonal$TMIN) # Stationary


# Model Creation and Diagnostics ------------------------------------------

# Create a model for the data. 
MA_TMIN_fit <- arima(MA_Temp_Monthly$TMIN, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
MA_TMIN_fit

# Fiding BIC.
AIC(MA_TMIN_fit, k = log(length(MA_Temp_Monthly$TMIN)))

# Confirm Stationarity with Unit Root Graph.
autoplot(MA_TMIN_fit) # Stationary

# Diagnostics Plots.
residuals_MA_TMIN_fit <- as.vector(residuals(MA_TMIN_fit))
predicted_MA_TMIN_fit <- as.vector(fitted(MA_TMIN_fit))

qqnorm(residuals_MA_TMIN_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_MA_TMIN_fit, datax = TRUE, col = "red")
plot(predicted_MA_TMIN_fit, residuals_MA_TMIN_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_MA_TMIN_fit, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_MA_TMIN_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_MA_TMIN_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_MA_TMIN <- acf(residuals_MA_TMIN_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_MA_TMIN <- acf(residuals_MA_TMIN_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
# NOTE: Where it says BP and Box-Pierce, this is a misnomer. The statistic used was an Ljung-Box statistic. 
# This remains true for all of the subsequent models created in the sections for problem 6.2.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(residuals(MA_TMIN_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_TMIN <- as.array(cbind(Res_ACF_MA_TMIN$lag[-1], Res_ACF_MA_TMIN$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_MA_TMIN$lag, Res_PACF_MA_TMIN$acf))
colnames(Ljung_Box_TMIN)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_TMIN <- as.data.frame(Ljung_Box_TMIN)

plot(Ljung_Box_TMIN$Lag, Ljung_Box_TMIN$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')


# Model Forecasting and Evaluation ----------------------------------------

# Obtain daily forecasts.
MA_TMIN_Forecasts <- as.array(forecast(MA_TMIN_fit, h = 12, level = 95))
MA_TMIN_Forecasts

# Plot the forecasts. 
(autoplot(ts(data = MA_Temp_Monthly$TMIN), series = "Data") + autolayer(fitted(MA_TMIN_fit), series = "ARIMA(1,0,0) x (0,1,1)")
  + autolayer(MA_TMIN_Forecasts, series = "Forecast")
  + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(1,0,0) x (0,1,1)'='#E30022', 'Forecast'='blue'),
                        breaks = c('Data', 'ARIMA(1,0,0) x (0,1,1)', 'Forecast'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Temperature (degrees Farenheit)') + theme_bw()
  + ggtitle("Minimum Daily Temperature in Massachusetts Forecasts by Month") + theme(legend.position = "bottom"))


# Yet ANOTHER way to graph the data, but forecasts ONLY.
dyTMIN = xts(as.data.frame(cbind(gen_array(MA_TMIN_Forecasts), MA_Temp_Monthly_Evaluation$TMIN)),
             order.by = seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "months"))
colnames(dyTMIN) <- c("Lo 95", "Hi 95","Point Forecast", "TMIN")

dygraph(dyTMIN, main = 'Minimum Daily Temperature in Massachusetts Forecasts by Month ', 
        xlab = 'Date', ylab = 'Temperature (degrees Farenheit)') %>%
  dySeries("TMIN", label = "2019 Data", color = "#330019") %>%
  dySeries(c("Lo 95", "Point Forecast", "Hi 95"), label = c("Forecast"), col = '#6666FF')



# Functions ---------------------------------------------------------------


gen_array <- function(forecast_obj){
  
  lower <- forecast_obj$lower[,1]
  upper <- forecast_obj$upper[,1]
  point_forecast <- forecast_obj$mean
  
  cbind(lower, upper, point_forecast)
}


