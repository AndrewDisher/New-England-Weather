
#
# Andrew Disher
# 4/5/2020
# Summer ATP Project
# Massachusetts ARIMA Model for Maximum Temperature
#

library(ggplot2)
library(forecast)
library(car)
library(tseries)
library(dygraphs)
library(xts)

# Original SNWD Time Series -----------------------------------------------

# Create time series plot.
(ggplot(data = MA_Temp_Monthly, aes(x = NewDate, y = TMAX, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Maximum Daily Temperature in Massachusetts by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(MA_Temp_Monthly$TMAX, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(MA_Temp_Monthly$TMAX, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(MA_Temp_Monthly$TMAX ~ MA_Temp_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose no lambda


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
MA_TMAX_Seasonal <- MA_Temp_Monthly
MA_TMAX_Seasonal$TMAX <- c(array(NA, dim = c(12, 1)), diff(MA_TMAX_Seasonal$TMAX, 12))
MA_TMAX_Seasonal <- MA_TMAX_Seasonal[-c(1:12), ]

# Create time series plot for deseasonalized data.
(ggplot(data = MA_TMAX_Seasonal, aes(x = NewDate, y = TMAX, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Maximum Daily Temperature in Massachusetts by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(MA_TMAX_Seasonal$TMAX, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(MA_TMAX_Seasonal$TMAX, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(MA_TMAX_Seasonal$TMAX, null = "Level") # Stationary
kpss.test(MA_TMAX_Seasonal$TMAX, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(MA_TMAX_Seasonal$TMAX) # Stationary


# Model Creation and Diagnostics ------------------------------------------

# Create a model for the data. 
MA_TMAX_fit <- arima(MA_Temp_Monthly$TMAX, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
MA_TMAX_fit

# Fiding BIC.
AIC(MA_TMAX_fit, k = log(length(MA_Temp_Monthly$TMAX)))

# Confirm Stationarity with Unit Root Graph.
autoplot(MA_TMAX_fit) # Stationary

# Diagnostics Plots.
residuals_MA_TMAX_fit <- as.vector(residuals(MA_TMAX_fit))
predicted_MA_TMAX_fit <- as.vector(fitted(MA_TMAX_fit))

qqnorm(residuals_MA_TMAX_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_MA_TMAX_fit, datax = TRUE, col = "red")
plot(predicted_MA_TMAX_fit, residuals_MA_TMAX_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_MA_TMAX_fit, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_MA_TMAX_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_MA_TMAX_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_MA_TMAX <- acf(residuals_MA_TMAX_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_MA_TMAX <- acf(residuals_MA_TMAX_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
# NOTE: Where it says BP and Box-Pierce, this is a misnomer. The statistic used was an Ljung-Box statistic. 
# This remains true for all of the subsequent models created in the sections for problem 6.2.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(residuals(MA_TMAX_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_TMAX <- as.array(cbind(Res_ACF_MA_TMAX$lag[-1], Res_ACF_MA_TMAX$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_MA_TMAX$lag, Res_PACF_MA_TMAX$acf))
colnames(Ljung_Box_TMAX)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_TMAX <- as.data.frame(Ljung_Box_TMAX)

plot(Ljung_Box_TMAX$Lag, Ljung_Box_TMAX$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')


# Model Forecasting and Evaluation ----------------------------------------

# Obtain daily forecasts.
MA_TMAX_Forecasts <- forecast(MA_TMAX_fit, h = 12, level = 95)
MA_TMAX_Forecasts

# Plot the forecasts. 
(autoplot(ts(data = MA_Temp_Monthly$TMAX), series = "Data") + autolayer(fitted(MA_TMAX_fit), series = "ARIMA(1,0,0) x (0,1,1)")
  + autolayer(MA_TMAX_Forecasts, series = "Forecast")
  + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(1,0,0) x (0,1,1)'='#E30022', 'Forecast'='blue'),
                        breaks = c('Data', 'ARIMA(1,0,0) x (0,1,1)', 'Forecast'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Temperature (degrees Farenheit)') + theme_bw()
  + ggtitle("Maximum Daily Temperature in Massachusetts Forecasts by Month") + theme(legend.position = "bottom"))


# Yet ANOTHER way to graph the data, but forecasts ONLY.
dyTMAX = xts(as.data.frame(cbind(gen_array(MA_TMAX_Forecasts), MA_Temp_Monthly_Evaluation$TMAX)),
                           order.by = seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "months"))
colnames(dyTMAX) <- c("Lo 95", "Hi 95","Point Forecast", "TMAX")


dygraph(dyTMAX, main = 'Maximum Daily Temperature in Massachusetts Forecasts by Month', 
        xlab = 'Date', ylab = 'Temperature (degrees Farenheit)') %>%
  dySeries("TMAX", label = "2019 Data", col = "orange") %>%
  dySeries(c("Lo 95", "Point Forecast", "Hi 95"), label = c("Forecast"), col = "red")
  



# Functions ---------------------------------------------------------------


gen_array <- function(forecast_obj){
  
  lower <- forecast_obj$lower[,1]
  upper <- forecast_obj$upper[,1]
  point_forecast <- forecast_obj$mean
  
  cbind(lower, upper, point_forecast)
}
