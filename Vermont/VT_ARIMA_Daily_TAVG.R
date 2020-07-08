
#
# Andrew Disher
# 4/5/2020
# Summer ATP Project
# Vermont ARIMA Model for Average Temperature
#

library(ggplot2)
library(forecast)
library(car)
library(tseries)
library(dygraphs)
library(xts)
library(lmtest)
library(uroot)


# Original Time Series -----------------------------------------------

# Create time series plot.
(ggplot(data = VT_Weather, aes(x = DATE, y = TAVG, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Average Daily Temperature in Vermont by Month")
 + theme_bw())

# Create ACF and PACF for time series. 
acf(VT_Weather$TAVG, lag.max = 730, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(VT_Weather$TAVG, lag.max = 730, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox((VT_Weather$TAVG + 15) ~ VT_Weather$DATE, lambda = seq(-3, 3, by = 1)) # Choose no lambda


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(VT_Weather$TAVG, null = "Level") # Stationary
kpss.test(VT_Weather$TAVG, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(VT_Weather$TAVG, alternative = "stationary") # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(VT_Weather$TAVG, frequency = 12), sid = 12)


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
VT_Daily_TAVG_Seasonal <- VT_Weather
VT_Daily_TAVG_Seasonal$TAVG <- c(array(NA, dim = c(365, 1)), diff(VT_Daily_TAVG_Seasonal$TAVG, 365))
VT_Daily_TAVG_Seasonal <- VT_Daily_TAVG_Seasonal[-c(1:365), ]

# Create time series plot for deseasonalized data.
(ggplot(data = VT_Daily_TAVG_Seasonal, aes(x = DATE, y = TAVG, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Average Daily Temperature in Vermont by Month, Deseasonalized")
  + theme_bw())

# Create ACF and PACF for time series. 
acf(VT_Daily_TAVG_Seasonal$TAVG, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(VT_Daily_TAVG_Seasonal$TAVG, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(VT_Daily_TAVG_Seasonal$TAVG, null = "Level") # Stationary
kpss.test(VT_Daily_TAVG_Seasonal$TAVG, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(VT_Daily_TAVG_Seasonal$TAVG, alternative = "stationary") # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(VT_Daily_TAVG_Seasonal$TAVG, frequency = 12), sid = 12)


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,1,0) model.  -------------------------------------
VT_Daily_TAVG_fit <- arima(VT_Weather$TAVG, order = c(5, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(VT_Daily_TAVG_fit)

# Fiding BIC.
AIC(VT_Daily_TAVG_fit, k = log(length(VT_Weather$TAVG)))

# ARIMA(0,0,0)(0,1,0)
# (AIC = 57144.4)
# (BIC = 57151.3)

# ARIMA(0,0,0)(1,1,0)
# (AIC = 55997.8)
# (BIC = 56011.59)

# ARIMA(2,0,0)(1,1,0)
# (AIC = 49980)
# (BIC = 50007.58)

# ARIMA(2,0,0)(1,1,1)
# (AIC = 48112.88)
# (BIC = 48147.3)


# Model Diganostics -------------------------------------------------------

# Confirm Stationarity with Unit Root Graph.
autoplot(VT_Daily_TAVG_fit) # Stationary

# Diagnostics Plots.
residuals_VT_TAVG_fit <- as.vector(residuals(VT_Daily_TAVG_fit))
predicted_VT_TAVG_fit <- as.vector(fitted(VT_Daily_TAVG_fit))

qqnorm(residuals_VT_TAVG_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_VT_TAVG_fit, datax = TRUE, col = "red")
plot(predicted_VT_TAVG_fit, residuals_VT_TAVG_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_VT_TAVG_fit, breaks = 24, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_VT_TAVG_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_VT_TAVG_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_VT_TAVG <- acf(residuals_VT_TAVG_fit, lag.max = 365, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_VT_TAVG <- acf(residuals_VT_TAVG_fit, lag.max = 365, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:365){
  variable1 <- Box.test(resid(VT_Daily_TAVG_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_TAVG <- as.array(cbind(Res_ACF_VT_TAVG$lag[-1], Res_ACF_VT_TAVG$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_VT_TAVG$lag, Res_PACF_VT_TAVG$acf))
colnames(Ljung_Box_TAVG)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_TAVG <- as.data.frame(Ljung_Box_TAVG)

plot(Ljung_Box_TAVG$Lag, Ljung_Box_TAVG$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')



