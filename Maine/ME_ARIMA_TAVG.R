
#
# Andrew Disher
# 4/5/2020
# Summer ATP Project
# Maine ARIMA Model for Average Temperature
#

library(ggplot2)
library(forecast)
library(car)
library(tseries)
library(dygraphs)
library(xts)
library(lmtest)


# Original Time Series -----------------------------------------------

# Create time series plot.
(ggplot(data = ME_Weather_Monthly, aes(x = NewDate, y = TAVG, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Average Daily Temperature in Maine by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(ME_Weather_Monthly$TAVG, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_Weather_Monthly$TAVG, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(ME_Weather_Monthly$TAVG ~ ME_Weather_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose no lambda


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(ME_Weather_Monthly$TAVG, null = "Level") # Stationary
kpss.test(ME_Weather_Monthly$TAVG, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(ME_Weather_Monthly$TAVG) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(ME_Weather_Monthly$TAVG, frequency = 12), sid = 12)


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
ME_TAVG_Seasonal <- ME_Weather_Monthly
ME_TAVG_Seasonal$TAVG <- c(array(NA, dim = c(12, 1)), diff(ME_TAVG_Seasonal$TAVG, 12))
ME_TAVG_Seasonal <- ME_TAVG_Seasonal[-c(1:12), ]

# Create time series plot for deseasonalized data.
(ggplot(data = ME_TAVG_Seasonal, aes(x = NewDate, y = TAVG, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Temperature (degrees Farenheit)") + ggtitle("Average Daily Temperature in Maine by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(ME_TAVG_Seasonal$TAVG, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_TAVG_Seasonal$TAVG, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(ME_TAVG_Seasonal$TAVG, null = "Level") # Stationary
kpss.test(ME_TAVG_Seasonal$TAVG, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(ME_TAVG_Seasonal$TAVG) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(ME_TAVG_Seasonal$TAVG, frequency = 12), sid = 12)


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,1,0) model.  -------------------------------------
ME_TAVG_fit <- arima(ME_Weather_Monthly$TAVG, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 12)) 
ME_TAVG_fit

# Fiding BIC.
AIC(ME_TAVG_fit, k = log(length(ME_Weather_Monthly$TAVG)))

# Results:
# (AIC = 1309.07)
# (BIC = 1312.55)

# Create ARIMA(0,0,0)x(0,1,1) model.  -------------------------------------
ME_TAVG_fit <- arima(ME_Weather_Monthly$TAVG, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
ME_TAVG_fit

# Fiding BIC.
AIC(ME_TAVG_fit, k = log(length(ME_Weather_Monthly$TAVG)))

# Results:
# (AIC = 1210.14)
# (BIC = 1217.105)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_TAVG_fit, df = 227)


# Create ARIMA(1,0,0)x(0,1,1) model.  -------------------------------------
ME_TAVG_fit <- arima(ME_Weather_Monthly$TAVG, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
ME_TAVG_fit

# Fiding BIC.
AIC(ME_TAVG_fit, k = log(length(ME_Weather_Monthly$TAVG)))

# Results:
# (AIC = 1180.86)
# (BIC = 1191.306)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_TAVG_fit, df = 226)

# Create ARIMA(3,0,0)x(0,1,1) model.  -------------------------------------
ME_TAVG_fit <- arima(ME_Weather_Monthly$TAVG, order = c(3, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
ME_TAVG_fit

# Fiding BIC.
AIC(ME_TAVG_fit, k = log(length(ME_Weather_Monthly$TAVG)))

# Results:
# (AIC = 1175.63)
# (BIC = 1193.029)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_TAVG_fit, df = 224)

# Create ARIMA(1,0,1)x(0,1,1) model.  -------------------------------------
ME_TAVG_fit <- arima(ME_Weather_Monthly$TAVG, order = c(1, 0, 1), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
ME_TAVG_fit

# Fiding BIC.
AIC(ME_TAVG_fit, k = log(length(ME_Weather_Monthly$TAVG)))

# Results:
# (AIC = 1175.42)
# (BIC = 1189.343)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_TAVG_fit, df = 226)


# Model Diganostics -------------------------------------------------------

# Confirm Stationarity with Unit Root Graph.
autoplot(ME_TAVG_fit) # Stationary

# Diagnostics Plots.
residuals_ME_TAVG_fit <- as.vector(residuals(ME_TAVG_fit))
predicted_ME_TAVG_fit <- as.vector(fitted(ME_TAVG_fit))

qqnorm(residuals_ME_TAVG_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_ME_TAVG_fit, datax = TRUE, col = "red")
plot(predicted_ME_TAVG_fit, residuals_ME_TAVG_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_ME_TAVG_fit, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_ME_TAVG_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_ME_TAVG_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_ME_TAVG <- acf(residuals_ME_TAVG_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_ME_TAVG <- acf(residuals_ME_TAVG_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(resid(ME_TAVG_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_TAVG <- as.array(cbind(Res_ACF_ME_TAVG$lag[-1], Res_ACF_ME_TAVG$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_ME_TAVG$lag, Res_PACF_ME_TAVG$acf))
colnames(Ljung_Box_TAVG)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_TAVG <- as.data.frame(Ljung_Box_TAVG)

plot(Ljung_Box_TAVG$Lag, Ljung_Box_TAVG$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')


# Model Forecasting and Evaluation ----------------------------------------

# Here we use the ARIMA(1,0,1)x(0,1,1) model. 

# Obtain daily forecasts.
ME_TAVG_Forecasts <- forecast(ME_TAVG_fit, h = 12, level = 95)
ME_TAVG_Forecasts

# Plot the forecasts. 
(autoplot(ts(data = ME_Weather_Monthly$TAVG), series = "Data") + autolayer(fitted(ME_TAVG_fit), series = "ARIMA(1,0,1) x (0,1,1)")
  + autolayer(ME_TAVG_Forecasts, series = "Forecast")
  + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(1,0,1) x (0,1,1)'='#E30022', 'Forecast'='blue'),
                        breaks = c('Data', 'ARIMA(1,0,1) x (0,1,1)', 'Forecast'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Temperature (degrees Farenheit)') + theme_bw()
  + ggtitle("Average Daily Temperature in Maaine Forecasts by Month") + theme(legend.position = "bottom"))


# Yet ANOTHER way to graph the data, but forecasts ONLY.
dyTAVG <- xts(as.data.frame(cbind(gen_array(ME_TAVG_Forecasts), ME_Weather_Monthly_Evaluation$TAVG)),
              order.by = seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "months"))
colnames(dyTAVG) <- c("Lo 95", "Hi 95","Point Forecast", "TAVG")

dygraph(data = dyTAVG, main = 'Average Daily Temperature in Maine Forecasts by Month ',
        xlab = 'Date', ylab = 'Temperature (degrees Farenheit)') %>%
  dySeries("TAVG", label = "2019 Data", color = "red") %>%
  dySeries(c("Lo 95", "Point Forecast", "Hi 95"), label = c("Forecast"), col = "blue") 



# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
TAVG_Model1 <- arima(ME_Weather_Monthly$TAVG, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 12)) 
TAVG_Model2 <- arima(ME_Weather_Monthly$TAVG, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
TAVG_Model3 <- arima(ME_Weather_Monthly$TAVG, order = c(1, 0, 1), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
TAVG_Model4 <- arima(ME_Weather_Monthly$TAVG, order = c(3, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 


# Plot data and various models.
(autoplot(ts(data = ME_Weather_Monthly$TAVG), series = "Data", size = 1) 
  + autolayer(fitted(TAVG_Model1), series = "ARIMA(0,0,0) x (0,1,0)")
  + autolayer(fitted(TAVG_Model2), series = "ARIMA(0,0,0) x (0,1,1)")
  + autolayer(fitted(TAVG_Model3), series = "ARIMA(1,0,1) x (0,1,1)")
  + autolayer(fitted(TAVG_Model4), series = "ARIMA(3,0,0) x (0,1,1)")
  + scale_colour_manual(values = c('Data'='black', 'ARIMA(1,0,1) x (0,1,1)'='#E30022',
                                   'ARIMA(0,0,0) x (0,1,0)'='blue', 
                                   'ARIMA(0,0,0) x (0,1,1)'='brown', 
                                   'ARIMA(3,0,0) x (0,1,1)'='gold'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)', 
                                   'ARIMA(1,0,1) x (0,1,1)', 'ARIMA(3,0,0) x (0,1,1)'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Temperature (degrees Farenheit)') + theme_bw()
  + ggtitle("Average Daily Temperature in Maine by Month") + theme(legend.position = "bottom"))


# Create a table that includes all error measures for comparing the models. 
TAVG_Comparison_Measures <- rbind(cbind(summary(TAVG_Model1), TAVG_Model1$aic, AIC(TAVG_Model1, k = log(length(ME_Weather_Monthly$TAVG)))),
                                  cbind(summary(TAVG_Model2), TAVG_Model2$aic, AIC(TAVG_Model2, k = log(length(ME_Weather_Monthly$TAVG)))),
                                  cbind(summary(TAVG_Model3), TAVG_Model3$aic, AIC(TAVG_Model3, k = log(length(ME_Weather_Monthly$TAVG)))),
                                  cbind(summary(TAVG_Model4), TAVG_Model4$aic, AIC(TAVG_Model4, k = log(length(ME_Weather_Monthly$TAVG)))))
colnames(TAVG_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(TAVG_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)',
                                         'ARIMA(1,0,1) x (0,1,1)', 'ARIMA(3,0,0) x (0,1,1)')

TAVG_Comparison_Measures <- as.data.frame(TAVG_Comparison_Measures)



# Functions ---------------------------------------------------------------


gen_array <- function(forecast_obj){
  
  lower <- forecast_obj$lower[,1]
  upper <- forecast_obj$upper[,1]
  point_forecast <- forecast_obj$mean
  
  cbind(lower, upper, point_forecast)
}





















