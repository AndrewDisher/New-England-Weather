
#
# Andrew Disher
# 4/8/2020
# Summer ATP Project
# Massachusetts ARIMA Model for Precipitation
#

library(ggplot2)
library(forecast)
library(car)
library(tseries)
library(dygraphs)
library(xts)


# Original Time Series -----------------------------------------------

# Create time series plot.
(ggplot(data = MA_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Precipitation (inches)") + ggtitle("Average Daily Precipitation in Massachusetts by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(MA_Weather_Monthly$PRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(MA_Weather_Monthly$PRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(MA_Weather_Monthly$PRCP ~ MA_Weather_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose lambda = 0

# Apply the Box Cox transformation.
MA_Weather_Monthly$tranPRCP <- BoxCox(MA_Weather_Monthly$PRCP, lambda = 0)

# Create a time series plot, ACF, and PACF for transformed series.
(ggplot(data = MA_Weather_Monthly, aes(x = NewDate, y = tranPRCP, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Log Precipitation (inches)") + ggtitle("Average Daily Precipitation in Massachusetts by Month")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(MA_Weather_Monthly$tranPRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(MA_Weather_Monthly$tranPRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(MA_Weather_Monthly$tranPRCP, null = "Level") # Stationary
kpss.test(MA_Weather_Monthly$tranPRCP, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(MA_Weather_Monthly$tranPRCP) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(MA_Weather_Monthly$tranPRCP, frequency = 12), sid = 12)


# Model Creation and Diagnostics ------------------------------------------

# Create an ARIMA(0,0,0)x(0,0,0) model. -----------------------------------
MA_PRCP_fit <- arima(MA_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(MA_PRCP_fit)

# Fiding BIC.
AIC(MA_PRCP_fit, k = log(length(MA_Weather_Monthly$PRCP)))

# Results:
# (AIC = 318.84)
# (BIC = 325.7991)

# Create an ARIMA(1,0,0)x(0,0,0) model. -----------------------------------
MA_PRCP_fit <- arima(MA_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(MA_PRCP_fit)

# Fiding BIC.
AIC(MA_PRCP_fit, k = log(length(MA_Weather_Monthly$PRCP)))

# Results:
# (AIC = 319.07)
# (BIC = 329.5091)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(MA_PRCP_fit, df = 239)

# Create an ARIMA(0,0,1)x(0,0,0) model. -----------------------------------
MA_PRCP_fit <- arima(MA_Weather_Monthly$tranPRCP, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(MA_PRCP_fit)

# Fiding BIC.
AIC(MA_PRCP_fit, k = log(length(MA_Weather_Monthly$PRCP)))

# Results:
# (AIC = 318.99)
# (BIC = 329.4337)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(MA_PRCP_fit, df = 239)


# Model Diganostics -------------------------------------------------------


# Confirm Stationarity with Unit Root Graph.
autoplot(MA_PRCP_fit) # Stationary

# Diagnostics Plots.
residuals_MA_PRCP_fit <- as.vector(residuals(MA_PRCP_fit))
predicted_MA_PRCP_fit <- as.vector(fitted(MA_PRCP_fit))

qqnorm(residuals_MA_PRCP_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_MA_PRCP_fit, datax = TRUE, col = "red")
plot(predicted_MA_PRCP_fit, residuals_MA_PRCP_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_MA_PRCP_fit, breaks = 25, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_MA_PRCP_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_MA_PRCP_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_MA_PRCP <- acf(residuals_MA_PRCP_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_MA_PRCP <- acf(residuals_MA_PRCP_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(resid(MA_PRCP_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_PRCP <- as.array(cbind(Res_ACF_MA_PRCP$lag[-1], Res_ACF_MA_PRCP$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_MA_PRCP$lag, Res_PACF_MA_PRCP$acf))
colnames(Ljung_Box_PRCP)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_PRCP <- as.data.frame(Ljung_Box_PRCP)

plot(Ljung_Box_PRCP$Lag, Ljung_Box_PRCP$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')


# Model Forecasting and Evaluation ----------------------------------------

# Obtain daily forecasts.   
MA_PRCP_Forecasts <- as.array(forecast(MA_PRCP_fit, lambda = 0, biasadj = FALSE, h = 12,level = 95))
MA_PRCP_Forecasts

# Plot the forecasts. 
(autoplot(ts(data = exp(MA_Weather_Monthly$tranPRCP)), series = "Data") + autolayer(exp(fitted(MA_PRCP_fit)), series = "ARIMA(0,0,0) x (0,0,0)")
  + autolayer(MA_PRCP_Forecasts, series = "Forecast")
  + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(0,0,0) x (0,0,0)'='#E30022', 'Forecast'='blue'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,0,0)', 'Forecast'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Precipitation (inches)') + theme_bw()
  + ggtitle("Average Daily Precipitation in Massachusetts by Month, Forecasts") + theme(legend.position = "bottom"))


# Yet ANOTHER way to graph the data, but forecasts ONLY.
dyPRCP = xts(as.data.frame(cbind(gen_array(MA_PRCP_Forecasts), MA_Weather_Monthly_Evaluation$PRCP)),
             order.by = seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "months"))
colnames(dyPRCP) <- c("Lo 95", "Hi 95","Point Forecast", "PRCP")

dygraph(dyPRCP, main = 'Average Daily Precipitation in Massachusetts by Month, Forecasts', 
        xlab = 'Date', ylab = 'Precipitation (inches)') %>%
  dySeries("PRCP", label = "2019 Data") %>%
  dySeries(c("Lo 95", "Point Forecast", "Hi 95"), label = c("Forecast")) 


# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
PRCP_Model1 <- arima(MA_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
PRCP_Model2 <- arima(MA_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
PRCP_Model3 <- arima(MA_Weather_Monthly$tranPRCP, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 


# Plot data and various models.
(autoplot(ts(data = exp(MA_Weather_Monthly$tranPRCP)), series = "Data") 
  + autolayer(exp(fitted(PRCP_Model1)), series = "ARIMA(0,0,0) x (0,0,0)")
  + autolayer(exp(fitted(PRCP_Model2)), series = "ARIMA(1,0,0) x (0,0,0)")
  + autolayer(exp(fitted(PRCP_Model3)), series = "ARIMA(0,0,1) x (0,0,0)")
  + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(0,0,0) x (0,0,0)'='#006B3C', 
                                   'ARIMA(1,0,0) x (0,0,0)'='blue', 
                                   'ARIMA(0,0,1) x (0,0,0)'='purple'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,0,0)',
                                   'ARIMA(1,0,0) x (0,0,0)', 'ARIMA(0,0,1) x (0,0,0)'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Precipitation (inches)') + theme_bw()
  + ggtitle("Average Daily Precipitation in Massachusetts by Month") + theme(legend.position = "bottom"))

# Create a table that includes all error measures for comparing the models. 
PRCP_Comparison_Measures <- rbind(cbind(summary(PRCP_Model1), PRCP_Model1$aic, AIC(PRCP_Model1, k = log(length(MA_Weather_Monthly$tranPRCP)))),
                                  cbind(summary(PRCP_Model2), PRCP_Model2$aic, AIC(PRCP_Model2, k = log(length(MA_Weather_Monthly$tranPRCP)))),
                                  cbind(summary(PRCP_Model3), PRCP_Model3$aic, AIC(PRCP_Model3, k = log(length(MA_Weather_Monthly$tranPRCP)))))
colnames(PRCP_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(PRCP_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,0,0)', 'ARIMA(1,0,0) x (0,0,0)',
                                         'ARIMA(0,0,1) x (0,0,0)')

PRCP_Comparison_Measures <- as.data.frame(PRCP_Comparison_Measures)








