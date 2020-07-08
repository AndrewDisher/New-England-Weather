
#
# Andrew Disher
# 4/8/2020
# Summer ATP Project
# Vermont ARIMA Model for Precipitation
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
(ggplot(data = VT_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Precipitation (inches)") + ggtitle("Average Daily Precipitation in Vermont by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(VT_Weather_Monthly$PRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(VT_Weather_Monthly$PRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(VT_Weather_Monthly$PRCP ~ VT_Weather_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose lambda = 0

# Apply the Box Cox transformation.
VT_Weather_Monthly$tranPRCP <- BoxCox(VT_Weather_Monthly$PRCP, lambda = 0)

# Create a time series plot, ACF, and PACF for transformed series.
(ggplot(data = VT_Weather_Monthly, aes(x = NewDate, y = tranPRCP, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Log Precipitation (inches)") + ggtitle("Average Daily Precipitation in Vermont by Month")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(VT_Weather_Monthly$tranPRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(VT_Weather_Monthly$tranPRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(VT_Weather_Monthly$tranPRCP, null = "Level") # Stationary
kpss.test(VT_Weather_Monthly$tranPRCP, null = "Trend") # Not Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(VT_Weather_Monthly$tranPRCP, alternative = "stationary") # Stationary, so we will use this result. ADF is more useful anyways.

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(VT_Weather_Monthly$tranPRCP, frequency = 12), sid = 12)


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. This seasonality is not as pronounced in the ACF,
# so including a seasoal difference may not be beneficial to the model. If this is the case, do not
# include it, and add terms as we have in the past. 
VT_PRCP_Seasonal <- VT_Weather_Monthly
VT_PRCP_Seasonal$tranPRCP <- c(array(NA, dim = c(12, 1)), diff(VT_PRCP_Seasonal$tranPRCP, 12))
VT_PRCP_Seasonal <- VT_PRCP_Seasonal[-c(1:12), ]

# Create time series plot for deseasonalized data.
(ggplot(data = VT_PRCP_Seasonal, aes(x = NewDate, y = tranPRCP, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Log Precipitation (inches)") + ggtitle("Average Daily Precipitation in Vermont by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(VT_PRCP_Seasonal$tranPRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(VT_PRCP_Seasonal$tranPRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,0,0) model.  -------------------------------------
VT_PRCP_fit <- arima(VT_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(VT_PRCP_fit)

# Fiding BIC.
AIC(VT_PRCP_fit, k = log(length(VT_Weather_Monthly$tranPRCP)))

# Results:
# (AIC = 282.45)
# (BIC = 289.4128)

# Create ARIMA(0,0,0)x(0,1,1) model.  -------------------------------------
VT_PRCP_fit <- arima(VT_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
summary(VT_PRCP_fit)

# Fiding BIC.
AIC(VT_PRCP_fit, k = log(length(VT_Weather_Monthly$tranPRCP)))

# Results:
# (AIC = 260.4)
# (BIC = 267.3605)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(VT_PRCP_fit, df = 227)

# Create ARIMA(1,0,0)x(0,0,0) model.  -------------------------------------
VT_PRCP_fit <- arima(VT_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(VT_PRCP_fit) 

# Fiding BIC.
AIC(VT_PRCP_fit, k = log(length(VT_Weather_Monthly$tranPRCP)))

# Results:
# (AIC = 279.12)
# (BIC = 289.5631)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(VT_PRCP_fit, df = 239)

# Create ARIMA(0,0,1)x(0,0,0) model.  -------------------------------------
VT_PRCP_fit <- arima(VT_Weather_Monthly$tranPRCP, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(VT_PRCP_fit) 

# Fiding BIC.
AIC(VT_PRCP_fit, k = log(length(VT_Weather_Monthly$tranPRCP)))

# Results:
# (AIC = 279.86)
# (BIC = 290.2996)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(VT_PRCP_fit, df = 239)

# Create ARIMA(1,0,1)x(0,0,0) model.  -------------------------------------
VT_PRCP_fit <- arima(VT_Weather_Monthly$tranPRCP, order = c(1, 0, 1), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
summary(VT_PRCP_fit) 

# Fiding BIC.
AIC(VT_PRCP_fit, k = log(length(VT_Weather_Monthly$tranPRCP)))

# Results:
# (AIC = 280.78)
# (BIC = 294.7002)

# Create ARIMA(1,0,0)x(1,0,0) model.  -------------------------------------
VT_PRCP_fit <- arima(VT_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(1, 0, 0), period = 12)) 
summary(VT_PRCP_fit) 

# Fiding BIC.
AIC(VT_PRCP_fit, k = log(length(VT_Weather_Monthly$tranPRCP)))

# Results:
# (AIC = 277.72)
# (BIC = 291.6406)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(VT_PRCP_fit, df = 238)


# Model Diganostics -------------------------------------------------------

# We will choose the ARIMA(1,0,0)x(1,0,0) model.

# Confirm Stationarity with Unit Root Graph.
autoplot(VT_PRCP_fit) # Stationary

# Diagnostics Plots.
residuals_VT_PRCP_fit <- as.vector(residuals(VT_PRCP_fit))
predicted_VT_PRCP_fit <- as.vector(fitted(VT_PRCP_fit))

qqnorm(residuals_VT_PRCP_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_VT_PRCP_fit, datax = TRUE, col = "red")
plot(predicted_VT_PRCP_fit, residuals_VT_PRCP_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_VT_PRCP_fit, breaks = 25, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_VT_PRCP_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_VT_PRCP_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_VT_PRCP <- acf(residuals_VT_PRCP_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_VT_PRCP <- acf(residuals_VT_PRCP_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(resid(VT_PRCP_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_PRCP <- as.array(cbind(Res_ACF_VT_PRCP$lag[-1], Res_ACF_VT_PRCP$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_VT_PRCP$lag, Res_PACF_VT_PRCP$acf))
colnames(Ljung_Box_PRCP)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_PRCP <- as.data.frame(Ljung_Box_PRCP)

# None of the residual lags seem to be significant, according to the p-values of the Ljung-Box statistics.
plot(Ljung_Box_PRCP$Lag, Ljung_Box_PRCP$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')


# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
PRCP_Model1 <- arima(VT_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
PRCP_Model2 <- arima(VT_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
PRCP_Model3 <- arima(VT_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 12)) 
PRCP_Model4 <- arima(VT_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(1, 0, 0), period = 12)) 


# Plot data and various models.
(autoplot(ts(data = exp(VT_Weather_Monthly$tranPRCP)), series = "Data") 
  + autolayer(InvBoxCox(fitted(PRCP_Model1), lambda = 0), series = "ARIMA(0,0,0) x (0,0,0)")
  + autolayer(InvBoxCox(fitted(PRCP_Model2), lambda = 0), series = "ARIMA(1,0,0) x (0,0,0)")
  + autolayer(InvBoxCox(fitted(PRCP_Model3), lambda = 0), series = "ARIMA(0,0,0) x (0,1,1)")
  + autolayer(InvBoxCox(fitted(PRCP_Model4), lambda = 0), series = "ARIMA(1,0,0) x (1,0,0)")
  + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(0,0,0) x (0,0,0)'='#006B3C', 
                                   'ARIMA(1,0,0) x (0,0,0)'='blue', 
                                   'ARIMA(0,0,0) x (0,1,1)'='purple', 
                                   'ARIMA(1,0,0) x (1,0,0)'='orange'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,0,0)',
                                   'ARIMA(1,0,0) x (0,0,0)', 'ARIMA(0,0,0) x (0,1,1)', 
                                   'ARIMA(1,0,0) x (1,0,0)'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Precipitation (inches)') + theme_bw()
  + ggtitle("Average Daily Precipitation in Vermont by Month") + theme(legend.position = "bottom"))

# Create a table that includes all error measures for comparing the models. 
PRCP_Comparison_Measures <- rbind(cbind(summary(PRCP_Model1), PRCP_Model1$aic, AIC(PRCP_Model1, k = log(length(VT_Weather_Monthly$tranPRCP)))),
                                  cbind(summary(PRCP_Model2), PRCP_Model2$aic, AIC(PRCP_Model2, k = log(length(VT_Weather_Monthly$tranPRCP)))),
                                  cbind(summary(PRCP_Model3), PRCP_Model3$aic, AIC(PRCP_Model3, k = log(length(VT_Weather_Monthly$tranPRCP)))), 
                                  cbind(summary(PRCP_Model4), PRCP_Model4$aic, AIC(PRCP_Model4, k = log(length(VT_Weather_Monthly$tranPRCP)))))
colnames(PRCP_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(PRCP_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,0,0)', 'ARIMA(1,0,0) x (0,0,0)',
                                         'ARIMA(0,0,0) x (0,1,1)', 'ARIMA(1,0,0) x (1,0,0)')

PRCP_Comparison_Measures <- as.data.frame(PRCP_Comparison_Measures)










