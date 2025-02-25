
#
# Andrew Disher
# 4/8/2020
# Summer ATP Project
# Maine ARIMA Model for Snowfall
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
(ggplot(data = ME_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNOW, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Snowfall (inches)") + ggtitle("Average Daily Snowfall in Maine by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(ME_SNWD_SNOW_Monthly$SNOW, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_SNWD_SNOW_Monthly$SNOW, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(ME_SNWD_SNOW_Monthly$SNOW + 1 ~ ME_SNWD_SNOW_Monthly$NewDate, lambda = seq(-6, 3, by = 1)) 
# Choose lambda = -2.

# Apply the Box Cox transformation.
ME_SNWD_SNOW_Monthly$tranSNOW <- BoxCox(ME_SNWD_SNOW_Monthly$SNOW + 1, lambda = -2)

# Create a time series plot, ACF, and PACF for transformed series.
(ggplot(data = ME_SNWD_SNOW_Monthly, aes(x = NewDate, y = tranSNOW, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Square Inverse Snowfall (inches)") + ggtitle("Average Daily Snowfall in Maine by Month")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(ME_SNWD_SNOW_Monthly$tranSNOW, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_SNWD_SNOW_Monthly$tranSNOW, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(ME_SNWD_SNOW_Monthly$tranSNOW, null = "Level") # Stationary
kpss.test(ME_SNWD_SNOW_Monthly$tranSNOW, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(ME_SNWD_SNOW_Monthly$tranSNOW) # Stationary
ch.test(ts(ME_SNWD_SNOW_Monthly$tranSNOW, frequency = 8), sid = 8)


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
ME_SNOW_Seasonal <- ME_SNWD_SNOW_Monthly
ME_SNOW_Seasonal$tranSNOW <- c(array(NA, dim = c(8, 1)), diff(ME_SNOW_Seasonal$tranSNOW, 8))
ME_SNOW_Seasonal <- ME_SNOW_Seasonal[-c(1:8), ]

# Create a time series plot, ACF, and PACF for deseasonalized series.
(ggplot(data = ME_SNOW_Seasonal, aes(x = NewDate, y = tranSNOW, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Square Inverse Snowfall (inches)") + ggtitle("Average Daily Snowfall in Maine by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(ME_SNOW_Seasonal$tranSNOW, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_SNOW_Seasonal$tranSNOW, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(ME_SNOW_Seasonal$tranSNOW, null = "Level") # Stationary
kpss.test(ME_SNOW_Seasonal$tranSNOW, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(ME_SNOW_Seasonal$tranSNOW) # Stationary

# CAnova Hanson Test for Seasonal Unit Roots
ch.test(ts(ME_SNOW_Seasonal$tranSNOW, frequency = 8), sid = 8)


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,1,0) model.  -------------------------------------
ME_SNOW_fit <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 8)) 
ME_SNOW_fit

# Fiding BIC.
paste("BIC = " , as.character(AIC(ME_SNOW_fit, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), sep = "")

# Results:
# (AIC = -244.71)
# (BIC = -241.6304)

# Create ARIMA(0,0,0)x(0,1,1) model.  -------------------------------------
ME_SNOW_fit <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
ME_SNOW_fit

# Fiding BIC.
paste("BIC = " , as.character(AIC(ME_SNOW_fit, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), sep = "")

# Results:
# (AIC = -341.48)
# (BIC = -335.3327)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_SNOW_fit, df = 151)

# Create ARIMA(0,0,0)x(1,1,0) model.  -------------------------------------
ME_SNOW_fit <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 0), period = 8)) 
ME_SNOW_fit

# Fiding BIC.
paste("BIC = " , as.character(AIC(ME_SNOW_fit, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), sep = "")

# Results:
# (AIC = -285.44)
# (BIC = -279.29408)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_SNOW_fit, df = 151)

# Create ARIMA(0,0,0)x(1,1,1) model.  -------------------------------------
ME_SNOW_fit <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 1), period = 8)) 
ME_SNOW_fit

# Fiding BIC.
paste("BIC = " , as.character(AIC(ME_SNOW_fit, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), sep = "")

# Results:
# (AIC = -344.24)
# (BIC = -335.01458)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_SNOW_fit, df = 150)

# Create ARIMA(0,0,0)x(0,1,2) model.  -------------------------------------
ME_SNOW_fit <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 2), period = 8)) 
ME_SNOW_fit

# Fiding BIC.
paste("BIC = " , as.character(AIC(ME_SNOW_fit, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), sep = "")

# Results:
# (AIC = -346.91)
# (BIC = -337.6871)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_SNOW_fit, df = 150)

# Conclusion: We will choose the ARIMA(0,0,0)x(1,1,1) model. 


# Model Diagnostics ------------------------------------------

# Confirm Stationarity with Unit Root Graph.
autoplot(ME_SNOW_fit) # Stationary

# Diagnostics Plots.
residuals_ME_SNOW_fit <- as.vector(residuals(ME_SNOW_fit))
predicted_ME_SNOW_fit <- as.vector(fitted(ME_SNOW_fit))

qqnorm(residuals_ME_SNOW_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_ME_SNOW_fit, datax = TRUE, col = "red")
plot(predicted_ME_SNOW_fit, residuals_ME_SNOW_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_ME_SNOW_fit, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_ME_SNOW_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_ME_SNOW_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_ME_SNOW <- acf(residuals_ME_SNOW_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_ME_SNOW <- acf(residuals_ME_SNOW_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(resid(ME_SNOW_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_SNOW <- as.array(cbind(Res_ACF_ME_SNOW$lag[-1], Res_ACF_ME_SNOW$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_ME_SNOW$lag, Res_PACF_ME_SNOW$acf))
colnames(Ljung_Box_SNOW)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_SNOW <- as.data.frame(Ljung_Box_SNOW)

plot(Ljung_Box_SNOW$Lag, Ljung_Box_SNOW$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')

# Perform test of normality
shapiro.test(residuals_ME_SNOW_fit) # Not bad, usable


# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
SNOW_Model1 <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 8)) 
SNOW_Model2 <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
SNOW_Model3 <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 0), period = 8)) 
SNOW_Model4 <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 1), period = 8)) 
SNOW_Model5 <- arima(ME_SNWD_SNOW_Monthly$tranSNOW, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 2), period = 8)) 


# Plot data and various models.
(autoplot(ts(data = InvBoxCox(ME_SNWD_SNOW_Monthly$tranSNOW, lambda = -2)), series = "Data", size = 1)
  + autolayer(InvBoxCox(fitted(SNOW_Model1), lambda = -2), series = "ARIMA(0,0,0) x (0,1,0)")
  + autolayer(InvBoxCox(fitted(SNOW_Model2), lambda = -2), series = "ARIMA(0,0,0) x (0,1,1)")
  + autolayer(InvBoxCox(fitted(SNOW_Model3), lambda = -2), series = "ARIMA(0,0,0) x (1,1,0)")
  + autolayer(InvBoxCox(fitted(SNOW_Model4), lambda = -2), series = "ARIMA(0,0,0) x (1,1,1)")
  + autolayer(InvBoxCox(fitted(SNOW_Model5), lambda = -2), series = "ARIMA(0,0,0) x (0,1,2)")
  + scale_colour_manual(values = c('Data'='black', 'ARIMA(0,0,0) x (0,1,0)'='#E30022',
                                   'ARIMA(0,0,0) x (0,1,1)'='blue', 
                                   'ARIMA(0,0,0) x (1,1,0)'='purple', 
                                   'ARIMA(0,0,0) x (1,1,1)'='green', 
                                   'ARIMA(0,0,0) x (0,1,2)'='brown'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,1,0)','ARIMA(0,0,0) x (0,1,1)',
                                   'ARIMA(0,0,0) x (1,1,0)', 'ARIMA(0,0,0) x (1,1,1)', 
                                   'ARIMA(0,0,0) x (0,1,2)'))
  + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Snowfall (inches)') + theme_bw()
  + ggtitle("Average Daily Snowfall in Maine by Month") + theme(legend.position = "bottom"))


# Create a table that includes all error measures for comparing the models. 
SNOW_Comparison_Measures <- rbind(cbind(summary(SNOW_Model1), SNOW_Model1$aic, AIC(SNOW_Model1, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))),
                                  cbind(summary(SNOW_Model2), SNOW_Model2$aic, AIC(SNOW_Model2, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))),
                                  cbind(summary(SNOW_Model3), SNOW_Model3$aic, AIC(SNOW_Model3, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), 
                                  cbind(summary(SNOW_Model4), SNOW_Model4$aic, AIC(SNOW_Model4, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))), 
                                  cbind(summary(SNOW_Model5), SNOW_Model5$aic, AIC(SNOW_Model5, k = log(length(ME_SNWD_SNOW_Monthly$tranSNOW)))))
colnames(SNOW_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(SNOW_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)',
                                         'ARIMA(0,0,0) x (1,1,0)', 'ARIMA(0,0,0) x (1,1,1)', 
                                         'ARIMA(0,0,0) x (0,1,2)')
SNOW_Comparison_Measures <- as.data.frame(SNOW_Comparison_Measures)




















