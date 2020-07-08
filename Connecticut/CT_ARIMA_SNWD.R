
#
# Andrew Disher
# 4/5/2020
# Summer ATP Project
# Maine ARIMA Model for Snow Depth
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
(ggplot(data = CT_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Snow Depth (inches)") + ggtitle("Average Monthly Snow Depth in Connecticut by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(CT_SNWD_SNOW_Monthly$SNWD, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(CT_SNWD_SNOW_Monthly$SNWD, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(CT_SNWD_SNOW_Monthly$SNWD + 1 ~ CT_SNWD_SNOW_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose lambda = -1

# Apply the Box Cox transformation.
CT_SNWD_SNOW_Monthly$tranSNWD <- BoxCox(CT_SNWD_SNOW_Monthly$SNWD + 1, lambda = -1)

# Create a time series plot, ACF, and PACF for transformed series.
(ggplot(data = CT_SNWD_SNOW_Monthly, aes(x = NewDate, y = tranSNWD, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Inverse Snow Depth (inches)") + ggtitle("Average Monthly Snow Depth in Connecticut by Month")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(CT_SNWD_SNOW_Monthly$tranSNWD, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(CT_SNWD_SNOW_Monthly$tranSNWD, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(CT_SNWD_SNOW_Monthly$tranSNWD, null = "Level") # Stationary
kpss.test(CT_SNWD_SNOW_Monthly$tranSNWD, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(CT_SNWD_SNOW_Monthly$tranSNWD) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(CT_SNWD_SNOW_Monthly$tranSNWD, frequency = 8), sid = 8)


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
CT_SNWD_Seasonal <- CT_SNWD_SNOW_Monthly
CT_SNWD_Seasonal$tranSNWD <- c(array(NA, dim = c(8, 1)), diff(CT_SNWD_Seasonal$tranSNWD, 8))
CT_SNWD_Seasonal <- CT_SNWD_Seasonal[-c(1:8), ]

# Create a time series plot, ACF, and PACF for deseasonalized series.
(ggplot(data = CT_SNWD_Seasonal, aes(x = NewDate, y = tranSNWD, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Inverse Snow Depth (inches)") + ggtitle("Average Monthly Snow Depth in Connecticut by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(CT_SNWD_Seasonal$tranSNWD, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(CT_SNWD_Seasonal$tranSNWD, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(CT_SNWD_Seasonal$tranSNWD, null = "Level") # Stationary
kpss.test(CT_SNWD_Seasonal$tranSNWD, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(CT_SNWD_Seasonal$tranSNWD) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(CT_SNWD_Seasonal$tranSNWD, frequency = 8), sid = 8)


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,1,0) model.  -------------------------------------
CT_SNWD_fit <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 8)) 
CT_SNWD_fit

# Fiding BIC.
AIC(CT_SNWD_fit, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = 57.47)
# (BIC = 60.54493)

# Create ARIMA(0,0,0)x(1,1,0) model.  -------------------------------------
CT_SNWD_fit <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 0), period = 8)) 
CT_SNWD_fit

# Fiding BIC.
AIC(CT_SNWD_fit, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -7.4)
# (BIC = -1.253304)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(CT_SNWD_fit, df = 151)

# Create ARIMA(0,0,0)x(0,1,1) model.  -------------------------------------
CT_SNWD_fit <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
CT_SNWD_fit

# Fiding BIC.
AIC(CT_SNWD_fit, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -49.02)
# (BIC = -42.87027)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(CT_SNWD_fit, df = 152)

# Create ARIMA(0,0,0)x(1,1,1) model.  -------------------------------------
CT_SNWD_fit <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 1), period = 8)) 
CT_SNWD_fit

# Fiding BIC.
AIC(CT_SNWD_fit, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -54.84)
# (BIC = -45.61471)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(CT_SNWD_fit, df = 150)

# Create ARIMA(1,0,0)x(1,1,1) model.  -------------------------------------
CT_SNWD_fit <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(1, 1, 1), period = 8)) 
CT_SNWD_fit

# Fiding BIC.
AIC(CT_SNWD_fit, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -71.92)
# (BIC = -59.6181)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(CT_SNWD_fit, df = 149)

# Create ARIMA(1,0,0)x(0,1,1) model.  -------------------------------------
CT_SNWD_fit <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
CT_SNWD_fit

# Fiding BIC.
AIC(CT_SNWD_fit, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -70.47)
# (BIC = -61.24589)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(CT_SNWD_fit, df = 150)

# CONCLUSION: ARIMA(1,0,0)x(1,1,1) is best.


# Model Diagnostics -------------------------------------------------------


# Confirm Stationarity with Unit Root Graph.
autoplot(CT_SNWD_fit) # Stationary

# Diagnostics Plots.
residuals_CT_SNWD_fit <- as.vector(residuals(CT_SNWD_fit))
predicted_CT_SNWD_fit <- as.vector(fitted(CT_SNWD_fit))

qqnorm(residuals_CT_SNWD_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_CT_SNWD_fit, datax = TRUE, col = "red")
plot(predicted_CT_SNWD_fit, residuals_CT_SNWD_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_CT_SNWD_fit, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_CT_SNWD_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_CT_SNWD_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_CT_SNWD <- acf(residuals_CT_SNWD_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_CT_SNWD <- acf(residuals_CT_SNWD_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(resid(CT_SNWD_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_SNWD <- as.array(cbind(Res_ACF_CT_SNWD$lag[-1], Res_ACF_CT_SNWD$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_CT_SNWD$lag, Res_PACF_CT_SNWD$acf))
colnames(Ljung_Box_SNWD)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_SNWD <- as.data.frame(Ljung_Box_SNWD)

plot(Ljung_Box_SNWD$Lag, Ljung_Box_SNWD$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')

# Perform test of normality
shapiro.test(residuals_CT_SNWD_fit) # Highly non-normal


# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
SNWD_Model1 <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 8)) 
SNWD_Model2 <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
SNWD_Model3 <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 0), period = 8)) 
SNWD_Model4 <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(1, 1, 1), period = 8)) 
SNWD_Model5 <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(1, 1, 1), period = 8)) 
SNWD_Model6 <- arima(CT_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 


# Plot data and various models.
(autoplot(ts(data = (-1*CT_SNWD_SNOW_Monthly$tranSNWD + 1))^(-1), series = "Data", size = 1) 
  + autolayer((-1*fitted(SNWD_Model1)+1)^(-1), series = "ARIMA(0,0,0) x (0,1,0)")
  + autolayer((-1*fitted(SNWD_Model2)+1)^(-1), series = "ARIMA(0,0,0) x (0,1,1)")
  + autolayer((-1*fitted(SNWD_Model3)+1)^(-1), series = "ARIMA(0,0,0) x (1,1,0)")
  + autolayer((-1*fitted(SNWD_Model4)+1)^(-1), series = "ARIMA(0,0,0) x (1,1,1)")
  + autolayer((-1*fitted(SNWD_Model5)+1)^(-1), series = "ARIMA(1,0,0) x (1,1,1)")
  + autolayer((-1*fitted(SNWD_Model6)+1)^(-1), series = "ARIMA(1,0,0) x (0,1,1)")
  + scale_colour_manual(values = c('Data'='black', 'ARIMA(0,0,0) x (0,1,0)'='green', 
                                   'ARIMA(0,0,0) x (0,1,1)'='blue', 
                                   'ARIMA(0,0,0) x (1,1,0)'='#E30022',
                                   'ARIMA(0,0,0) x (1,1,1)'='purple', 
                                   'ARIMA(1,0,0) x (1,1,1)'='gold', 
                                   'ARIMA(1,0,0) x (0,1,1)'='brown'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)',
                                   'ARIMA(0,0,0) x (1,1,0)', 'ARIMA(0,0,0) x (1,1,1)', 
                                   'ARIMA(1,0,0) x (1,1,1)', 'ARIMA(1,0,0) x (0,1,1)'))
  + guides(color=guide_legend(title="")) + xlab('Date (months)') + ylab('Snow Depth (inches)') + theme_bw()
  + ggtitle("Average Daily Snow Depth in Connecticut by Month") + theme(legend.position = "bottom"))


# Create a table that includes all error measures for comparing the models. 
SNWD_Comparison_Measures <- rbind(cbind(summary(SNWD_Model1), SNWD_Model1$aic, AIC(SNWD_Model1, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))),
                                  cbind(summary(SNWD_Model2), SNWD_Model2$aic, AIC(SNWD_Model2, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))),
                                  cbind(summary(SNWD_Model3), SNWD_Model3$aic, AIC(SNWD_Model3, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))), 
                                  cbind(summary(SNWD_Model4), SNWD_Model4$aic, AIC(SNWD_Model4, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))), 
                                  cbind(summary(SNWD_Model5), SNWD_Model5$aic, AIC(SNWD_Model5, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))), 
                                  cbind(summary(SNWD_Model6), SNWD_Model6$aic, AIC(SNWD_Model6, k = log(length(CT_SNWD_SNOW_Monthly$tranSNWD)))))
colnames(SNWD_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(SNWD_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)',
                                         'ARIMA(0,0,0) x (1,1,0)', 'ARIMA(0,0,0) x (1,1,1)', 
                                         'ARIMA(1,0,0) x (1,1,1)', 'ARIMA(1,0,0) x (0,1,1)')

SNWD_Comparison_Measures <- as.data.frame(SNWD_Comparison_Measures)


### Model Decision: ARIMA(1,0,0)x(1,1,1)
### ARIMA(1,0,0)x(0,1,1) is not invertible since there is a unit root in the SMA portion of the model. 







