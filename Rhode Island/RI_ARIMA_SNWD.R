
#
# Andrew Disher
# 6/18/2020
# Summer ATP Project
# Rhode Island ARIMA Model for Snow Depth
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
(ggplot(data = RI_SNWD_SNOW_Monthly, aes(x = NewDate, y = SNWD, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Snow Depth (inches)") + ggtitle("Average Daily Snow Depth in Rhode Island by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(RI_SNWD_SNOW_Monthly$SNWD, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(RI_SNWD_SNOW_Monthly$SNWD, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(RI_SNWD_SNOW_Monthly$SNWD + 1 ~ RI_SNWD_SNOW_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose lambda = -2

# Apply the Box Cox transformation.
RI_SNWD_SNOW_Monthly$tranSNWD <- BoxCox(RI_SNWD_SNOW_Monthly$SNWD + 1, lambda = -2)

# Create a time series plot, ACF, and PACF for transformed series.
(ggplot(data = RI_SNWD_SNOW_Monthly, aes(x = NewDate, y = tranSNWD, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Square Inverse Snow Depth (inches)") + ggtitle("Average Daily Snow Depth in Rhode Island by Month")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(RI_SNWD_SNOW_Monthly$tranSNWD, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(RI_SNWD_SNOW_Monthly$tranSNWD, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(RI_SNWD_SNOW_Monthly$tranSNWD, null = "Level") # Stationary
kpss.test(RI_SNWD_SNOW_Monthly$tranSNWD, null = "Trend") # Stationary

# Perform an ADF test for non-seasonal unit-root stationarity.
adf.test(RI_SNWD_SNOW_Monthly$tranSNWD) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(RI_SNWD_SNOW_Monthly$tranSNWD, frequency = 8), sid = 8)


# Deasonalized Data -------------------------------------------------------

# Try differencing the data to eliminate seasonality. 
RI_SNWD_Seasonal <- RI_SNWD_SNOW_Monthly
RI_SNWD_Seasonal$tranSNWD <- c(array(NA, dim = c(8, 1)), diff(RI_SNWD_Seasonal$tranSNWD, 8))
RI_SNWD_Seasonal <- RI_SNWD_Seasonal[-c(1:8), ]

# Create a time series plot, ACF, and PACF for deseasonalized series.
(ggplot(data = RI_SNWD_Seasonal, aes(x = NewDate, y = tranSNWD, group = 1, color = "blue")) 
  + geom_line(color = "blue") 
  + xlab("Date") + ylab("Square Inverse Snow Depth (inches)") + ggtitle("Average Daily Snow Depth in Rhode Island by Month, Deseasonalized")
  + theme_bw() + geom_point(col = "red", pch = 1))

acf(RI_SNWD_Seasonal$tranSNWD, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(RI_SNWD_Seasonal$tranSNWD, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(RI_SNWD_Seasonal$tranSNWD, null = "Level") # Stationary
kpss.test(RI_SNWD_Seasonal$tranSNWD, null = "Trend") # Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(RI_SNWD_Seasonal$tranSNWD) # Stationary

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(RI_SNWD_Seasonal$tranSNWD, frequency = 8), sid = 8)


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,1,0) model.  -------------------------------------
RI_SNWD_fit <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 8)) 
summary(RI_SNWD_fit)

# Fiding BIC.
AIC(RI_SNWD_fit, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -114.42)
# (BIC = -111.348)

# Create ARIMA(0,0,0)x(0,1,1) model.  -------------------------------------
RI_SNWD_fit <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
summary(RI_SNWD_fit)

# Fiding BIC.
AIC(RI_SNWD_fit, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -210.09)
# (BIC = -203.9375)

# Test significance of paramter estimates.
coeftest(RI_SNWD_fit, df = 151)

# Create ARIMA(1,0,0)x(0,1,1) model.  -------------------------------------
RI_SNWD_fit <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
summary(RI_SNWD_fit)

# Fiding BIC.
AIC(RI_SNWD_fit, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -220)
# (BIC = -210.778)


# Test significance of paramter estimates.
coeftest(RI_SNWD_fit, df = 150)

# Create ARIMA(0,0,1)x(0,1,1) model.  -------------------------------------
RI_SNWD_fit <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
summary(RI_SNWD_fit)

# Fiding BIC.
AIC(RI_SNWD_fit, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -218.15)
# (BIC = -208.9217)

# Test significance of paramter estimates.
coeftest(RI_SNWD_fit, df = 150)

# Create ARIMA(1,0,0)x(3,1,0) model.  -------------------------------------
RI_SNWD_fit <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(3, 1, 0), period = 8)) 
summary(RI_SNWD_fit)

# Fiding BIC.
AIC(RI_SNWD_fit, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))

# Results:
# (AIC = -210.9)
# (BIC = -195.5228)

# Test significance of paramter estimates.
coeftest(RI_SNWD_fit, df = 148)


# Conclusion: We will choose the ARIMA(1,0,0)x(0,1,1) model. 
# EDIT: Maybe not. The issue with all of the models including a seasonal moving average term (SMA1) is that
# the coefficient is always -1, which means that it makes the model explosive. This makes the model 
# unstable and the forecasts can't be trusted, even if the AIC and BIC values are better than the other 
# models. 
# Instead, we will choose either the AR(4) model or the ARIMA(1,0,0)x(3,1,0) model. 
#
# EDIT: AR(4) model has some incredibly bad Ljung-Box P-values, many of which suggest serial correlation.
# This model is not an option. 

# Model Diagnostics -------------------------------------------------------


# Confirm Stationarity with Unit Root Graph.
autoplot(RI_SNWD_fit) # Stationary

# Diagnostics Plots.
residuals_RI_SNWD_fit <- as.vector(residuals(RI_SNWD_fit))
predicted_RI_SNWD_fit <- as.vector(fitted(RI_SNWD_fit))

qqnorm(residuals_RI_SNWD_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_RI_SNWD_fit, datax = TRUE, col = "red")
plot(predicted_RI_SNWD_fit, residuals_RI_SNWD_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_RI_SNWD_fit, breaks = 20, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_RI_SNWD_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_RI_SNWD_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_RI_SNWD <- acf(residuals_RI_SNWD_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_RI_SNWD <- acf(residuals_RI_SNWD_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
  variable1 <- Box.test(resid(RI_SNWD_fit), type = "Ljung", lag = number, fitdf=1)
  LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
  LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_SNWD <- as.array(cbind(Res_ACF_RI_SNWD$lag[-1], Res_ACF_RI_SNWD$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_RI_SNWD$lag, Res_PACF_RI_SNWD$acf))
colnames(Ljung_Box_SNWD)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_SNWD <- as.data.frame(Ljung_Box_SNWD)

plot(Ljung_Box_SNWD$Lag, Ljung_Box_SNWD$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')

# Perform test of normality
shapiro.test(residuals_RI_SNWD_fit) # Highly non-normal


# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
SNWD_Model1 <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 8)) 
SNWD_Model2 <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
SNWD_Model3 <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
SNWD_Model4 <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 1, 1), period = 8)) 
SNWD_Model5 <- arima(RI_SNWD_SNOW_Monthly$tranSNWD, order = c(1, 0, 0), 
                     seasonal = list(order = c(3, 1, 0), period = 8)) 


# Plot data and various models.
(autoplot(ts(data = InvBoxCox(RI_SNWD_SNOW_Monthly$tranSNWD, lambda = -2)), series = "Data", size = 1) 
  + autolayer(InvBoxCox(fitted(SNWD_Model1), lambda = -2), series = "ARIMA(0,0,0) x (0,1,0)")
  + autolayer(InvBoxCox(fitted(SNWD_Model2), lambda = -2), series = "ARIMA(0,0,0) x (0,1,1)")
  + autolayer(InvBoxCox(fitted(SNWD_Model3), lambda = -2), series = "ARIMA(1,0,0) x (0,1,1)")
  + autolayer(InvBoxCox(fitted(SNWD_Model4), lambda = -2), series = "ARIMA(0,0,1) x (0,1,1)")
  + autolayer(InvBoxCox(fitted(SNWD_Model5), lambda = -2), series = "ARIMA(1,0,0) x (3,1,0)")
  + scale_colour_manual(values = c('Data'='black', 'ARIMA(0,0,0) x (0,1,0)'='green', 
                                   'ARIMA(0,0,0) x (0,1,1)'='blue', 
                                   'ARIMA(1,0,0) x (0,1,1)'='#E30022', 
                                   'ARIMA(0,0,1) x (0,1,1)'='purple', 
                                   'ARIMA(1,0,0) x (3,1,0)'='orange'),
                        breaks = c('Data', 'ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)',
                                   'ARIMA(1,0,0) x (0,1,1)', 'ARIMA(0,0,1) x (0,1,1)', 
                                   'ARIMA(1,0,0) x (3,1,0)'))
  + guides(color=guide_legend(title="")) + xlab('Date (months)') + ylab('Snow Depth (inches)') + theme_bw()
  + ggtitle("Average Daily Snow Depth in Rhode Island by Month") + theme(legend.position = "bottom"))


# Create a table that includes all error measures for comparing the models. 
SNWD_Comparison_Measures <- rbind(cbind(summary(SNWD_Model1), SNWD_Model1$aic, AIC(SNWD_Model1, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))),
                                  cbind(summary(SNWD_Model2), SNWD_Model2$aic, AIC(SNWD_Model2, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))),
                                  cbind(summary(SNWD_Model3), SNWD_Model3$aic, AIC(SNWD_Model3, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))), 
                                  cbind(summary(SNWD_Model4), SNWD_Model4$aic, AIC(SNWD_Model4, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))), 
                                  cbind(summary(SNWD_Model5), SNWD_Model5$aic, AIC(SNWD_Model5, k = log(length(RI_SNWD_SNOW_Monthly$tranSNWD)))))
colnames(SNWD_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(SNWD_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,1,0)', 'ARIMA(0,0,0) x (0,1,1)',
                                         'ARIMA(1,0,0) x (0,1,1)', 'ARIMA(0,0,1) x (0,1,1)', 
                                         'ARIMA(1,0,0) x (3,1,0)')

SNWD_Comparison_Measures <- as.data.frame(SNWD_Comparison_Measures)







