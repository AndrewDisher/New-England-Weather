
#
# Andrew Disher
# 4/8/2020
# Summer ATP Project
# Maine ARIMA Model for Precipitation
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
(ggplot(data = ME_Weather_Monthly, aes(x = NewDate, y = PRCP, group = 1, color = "blue")) 
 + geom_line(color = "blue") 
 + xlab("Date") + ylab("Precipitation (inches)") + ggtitle("Average Daily Precipitation in Maine by Month")
 + theme_bw() + geom_point(col = "red", pch = 1))

# Create ACF and PACF for time series. 
acf(ME_Weather_Monthly$PRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_Weather_Monthly$PRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))

# Check Constant Variance Assumption --------------------------------------

# Check for a lambda value for a potential Box-Cox transformation. 
boxCox(ME_Weather_Monthly$PRCP ~ ME_Weather_Monthly$NewDate, lambda = seq(-3, 3, by = 1)) # Choose lambda = .5

# Apply the Box Cox transformation.
ME_Weather_Monthly$tranPRCP <- BoxCox(ME_Weather_Monthly$PRCP, lambda = .5)

# Create a time series plot, ACF, and PACF for transformed series.
(ggplot(data = ME_Weather_Monthly, aes(x = NewDate, y = tranPRCP, group = 1, color = "blue")) 
    + geom_line(color = "blue") 
    + xlab("Date") + ylab("Square Root Precipitation (inches)") + ggtitle("Average Daily Precipitation in Maine by Month")
    + theme_bw() + geom_point(col = "red", pch = 1))

acf(ME_Weather_Monthly$tranPRCP, lag.max = 50, type = "correlation", main = expression('ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
acf(ME_Weather_Monthly$tranPRCP, lag.max = 50, type = "partial", main = expression('Partial ACF'),
    ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Hypothesis Tests to Assess Stationarity ---------------------------------

# Conduct KPSS tests for level and trend stationarity.
kpss.test(ME_Weather_Monthly$tranPRCP, null = "Level") # Not Stationary
kpss.test(ME_Weather_Monthly$tranPRCP, null = "Trend") # Not Stationary

# Perform an ADF test for unit-root stationarity.
adf.test(ME_Weather_Monthly$tranPRCP) # Stationary, so we will use this result. ADF is more useful anyways.

# Perform Canova-Hanson test for seasonal unit root stationarity.
ch.test(ts(ME_Weather_Monthly$tranPRCP, frequency = 12), sid = 12)


# Model Creation and Diagnostics ------------------------------------------

# Create ARIMA(0,0,0)x(0,0,0) model.  -------------------------------------
ME_PRCP_fit <- arima(ME_Weather_Monthly$tranPRCP, order = c(0, 0 , 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
ME_PRCP_fit 

# Fiding BIC.
AIC(ME_PRCP_fit, k = log(length(ME_Weather_Monthly$PRCP)))

# Results:
# (AIC = -209.17)
# (BIC = -202.2098)

# Create ARIMA(1,0,0)x(0,0,0) model.  -------------------------------------
ME_PRCP_fit <- arima(ME_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
ME_PRCP_fit 

# Fiding BIC.
AIC(ME_PRCP_fit, k = log(length(ME_Weather_Monthly$PRCP)))

# Results:
# (AIC = -208.85)
# (BIC = -198.41)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_PRCP_fit, df = 239)

# Create ARIMA(0,0,1)x(0,0,0) model.  -------------------------------------
ME_PRCP_fit <- arima(ME_Weather_Monthly$tranPRCP, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
ME_PRCP_fit 

# Fiding BIC.
AIC(ME_PRCP_fit, k = log(length(ME_Weather_Monthly$PRCP)))

# Results:
# (AIC = -208.78)
# (BIC = -198.3371)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_PRCP_fit, df = 239)

# Create ARIMA(2,0,0)x(0,0,0) model.  -------------------------------------
ME_PRCP_fit <- arima(ME_Weather_Monthly$tranPRCP, order = c(2, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
ME_PRCP_fit 

# Fiding BIC.
AIC(ME_PRCP_fit, k = log(length(ME_Weather_Monthly$PRCP)))

# Results:
# (AIC = -206.88)
# (BIC = -192.9591)

# Check the p-values of the model coefficients to see if they are significant. 
coeftest(ME_PRCP_fit, df = 238)


# Model Diganostics -------------------------------------------------------


# Confirm Stationarity with Unit Root Graph.
autoplot(ME_PRCP_fit) # Stationary

# Diagnostics Plots.
residuals_ME_PRCP_fit <- as.vector(residuals(ME_PRCP_fit))
predicted_ME_PRCP_fit <- as.vector(fitted(ME_PRCP_fit))

qqnorm(residuals_ME_PRCP_fit, datax = TRUE, pch = 16, xlab = expression('Residual'), 
       main = expression('Normal Probability Plot'))
qqline(residuals_ME_PRCP_fit, datax = TRUE, col = "red")
plot(predicted_ME_PRCP_fit, residuals_ME_PRCP_fit, pch = 16, xlab = expression('Fitted Value'), 
     ylab = expression('Residual'), main = expression('Residuals vs. Fitted Values'))
abline(h = 0, col = "red")
hist(residuals_ME_PRCP_fit, breaks = 25, col = "gray", xlab = expression('Residuals'), main = expression('Histogram of Residuals'))
plot(residuals_ME_PRCP_fit, type = "l", xlab = expression('Observation Order'), 
     ylab = expression('Residual'), main = expression('Ordered Residual Plot'))
points(residuals_ME_PRCP_fit, pch = 16, cex = .5)
abline(h = 0, col = "red")

# ACF and PACF for Model Residuals. Shows random, white noise.
Res_ACF_ME_PRCP <- acf(residuals_ME_PRCP_fit, lag.max = 25, type = "correlation", main = expression('ACF of Residuals'), 
                       ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))
Res_PACF_ME_PRCP <- acf(residuals_ME_PRCP_fit, lag.max = 25, type = "partial", main = expression('Partial ACF of Residuals'), 
                        ylab = expression('ACF, r'[k]), xlab = expression('Lag, k'))


# Create a JMP-like table containing LAG, ACF, PACF, Ljung-Box Statistic, and its p-value.
LJ_Pvalue <- c()
LJ_Stat <- c()
for(number in 1:25){
    variable1 <- Box.test(resid(ME_PRCP_fit), type = "Ljung", lag = number, fitdf=1)
    LJ_Pvalue = c(LJ_Pvalue, variable1$p.value)
    LJ_Stat = c(LJ_Stat, variable1$statistic )
}
Ljung_Box_PRCP <- as.array(cbind(Res_ACF_ME_PRCP$lag[-1], Res_ACF_ME_PRCP$acf[-1], LJ_Stat, LJ_Pvalue, Res_PACF_ME_PRCP$lag, Res_PACF_ME_PRCP$acf))
colnames(Ljung_Box_PRCP)[c(1, 2, 3, 4, 5, 6)] <- c("Lag", "ACF", "Ljung_Stat", "P-Value", "Lag", "PACF")
Ljung_Box_PRCP <- as.data.frame(Ljung_Box_PRCP)

# None of the residual lags seem to be significant, according to the p-values of the Ljung-Box statistics.
plot(Ljung_Box_PRCP$Lag, Ljung_Box_PRCP$`P-Value`, xlab = expression('Lag, k'), 
     ylab = expression('P-Value'), main = expression('P-Values of Autocorrelation Lags'))
abline(h = .05, col = 'red')



# Model Comparison --------------------------------------------------------

# First, store all of the different competing models in variables that are to be plotted. 
PRCP_Model1 <- arima(ME_Weather_Monthly$tranPRCP, order = c(0, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
PRCP_Model2 <- arima(ME_Weather_Monthly$tranPRCP, order = c(1, 0, 0), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 
PRCP_Model3 <- arima(ME_Weather_Monthly$tranPRCP, order = c(0, 0, 1), 
                     seasonal = list(order = c(0, 0, 0), period = 12)) 

# Plot data and various models.
(autoplot(ts(data = (.5*ME_Weather_Monthly$tranPRCP)+1)^2, series = "Data") 
    + autolayer((.5*fitted(PRCP_Model1)+1)^2, series = "ARIMA(0,0,0) x (0,0,0)")
    + autolayer((.5*fitted(PRCP_Model2)+1)^2, series = "ARIMA(1,0,0) x (0,0,0)")
    + autolayer((.5*fitted(PRCP_Model3)+1)^2, series = "ARIMA(0,0,1) x (0,0,0)")
    + scale_colour_manual(values = c('Data'='#006B3C', 'ARIMA(0,0,0) x (0,0,0)'='#006B3C', 
                                     'ARIMA(1,0,0) x (0,0,0)'='blue', 
                                     'ARIMA(0,0,1) x (0,0,0)'='purple'),
                          breaks = c('Data', 'ARIMA(0,0,0) x (0,0,0)',
                                     'ARIMA(1,0,0) x (0,0,0)', 'ARIMA(0,0,1) x (0,0,0)'))
    + guides(color=guide_legend(title="")) + xlab('Date ') + ylab('Precipitation (inches)') + theme_bw()
    + ggtitle("Average Daily Precipitation in Maine by Month") + theme(legend.position = "bottom"))

# Create a table that includes all error measures for comparing the models. 
PRCP_Comparison_Measures <- rbind(cbind(summary(PRCP_Model1), PRCP_Model1$aic, AIC(PRCP_Model1, k = log(length(ME_Weather_Monthly$tranPRCP)))),
                                  cbind(summary(PRCP_Model2), PRCP_Model2$aic, AIC(PRCP_Model2, k = log(length(ME_Weather_Monthly$tranPRCP)))),
                                  cbind(summary(PRCP_Model3), PRCP_Model3$aic, AIC(PRCP_Model3, k = log(length(ME_Weather_Monthly$tranPRCP)))))
colnames(PRCP_Comparison_Measures)[c(8, 9)] <- c('AIC', 'BIC')
row.names(PRCP_Comparison_Measures) <- c('ARIMA(0,0,0) x (0,0,0)', 'ARIMA(1,0,0) x (0,0,0)',
                                         'ARIMA(0,0,1) x (0,0,0)')

PRCP_Comparison_Measures <- as.data.frame(PRCP_Comparison_Measures)

















