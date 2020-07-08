
#
# Andrew Disher
# ATP Summer Project
# Bridgewater State University
# TASK: Determine if there is a significant increase or decrease in average 
# temperature over time in Vermont.
#

library(lmtest)


# Create a month variable to be used as the X variable in the regression.
Months_Since_1999 <- 1:nrow(VT_Weather_Monthly)

# Combine months vector and TAVG vector for the regression data set. 
TAVG_Regression <- cbind(Months_Since_1999, VT_Weather_Monthly$TAVG)
colnames(TAVG_Regression)[2] <- "TAVG"
TAVG_Regression <- as.data.frame(TAVG_Regression)

# Create the regression model. 
TAVG_Regression_Model <- lm(data = TAVG_Regression, TAVG ~ Months_Since_1999)
# Or a different one.
TAVG_Regression_Model <- lm(data = TAVG_Regression, TAVG ~ sin(2*pi*Months_Since_1999/12) + cos(2*pi*Months_Since_1999/12))

# Plot the model against the data
plot(TAVG_Regression, type = 'l', xlab = "Months Since 1999", ylab = "Average Temperature (degrees Farenheit)", 
     main = "Regression of Average Temperature on Time")
points(TAVG_Regression, pch = 16, cex = .5)
lines(TAVG_Regression$Months_Since_1999, TAVG_Regression_Model$fitted.values, col = "red", lty = 2)
legend("bottomright", c("Actual Data (black)", "Fitted Data (red)"))

coeftest(TAVG_Regression_Model)








