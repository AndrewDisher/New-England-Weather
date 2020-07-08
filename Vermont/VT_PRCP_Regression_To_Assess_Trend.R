
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
PRCP_Regression <- cbind(Months_Since_1999, VT_Weather_Monthly$PRCP)
colnames(PRCP_Regression)[2] <- "PRCP"
PRCP_Regression <- as.data.frame(PRCP_Regression)

# Create the regression model. 
PRCP_Regression_Model <- lm(data = PRCP_Regression, PRCP ~ Months_Since_1999)
# Or a different one.
PRCP_Regression_Model <- lm(data = PRCP_Regression, PRCP ~ sin(2*pi*Months_Since_1999/12) + cos(2*pi*Months_Since_1999/12))

# Plot the model against the data
plot(PRCP_Regression, type = 'l', xlab = "Months Since 1999", ylab = "Precipitation (inches)", 
     main = "Regression of Precipitation on Time")
points(PRCP_Regression, pch = 16, cex = .5)
lines(PRCP_Regression$Months_Since_1999, PRCP_Regression_Model$fitted.values, col = "red", lty = 2)
legend("bottomright", c("Actual Data (black)", "Fitted Data (red)"))

coeftest(PRCP_Regression_Model)






