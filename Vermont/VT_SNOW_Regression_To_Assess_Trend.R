
#
# Andrew Disher
# ATP Summer Project
# Bridgewater State University
# TASK: Determine if there is a significant increase or decrease in 
# snow depth over time in Vermont.
#

library(lmtest)


# Create a month variable to be used as the X variable in the regression.
Months_Since_1999 <- 1:nrow(VT_SNWD_SNOW_Monthly)

# Combine months vector and TAVG vector for the regression data set. 
SNOW_Regression <- cbind(Months_Since_1999, VT_SNWD_SNOW_Monthly$SNOW)
colnames(SNOW_Regression)[2] <- "SNOW"
SNOW_Regression <- as.data.frame(SNOW_Regression)

# Create the regression model. 
SNOW_Regression_Model <- lm(data = SNOW_Regression, SNOW ~ Months_Since_1999)
# Or a different one.
SNOW_Regression_Model <- lm(data = SNOW_Regression, SNOW ~ sin(2*pi*Months_Since_1999/12) + cos(2*pi*Months_Since_1999/12))

# Plot the model against the data
plot(SNOW_Regression, type = 'l', xlab = "'Snow Season' Months Since 1999", ylab = "Snowfall (inches)", 
     main = "Regression of Snowfall on Time")
points(SNOW_Regression, pch = 16, cex = .5)
lines(SNOW_Regression$Months_Since_1999, SNOW_Regression_Model$fitted.values, col = "red", lty = 2)
legend("topright", c("Actual Data (black)", "Fitted Data (red)"))

coeftest(SNOW_Regression_Model)


