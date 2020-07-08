
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
SNWD_Regression <- cbind(Months_Since_1999, VT_SNWD_SNOW_Monthly$SNWD)
colnames(SNWD_Regression)[2] <- "SNWD"
SNWD_Regression <- as.data.frame(SNWD_Regression)

# Create the regression model. 
SNWD_Regression_Model <- lm(data = SNWD_Regression, SNWD ~ Months_Since_1999)
# Or a different one.
SNWD_Regression_Model <- lm(data = SNWD_Regression, SNWD ~ sin(2*pi*Months_Since_1999/12) + cos(2*pi*Months_Since_1999/12))

# Plot the model against the data
plot(SNWD_Regression, type = 'l', xlab = "'Snow Season' Months Since 1999", ylab = "Snow Depth (inches)", 
     main = "Regression of Snow Depth on Time")
points(SNWD_Regression, pch = 16, cex = .5)
lines(SNWD_Regression$Months_Since_1999, SNWD_Regression_Model$fitted.values, col = "red", lty = 2)
legend("topright", c("Actual Data (black)", "Fitted Data (red)"))

coeftest(SNWD_Regression_Model)
