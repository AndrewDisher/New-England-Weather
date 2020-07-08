#
# Andrew Disher
# Bridgewater State University
# ATP Summer Research
# USHCN Data Base
#
# TASK: Create a data set containing average temperature for the years between 1999 and 2018, inclusive.
# Then subset it to contain the data between August, 2005 and March, 2013. Finally, insert this data into the 
# data set created for Rhode Island temperatures.
#

# Import data from the four different Rhode Island stations available in the USHCN data base.
USH00370896.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00370896.tob.tavg", quote="\"", comment.char="")
USH00374266.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00374266.tob.tavg", quote="\"", comment.char="")
USH00376698.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00376698.tob.tavg", quote="\"", comment.char="")

# Combine the data using the rbind() function.
RI_TAVG_1999_2018 <- rbind(USH00370896.tob, USH00374266.tob, USH00376698.tob)

# Remove the final column, which contains the average for the year. 
RI_TAVG_1999_2018 <- RI_TAVG_1999_2018[, -14]    

# Now rename the columns. 
columnNames <- c("Station_ID", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
colnames(RI_TAVG_1999_2018) <- columnNames

# Replace the (MISSING = -9999) values with NA values instead. This will make missing values easier to 
# work with later. 
RI_TAVG_1999_2018[RI_TAVG_1999_2018 == -9999] <- NA

# According the USHCN .readme file, temperature values are in hundredths of a degree Celsius,
# but are expressed as whole integers (e.g. divide by 100.0 to get whole degrees Celsius). 
# So now we must rescale the values to be in degrees celsius, and after that convert to degrees farenheit.

# To Celsius
RI_TAVG_1999_2018[, c(2:13)] <- RI_TAVG_1999_2018[, c(2:13)]/100

# To Farenheit
RI_TAVG_1999_2018[, c(2:13)] <- RI_TAVG_1999_2018[, c(2:13)]*1.8 + 32

# The data set now must be aggregated according to year, which is included as the last four digits of 
# the station ID.

# Create a Year column.
RI_TAVG_1999_2018$Year <- numeric(nrow(RI_TAVG_1999_2018))

# Extract the year from the station ID.
RI_TAVG_1999_2018$Station_ID <- as.character(RI_TAVG_1999_2018$Station_ID)

for (year in 1:nrow(RI_TAVG_1999_2018)) {
  RI_TAVG_1999_2018[year, 14] <- substring(RI_TAVG_1999_2018[year, 1], 13, 16)
}

# Aggregate by year
RI_TAVG_1999_2018 <- aggregate(data = RI_TAVG_1999_2018, 
                               cbind(January, February, March, April, May, June, July, August, September,
                                     October, November, December) ~ Year,
                               mean, na.rm = TRUE, na.action = NULL)

RI_TAVG_1999_2018 <- RI_TAVG_1999_2018[c(120:139), ]

# Now we must create a vector with the data we need to fill in the missing values in our Rhode Island data set.
# We will concatinate all of the vectors for each month, creating one long vector.

RI_TAVG_2005_2013 <- c()
for (row in 1:nrow(RI_TAVG_1999_2018)) {
  RI_TAVG_2005_2013 <- c(RI_TAVG_2005_2013, RI_TAVG_1999_2018[row, 2:ncol(RI_TAVG_1999_2018)])
}

RI_TAVG_2005_2013 <- as.data.frame(RI_TAVG_2005_2013)
RI_TAVG_2005_2013 <- t(RI_TAVG_2005_2013)
RI_TAVG_2005_2013 <- as.data.frame(RI_TAVG_2005_2013)

# Create a date column.
RI_TAVG_2005_2013 <- cbind(seq.Date(from = as.Date("1999/01/01"), to = as.Date("2018/12/01"), by = "month"), RI_TAVG_2005_2013)
colnames(RI_TAVG_2005_2013) <- c("Date", "TAVG")
row.names(RI_TAVG_2005_2013) <- 1:nrow(RI_TAVG_2005_2013)

# Subset the data to contain values between August, 2005 and March, 2013.
RI_TAVG_2005_2013 <- subset(RI_TAVG_2005_2013, Date >= as.Date("2005/08/01") & Date <= as.Date("2013/03/01"))

# Finally, RI_TAVG_2003_2013 contains the missing TAVG information that wsa needed by our previous Rhode Island
# data set. All we need to do is transfer the data to complete the old data set. We will do this below. 
# NOTE: This requires the loading of the RI_Weather and the USHCN_RI_Workspace R workspaces.

RI_Weather_Monthly[80:171, 6] <- RI_TAVG_2005_2013$TAVG










