#
# Andrew Disher
# Bridgewater State University
# ATP Summer Research
# USHCN Data Base
#
# TASK: Create a data set containing average temperature for the years between 1999 and 2018, inclusive.
# Then subset it to contain the data between August, 2005 and March, 2013. Finally, insert this data into the 
# data set created for Massachusetts temperatures.
#

# Import data from the four different Massachusetts stations available in the USHCN data base.
USH00190120.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00190120.tob.tavg", quote="\"", comment.char="")
USH00190535.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00190535.tob.tavg", quote="\"", comment.char="")
USH00190736.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00190736.tob.tavg", quote="\"", comment.char="")
USH00193213.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00193213.tob.tavg", quote="\"", comment.char="")
USH00194105.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00194105.tob.tavg", quote="\"", comment.char="")
USH00195246.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00195246.tob.tavg", quote="\"", comment.char="")
USH00196486.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00196486.tob.tavg", quote="\"", comment.char="")
USH00196681.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00196681.tob.tavg", quote="\"", comment.char="")
USH00196783.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00196783.tob.tavg", quote="\"", comment.char="")
USH00198367.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00198367.tob.tavg", quote="\"", comment.char="")
USH00198757.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00198757.tob.tavg", quote="\"", comment.char="")
USH00199316.tob <- read.table("~/BSU Stuff/Summer ATP Project/Climate Datasets/USHCN_Data_Base/ushcn.v2.5.5.20200518/USH00199316.tob.tavg", quote="\"", comment.char="")

# Combine the data using the rbind() function.
MA_TAVG_1999_2018 <- rbind(USH00190120.tob, USH00190535.tob, USH00190736.tob, USH00193213.tob, USH00194105.tob,
                           USH00195246.tob, USH00196486.tob, USH00196681.tob, USH00196783.tob, USH00198367.tob, 
                           USH00198757.tob, USH00199316.tob)

# Remove the final column, which contains the average for the year. 
MA_TAVG_1999_2018 <- MA_TAVG_1999_2018[, -14]

# Now rename the columns. 
columnNames <- c("Station_ID", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
colnames(MA_TAVG_1999_2018) <- columnNames

# Replace the (MISSING = -9999) values with NA values instead. This will make missing values easier to 
# work with later. 
MA_TAVG_1999_2018[MA_TAVG_1999_2018 == -9999] <- NA

# According the USHCN .readme file, temperature values are in hundredths of a degree Celsius,
# but are expressed as whole integers (e.g. divide by 100.0 to get whole degrees Celsius). 
# So now we must rescale the values to be in degrees celsius, and after that convert to degrees farenheit.

# To Celsius
MA_TAVG_1999_2018[, c(2:13)] <- MA_TAVG_1999_2018[, c(2:13)]/100

# To Farenheit
MA_TAVG_1999_2018[, c(2:13)] <- MA_TAVG_1999_2018[, c(2:13)]*1.8 + 32

# The data set now must be aggregated according to year, which is included as the last four digits of 
# the station ID.

# Create a Year column.
MA_TAVG_1999_2018$Year <- numeric(nrow(MA_TAVG_1999_2018))

# Extract the year from the station ID.
MA_TAVG_1999_2018$Station_ID <- as.character(MA_TAVG_1999_2018$Station_ID)

for (year in 1:nrow(MA_TAVG_1999_2018)) {
  MA_TAVG_1999_2018[year, 14] <- substring(MA_TAVG_1999_2018[year, 1], 13, 16)
}

# Aggregate by year
MA_TAVG_1999_2018 <- aggregate(data = MA_TAVG_1999_2018, 
                               cbind(January, February, March, April, May, June, July, August, September,
                                     October, November, December) ~ Year,
                               mean, na.rm = TRUE, na.action = NULL)

MA_TAVG_1999_2018 <- MA_TAVG_1999_2018[c(116:135), ]

# Now we must create a vector with the data we need to fill in the missing values in our Massachusetts data set.
# We will concatinate all of the vectors for each month, creating one long vector.

MA_TAVG_2006 <- c()
for (row in 1:nrow(MA_TAVG_1999_2018)) {
  MA_TAVG_2006 <- c(MA_TAVG_2006, MA_TAVG_1999_2018[row, 2:ncol(MA_TAVG_1999_2018)])
}

MA_TAVG_2006 <- as.data.frame(MA_TAVG_2006)
MA_TAVG_2006 <- t(MA_TAVG_2006)
MA_TAVG_2006 <- as.data.frame(MA_TAVG_2006)

# Create a date column.
MA_TAVG_2006 <- cbind(seq.Date(from = as.Date("1999/01/01"), to = as.Date("2018/12/01"), by = "month"), MA_TAVG_2006)
colnames(MA_TAVG_2006) <- c("Date", "TAVG")
row.names(MA_TAVG_2006) <- 1:nrow(MA_TAVG_2006)

# Subset the data to contain values between August, 2005 and March, 2013.
MA_TAVG_2006 <- subset(MA_TAVG_2006, Date >= as.Date("2006/01/01") & Date <= as.Date("2006/12/31"))

# Finally, MA_TAVG_2006 contains the correct TAVG information that was needed by our previous Massachusetts
# data set. All we need to do is transfer the data to complete the old data set. We will do this below. 
# NOTE: This requires the loading of the MA_Weather and the USHCN_MA_Workspace R workspaces.

MA_Weather_Monthly[85:96, 6] <- MA_TAVG_2006$TAVG
















