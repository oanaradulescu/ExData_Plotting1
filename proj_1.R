setwd("/Users/oana/Documents/Education/MOOC/Pop Datography | Telling Stories with Data/Data Science @ coursera/exploratory data analysis")
getwd()

library(sqldf)

# build a tidy data set to support our goal -
# examine how household energy usage varies over a 2-day period in February 2007

powerfile <- "household_power_consumption.txt"
powerfebsql <- "SELECT * from file WHERE Date = '1/2/2007' OR Date = '2/2/2007'"
power <- read.csv2.sql(powerfile, powerfebsql)

str(power)

# rename existing Date column
names(power)[1] <- "DateAsString"
# add new Date column of type Sys.Date()
power$Date <- as.Date(power$DateAsStr, "%d/%m/%Y")

# power <- transform(power, Date = as.Date(power$DateAsStr, "%d/%m/%Y"))

plot.new()
hist(power$Global_active_power, col="red", ylim=c(0,1200),
     main="Global Active Power",
     cex.axis=0.7,
     cex.lab=0.8,
     cex.main=0.9,
     xlab="Global Active Power (kilowatts)", ylab="Frequency")

