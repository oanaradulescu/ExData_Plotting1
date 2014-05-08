# Assumes household_power_consumption.txt is in the working directory

# Our overall goal here is simply to examine how household energy usage varies 
# over a 2-day period in February, 2007. Your task is to reconstruct the following 
# plots below, all of which were constructed using the base plotting system.

install.packages("sqldf")
library(sqldf)

# build a tidy data set to support our goal

tidy.power <- function() {
    powerfile <- "household_power_consumption.txt"
    powerfebsql <- "SELECT * from file WHERE Date = '1/2/2007' OR Date = '2/2/2007'"
    power <- read.csv2.sql(powerfile, powerfebsql)
    power
}

power <- tidy.power()
power$DateTime <- strptime(paste(power$Date, power$Time), "%d/%m/%Y %H:%M:%S")

plot3 <- function() {
    max <- max(power$Sub_metering_1, power$Sub_metering_2, power$Sub_metering_3)
    plot(power$DateTime, power$Sub_metering_1
         , type="l"
         , ylim=c(0,max)
         , ann=FALSE)
    lines(power$DateTime, power$Sub_metering_2, col="red")
    lines(power$DateTime, power$Sub_metering_3, col="blue")
    title(xlab="")
    title(ylab="Energy sub metering")
    legend(x="topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
           , col=c("black", "red", "blue"), lty=1)
}

png(file="plot3.png"
    , width=480
    , height=480
    , bg="transparent")
plot3()
dev.off()

