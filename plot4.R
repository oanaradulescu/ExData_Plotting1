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

plot4 <- function() {
    par(mfrow=c(2,2))

    with(power, {
    
        plot(DateTime, Global_active_power
             , type="l"
             , xlab="", ylab="Global Active Power")
    
        plot(DateTime, Voltage
             , type="l"
             , xlab="datetime", ylab="Voltage")
    
        maxsm <- max(Sub_metering_1, Sub_metering_2, Sub_metering_3)
        plot(DateTime, Sub_metering_1
             , type="l"
             , ylim=c(0,maxsm)
             , ann=FALSE)
        lines(DateTime, Sub_metering_2, col="red")
        lines(DateTime, Sub_metering_3, col="blue")
        title(xlab="")
        title(ylab="Energy sub metering")
        legend(x="topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
               , col=c("black", "red", "blue"), lty=1)
    
        plot(DateTime, Global_reactive_power
             , type="l"
             , xlab="datetime")
    
    })
}

png(file="plot4.png"
    , width=504
    , height=504
    , bg="transparent")
plot4()
dev.off()

