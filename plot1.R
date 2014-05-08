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

plot1 <- function() {
    hist(power$Global_active_power, col="red"
         , main="Global Active Power"
         , xlab="Global Active Power (kilowatts)", ylab="Frequency"
    )
}

png(file="plot1.png"
    , width=480
    , height=480
    , bg="transparent")
plot1()
dev.off()

