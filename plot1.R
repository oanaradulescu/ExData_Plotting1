# Our overall goal here is simply to examine how household energy usage varies 
# over a 2-day period in February, 2007. Your task is to reconstruct the following 
# plots below, all of which were constructed using the base plotting system.

library(sqldf)

# build a tidy data set to support our goal

powerfile <- "household_power_consumption.txt"
powerfebsql <- "SELECT * from file WHERE Date = '1/2/2007' OR Date = '2/2/2007'"
power <- read.csv2.sql(powerfile, powerfebsql)

str(power)

# rename existing Date column
names(power)[1] <- "DateAsString"
# add new Date column of type Sys.Date()
power$Date <- as.Date(power$DateAsStr, "%d/%m/%Y")

# Warning
# This is a convenience function intended for use interactively. 
# For programming it is better to use the standard subsetting arithmetic functions, 
# and in particular the non-standard evaluation of argument transform 
# can have unanticipated consequences.

# power <- transform(power, Date = as.Date(power$DateAsStr, "%d/%m/%Y"))


plot <- function() {
#     par(fig=c(0, 1, .2, .8))
    hist(power$Global_active_power, col="red", ylim=c(0,1200)
         , main="Global Active Power"
#        , cex.axis=0.7
#        , cex.lab=0.8
#        , cex.main=0.9
         , xlab="Global Active Power (kilowatts)", ylab="Frequency"
#        , mgp=c(2.25,1,0)

    )
}



png(file="plot1.png"
    , width=504
    , height=504
    , bg="transparent")
plot()
dev.off()

