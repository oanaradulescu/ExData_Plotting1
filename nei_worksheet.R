# andy kirk :: data visualization, a successful design process
# establishing intent - the visualization's function - provide an interface 
# to data in order to facilitate visual exploration
# establishing intent - the visualization's tone - pragmatic and analytical - 
# creating a visualization with a pragmatic tone is about recognizing 
# a need for a design that delivers fast, efficient and precise portrayals of data

# TODO show actual years in data set

getwd()
setwd("/Users/oana/Documents/Education/MOOC/Pop Datography | Telling Stories with Data/Data Science @ coursera/4 exploratory data analysis/exdata-data-NEI_data")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

install.packages("dplyr")
library(dplyr)

library(scales)

# The bottleneck in most data analyses is the time it takes 
# for you to figure out what to do with your data, 
# and dplyr makes this easier by having individual functions that correspond 
# to the most common operations (group_by, summarise, mutate, filter, select and arrange). 
# Each function does one only thing, but does it well.

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

emissionsperyr <- NEI %.%
    group_by(year) %.%
    summarise(totalemissions = sum(Emissions))

pm25.1 <- function() {

    par(mar = c(5, 5, 4, 1) + 0.1)
    par(mgp = c(3, 1, 0.25))
    
    with(emissionsperyr, plot(year, totalemissions, type = "b", 
                              ann = FALSE, 
                              xaxt = "n", yaxt = "n"))
    with(emissionsperyr, axis(1, at = year, las = 0, cex.axis = 0.75))
    with(emissionsperyr, axis(2, at = totalemissions, 
                               labels = prettyNum(totalemissions, big.mark = ","),
                               las = 2, cex.axis = 0.75))
    title(main = "PM2.5 Emissions, United States", cex.main = 0.95)
    mtext("Year", side = 1, line = 2, cex = 0.85, font = 2)
    mtext("PM2.5 Emitted (Tons)", side = 2, line = 4, cex = 0.85, font = 2)
    
    with(emissionsperyr, abline(h = totalemissions, col = '#000000', lty = 3))
    with(emissionsperyr, abline(v = year, col = '#000000', lty = 3))

}


png(file = "pm2.5emissions-us.png")
pm25.1()
dev.off()

# TODO
# format y-axis ticks
# format y-axis label
# better align the x-axis values (1999, 2002, 2005, 2008) with the x-axis ticks
# prettify: make the point outline bolder, improve typography
# experiment with line plot vs. scatterplot

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.


pm25.2 <- function() {

    emissionsperyr.bc <- filter(NEI, fips == "24510") %.%
        group_by(year) %.%
        summarise(totalemissions = sum(Emissions))
    
    with(emissionsperyr.bc, plot(year, totalemissions, type = "b", ann = FALSE, las = 2, xaxt = "n"))
    with(emissionsperyr.bc, axis(1, at = year))
    title(main = "PM2.5 Emissions, Baltimore City, Maryland",
          xlab = "Year",
          ylab = "PM2.5 Emitted (Tons)")

}

png(file = "pm2.5emissions-bc.png")
pm25.2()
dev.off()

library(ggplot2)

ggplot(emissionsperyr.bc, aes(x=year, y=totalemissions)) + geom_point()
qplot(year, totalemissions, data=emissionsperyr.bc)

qplot(year, totalemissions, data=emissionsperyr.bc, geom="line")
ggplot(emissionsperyr.bc, aes(x=year, y=totalemissions)) + geom_line()

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases 
# in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

pm25.3 <- function() {

    library(ggplot2)
    
    emissionsperyrntype.bc <- filter(NEI, fips == "24510") %.%
        group_by(year, type) %.%
        summarise(totalemissions = sum(Emissions))
    
    e <- ggplot(emissionsperyrntype.bc, aes(x=year, y=totalemissions, color=type)) + geom_line() + geom_point()
    e + ggtitle("PM2.5 Emissions by Source Type\nBaltimore City") + 
        scale_color_discrete(name = "Source Type") + 
        ylab("PM2.5 Emitted (Tons)") +
        xlab("Year")
    e

}

png("pm2.5emissions-bc-bysourcetype.png")
pm25.3()
dev.off()


# TODO
# add title
# format axis labels
# format axis ticks
# improve legend: order, typography
# add a horizontal line to help answer emissions decrease/increase for point sources

# 4. Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999–2008?

str(NEI)
str(SCC)

SCC %.%
    group_by(Data.Category) %.%
    summarise(unique(Data.Category))

unique(SCC$Data.Category)
scc.names <- unique(SCC$Short.Name)

?grepl

pm25.4 <- function() {

    # rows <- grep("[Cc][Oo][Aa][Ll]", SCC$Short.Name)
    rows <- grep("coal", SCC$Short.Name, ignore.case = TRUE)
    coal.scc <- SCC[rows,"SCC"]
    
    emissionsperyr.coal <- NEI %.%
        filter(SCC %in% coal.scc) %.%
        group_by(year) %.%
        summarise(coalemissions = sum(Emissions))
    
    e <- ggplot(emissionsperyr.coal, aes(year, coalemissions)) + geom_point() + geom_line()
    e + ggtitle("PM2.5 Emissions from Coal Combustion-Related Sources\nUnited States") + 
        scale_y_continuous(labels = comma) +
        ylab("PM2.5 Emitted (Tons)") +
        xlab("Year")
    e
}

png(file = "pm2.5emissions-coal.png")
pm25.4()
dev.off()

?scale_y_continuous

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

pm25.5 <- function() {
    emissionsperyr.bc.motorvehicles <- NEI %.%
        filter(type == "ON-ROAD", fips == "24510") %.%
        group_by(year) %.%
        summarise(onroademissions = sum(Emissions))
    
    e <- ggplot(emissionsperyr.bc.motorvehicles, aes(year, onroademissions)) + geom_point() + geom_line()
    e + ggtitle("PM2.5 Emissions from Motor Vehicle Sources\nBaltimore City") +
        scale_y_continuous(labels = comma) +
        ylab("PM2.5 Emitted (Tons)") + 
        xlab("Year")
}

png(file = "pm2.5emissions-bc-motorvehicles.png")
pm25.5()
dev.off()

# 6. Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

pm25.6 <- function() {

    emissionsperyr.bcla.motorvehicles <- NEI %.%
        filter(type == "ON-ROAD", fips == "24510" | fips == "06037") %.%
        group_by(year, fips) %.%
        summarise(onroademissions = sum(Emissions))
    
    e <- ggplot(emissionsperyr.bcla.motorvehicles, aes(x=year, y=onroademissions, color=fips)) + geom_point() + geom_line()
    e + ggtitle("PM2.5 Emissions from Motor Vehicle Sources\nBaltimore City vs. Los Angeles County") +
        scale_y_continuous(labels = comma) +
        scale_color_discrete(name = "Location",
                             breaks = c("24510", "06037"),
                             labels = c("Baltimore City", "Los Angeles\nCounty")) +
        ylab("PM2.5 Emitted (Tons)") +
        xlab("Year")
}

png("pm2.5emissions-bcla-motorvehicles.png")
pm25.6()
dev.off()

# parallel vectors used as arguments to scale_color_discrete

par(mfrow = c(1, 3))

pm25.1()
pm25.2()

# need facets

e3 <- pm25.3()
e4 <- pm25.4()

par(mfrow = c(2, 1))
e3; e4

pm25.5()
pm25.6()


