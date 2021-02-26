Plot1Run <-function(Run) {
  




library(dplyr)

Data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?",colClasses=c("character", "character","numeric","numeric","numeric","numeric",
                                                                                                              "numeric","numeric","numeric"))

Data$Date <- as.Date(Data$Date, "%d/%m/%Y" )


DataSub <- Data %>%
 filter( Date >"2007-01-31" & Date <"2007-02-02")

DataSub$Dayofweek <-weekdays(DataSub$Date)


png("plot.1.png",width = 480 , height = 480)
with(DataSub, hist(Global_active_power, col = "Red", 
main = "Global Active Power", xlab = "Global Active Power", ylab = "Frequency"))

dev.off()
}