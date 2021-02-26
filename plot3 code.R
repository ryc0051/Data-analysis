
plot.3 <- function(Run)    {
  library(dplyr)
  
  Data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?",
                     colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
  
  Data$Date <- as.Date(Data$Date, "%d/%m/%Y" )
  Data <- Data %>%
    filter( Date >"2007-01-31" & Date <"2007-02-03")
  Data$Time<-strptime(paste(Data$Date,Data$Time),"%F %T")
  

  
  png("plot.3.png",width = 480 , height =480)
  with(Data, plot(Time,Sub_metering_1, type = 'l', xlab =  "Global Active Power (kilowatts"), xlab = '')
  with(Data,lines(Time,Sub_metering_2,type = 'l', col = "Red"))
  with(Data,lines(Time,Sub_metering_3, type = 'l', col = "Blue"))
  legend("topright", lwd=1, col=c("black", "red", "blue"), legend=names(Data[,7:9]))
  dev.off()
  
}