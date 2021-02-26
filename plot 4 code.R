plot.4 <- function(Run)    {
  library(dplyr)
  
  Data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?",
                     colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
  
  Data$Date <- as.Date(Data$Date, "%d/%m/%Y" )
  Data <- Data %>%
    filter( Date >"2007-01-31" & Date <"2007-02-03")
  Data$Time<-strptime(paste(Data$Date,Data$Time),"%F %T")
  
  png("plot.4.png", width = 480, height = 480)
  par(mfcol=c(2,2), mar=c(4.5,5,2,2))
  with(Data, plot(Time , Global_active_power, type = 'l',
                  xlab =  "Global Active Power (kilowatts"))
  
  with(Data, plot(Time,Sub_metering_1, type = 'l', xlab =  "Global Active Power (kilowatts"), xlab = '')
  with(Data,lines(Time,Sub_metering_2,type = 'l', col = "Red"))
  with(Data,lines(Time,Sub_metering_3, type = 'l', col = "Blue"))
  
  
  with(Data,plot(Time,Voltage, ylab="Voltage", xlab="datetime", type="l"))
  
  with(Data,plot(Time,Voltage,ylab="Global_reactive_power", xlab="datetime", type="l"))
       
       dev.off()
       
}
