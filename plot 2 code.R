
  plot.3 <- function(Run)    {
  library(dplyr)
  
  Data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, na.strings = "?",
  colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
  
  Data$Date <- as.Date(Data$Date, "%d/%m/%Y" )
  Data$Time<-strptime(paste(Data$Date,Data$Time),"%F %T")
  
  Data <- Data %>%
    filter( Date >"2007-01-31" & Date <"2007-02-03")

  
  
  
  png("plot.2.png", width = 480 , height =480)
    with(Data, plot(Time , Global_active_power, type = 'l',
  xlab =  "Global Active Power (kilowatts"))
  dev.off()
 
}