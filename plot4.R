plot4 <- function(dates = c("2007/02/01", "2007/02/02")) {
  #reads dataset into dataframe"power" and converts date column to Date object
  power <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
                      na.strings = "?", stringsAsFactors = FALSE)
  power[[1]] <- as.Date(power[[1]], "%d/%m/%Y")
  
  wantedDate <- as.Date(dates, "%Y/%m/%d")
  
  timeframe <- subset(power, Date == wantedDate[1] | Date == wantedDate[2])
  
  #create a new column Datetime and convert it to POSIXct
  timeframe $Datetime <- with(timeframe, paste(Date,Time) )
  timeframe $Datetime <- as.POSIXct(timeframe$Datetime)
  
  #start the png device with name plot4.png
  png(filename="plot4.png", width = 480, height = 480)
  
  #plot line graph into png as specified
  par(mfrow = c(2,2))
  
  # recreate plot2 for topleft plot
  with(timeframe, plot(Datetime, Global_active_power, type = "l",xlab="", ylab="Global Active Power"))
  
  # plot voltage against datetme for topright plot
  with(timeframe, plot(Datetime,Voltage, type = "l", xlab ="datetime", ylab ="Voltage"))
  
  # recreate plot 3 for bottom left plot
  with(timeframe, plot(Datetime, Sub_metering_1 , type = "l",col="black", xlab="", ylab="Energy in sub metering"))
  with(timeframe, points(Datetime,Sub_metering_2, type = "l", col ="red"))
  with(timeframe, points(Datetime, Sub_metering_3, type="l", col="blue"))
  legend("topright", legend=c("Sub_metering 1","Sub_metering 2", "Sub_metering 3"),lty= c(1,1,1), col = c("black","red","blue"),bty="n")
  
  # plot global reactive power against datetime for bottom right plot
  with(timeframe, plot(Datetime, Global_reactive_power, type= "l", xlab = "datetime", ylab ="Global_reactive_power" ))
  
  #close png device to save plot4.png
  dev.off()
}