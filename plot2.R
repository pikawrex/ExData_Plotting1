plot2 <- function(dates = c("2007/02/01", "2007/02/02")) {
  #reads dataset into dataframe"power" and converts date column to Date object
  power <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
                      na.strings = "?", stringsAsFactors = FALSE)
  power[[1]] <- as.Date(power[[1]], "%d/%m/%Y")

  wantedDate <- as.Date(dates, "%Y/%m/%d")
  
  timeframe <- subset(power, Date == wantedDate[1] | Date == wantedDate[2])

  #create a new column Datetime and convert it to POSIXct
  timeframe $Datetime <- with(timeframe, paste(Date,Time) )
  timeframe $Datetime <- as.POSIXct(timeframe$Datetime)
  
  #start the png device with name plot2.png
  png(filename="plot2.png", width = 480, height = 480)
  
  #plot line graph into png as specified
  with(timeframe, plot(Datetime, Global_active_power, type = "l",xlab="", ylab="Global Active Power (kilowatts)"))
  
  #close png device to save plot2.png
  dev.off()
}