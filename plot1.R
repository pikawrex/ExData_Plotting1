plot1 <- function(dates = c("2007/02/01", "2007/02/02")) {
  #reads dataset into dataframe"power" and converts date column to Date object
  power <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
                      na.strings = "?", stringsAsFactors = FALSE)
  power[[1]] <- as.Date(power[[1]], "%d/%m/%Y")
  
  wantedDate <- as.Date(dates, "%Y/%m/%d")
  
  timeframe <- subset(power, Date == wantedDate[1] | Date == wantedDate[2])
  
  #start png device
  png(filename="plot1.png", width = 480, height = 480)
  #plot histogram into png as specified
  hist(timeframe$Global_active_power,breaks = 11, col= "red", main = "Global Active Power"
        ,xlab= "Global Active Power (kilowatts)")
  #close png device to save plot1.png
  dev.off()
  }