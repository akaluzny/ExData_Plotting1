LoadData <- function() {
  # Download from specified location
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                "exdata-data-household_power_consumption.zip", method = "curl")
  
  # Unzip
  #unzip("exdata-data-household_power_consumption.zip")
  
  # Read table with header and columns separated by ;
  consumption <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
  
  # Filter only by observations from 2007-02-01 and 2007-02-02
  consumption <- consumption[consumption$Date == "1/2/2007" | consumption$Date == "2/2/2007", ]
  
  # Convert all numeric observations to numbers from strings
  for(i in 3:9) {
    consumption[, i] <- as.numeric(consumption[, i])
  }
  
  # Create a new column with date and time in the appropriate format
  consumption$datetime <- strptime(paste(consumption$Date, consumption$Time), format = "%d/%m/%Y %H:%M:%S")
  
  consumption
}

Plot2 <- function() {
  # Load necessary data
  consumption <- LoadData()
  
  # Create graphics device for png file
  png(file = "plot2.png")
  
  # Create a line for global active power in kilowatts by time
  with(consumption, plot(datetime, Global_active_power / 1000, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
  
  # Device off
  invisible(dev.off())
}