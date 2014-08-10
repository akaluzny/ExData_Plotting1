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

Plot3 <- function() {
  # Load necessary data
  consumption <- LoadData()
  
  # Create graphics device for png file
  png(file = "plot3.png")

  with(consumption, {
    # Create a line for sub metering 1
    plot(datetime, Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l")
    # Create a line for sub metering 2
    lines(datetime, Sub_metering_2, type = "l", col = "red")
    # Create a line for sub metering 3
    lines(datetime, Sub_metering_3, type = "l", col = "blue")
  })
  
  # Add a legend
  legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1, 1))

  # Device off
  invisible(dev.off())
}