library(data.table)

# Main script function that executes all of the sub-steps necessary to generate out plot.
# If you wish to override the default work directory, callers can specify an alternative 
# location via the workDirectory parameter.
runPlotGeneration <- function(workDirectory = getwd()) {
  uciDataSetFile = uciDataSetPath(workDirectory)
  pullSourceData(uciDataSetFile, workDirectory)
  uciDataSet = loadUciDataSet(uciDataSetFile)
  constructPlot(uciDataSet, workDirectory)
}

# Handles the logic around actual plot creation given a clean data set and working directory
constructPlot <- function(uciDataSet, workDirectory) {
  png(filename = paste(workDirectory, 'plot4.png', sep = '/'))
  par(mfrow = c(2,2))
  with(uciDataSet, {
    # 1st plot
    plot(Time, Global_active_power, type = 'l', xlab = '', 
                        ylab = 'Global Active Power')
    # 2nd plot
    plot(Time, Voltage, type = 'l', xlab = 'datetime')
    #3rd plot
    plot(Time, Sub_metering_1, type = 'l', xlab = '', 
                        ylab = 'Energy sub metering')
    lines(Time, Sub_metering_2, col = 'red')
    lines(Time, Sub_metering_3, col = 'blue')
    legend('topright', col = c('black', 'red', 'blue'), 
         legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), lty = 1)
    # 4th plot
    plot(Time, Global_reactive_power, type = 'l', xlab = 'datetime')
  })
  dev.off()
}

##### Begin common (across all plot.R files) boilerplate functions for retrieving/loading data set
uciDataSetPath <- function(workDirectory) { paste(workDirectory, 'household_power_consumption.txt', sep='/') }

pullSourceData <- function(uciDataSetFile, workDirectory) {
  if(!file.exists(uciDataSetFile)) {
    uciDatasetUrl = 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    uciDataZipFile = paste(workDirectory, 'household_power_consumption.zip', sep = '/')
    if(!file.exists(uciDataZipFile)) {
      download.file(uciDatasetUrl, destfile = uciDataZipFile, method = 'curl')  
    }
    unzip(uciDataZipFile, exdir = workDirectory)
  }
}

loadUciDataSet <- function(uciDataSetFile, sep = ";") {
  dataSetColNames = colnames(fread(uciDataSetFile, sep = sep, nrows = 2))
  classes = c("character", "character", rep("numeric", 7))
  ds = fread(uciDataSetFile, sep = sep, na.strings = c("NA", "?"), colClasses = classes, 
             skip = "1/2/2007", nrows = 2880)
  setnames(ds, dataSetColNames)
  # To get a proper time value, combine date and time together before converting
  ds$Time = paste(ds$Date, ds$Time, " ")
  ds$Time = as.POSIXct(strptime(ds$Time, format = '%d/%m/%Y %H:%M:%S'))                   
  ds$Date = NULL
  ds
}

##### End boilerplate functions