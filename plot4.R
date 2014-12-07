plot4 <- function(){
    options(warn=-1)
    library(plyr)
    library(dplyr)
    library(data.table)
    # Set the working directory to the GitHub path
    strPathOut <- paste("C:","Programmeren","workspace","GitHub","ExData_Plotting1",sep="\\")
    setwd(strPathOut)
    
    # The path of the power consumption file
    strPath <- paste("C:","Programmeren","Bronbestanden","Exploratory Data Analysis",sep="\\")
    strFile <- "household_power_consumption.txt"
    strNameFileIn <- paste(strPath,strFile,sep="\\")
    
    # Read in the data using data.table's "fread" function
    dfPower <- fread(strNameFileIn)
    
    # Use regex to grep the indices of the designated dates
    lsAinIndices <- grep("^[1|2]/2/2007", dfPower$Date,perl = TRUE)
    
    # Apply the indices to the data frame
    dfPower <- dfPower[lsAinIndices]
    
    # Plot-specific data extraction
    Sys.setlocale("LC_TIME", "English")
    # Data plot 1,1
    lsAdoGlobActPow <- as.numeric(dfPower$Global_active_power)
    # Data plot 1,2
    lsAdoVoltage <- as.numeric(dfPower$Voltage)
    # Data plot 2,1
    dfSubMetering <- transmute(dfPower,
                               Sub_metering_1 = as.numeric(Sub_metering_1),
                               Sub_metering_2 = as.numeric(Sub_metering_2),
                               Sub_metering_3 = as.numeric(Sub_metering_3))
    # Data plot 2,2
    lsAdoGlobReacPow <- as.numeric(dfPower$Global_reactive_power)
    lsAstrDateTime <- ddply(dfPower,.(Date,Time),function(dfPower) paste(dfPower[[1]],dfPower[[2]],sep=" "))[,3]
    strTimeFormat <- "%e/%m/%Y %T"
    lsAstrDateTimeConv <- strptime(lsAstrDateTime,strTimeFormat)

    # Set the png device
    # Files will be stored in the current working directory
    png(filename = "plot4.png",
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "transparent", res = NA, restoreConsole = TRUE,
        type = "windows")
    par(mfrow=c(2,2))
    strType = "l"
    plot(lsAstrDateTimeConv,
         lsAdoGlobActPow,
         type=strType,
         xlab="",
         ylab="Global Active Power (kilowatts)",
         lty=1)
    plot(lsAstrDateTimeConv,
         lsAdoVoltage,
         type=strType,
         xlab="datetime",
         ylab="Voltage",
         lty=1)
    plot(lsAstrDateTimeConv,
         dfSubMetering$Sub_metering_1,
         col="black",
         type=strType,
         xlab="",
         ylab="Energy sub metering",
         lty=1)
    lines(lsAstrDateTimeConv,
          dfSubMetering$Sub_metering_2,
          col="red")
    lines(lsAstrDateTimeConv,
          dfSubMetering$Sub_metering_3,
          col="blue")
    legend("topright",
           lty=1,
           col = c("black","blue", "red"),
           legend = names(dfSubMetering),
           bty = "n")
    plot(lsAstrDateTimeConv,
         lsAdoGlobReacPow,
         type=strType,
         xlab="datetime",
         ylab="Global_reactive_power",
         lty=1)
    dev.off()
    options(warn=0)
}