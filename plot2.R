plot2 <- function(){
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
    lsAdoGlobActPow <- as.numeric(dfPower$Global_active_power)
    lsAstrDateTime <- ddply(dfPower,.(Date,Time),function(dfPower) paste(dfPower[[1]],dfPower[[2]],sep=" "))[,3]
    strTimeFormat <- "%e/%m/%Y %T"
    lsAstrDateTimeConv <- strptime(lsAstrDateTime,strTimeFormat)
    
    # Set the png device
    # Files will be stored in the current working directory
    png(filename = "plot2.png",
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "transparent", res = NA, restoreConsole = TRUE,
        type = "windows")
    plot(lsAstrDateTimeConv,
         lsAdoGlobActPow,
         type="l",
         xlab="",
         ylab="Global Active Power (kilowatts)",
         lty=1)
    dev.off()
    options(warn=0)
}