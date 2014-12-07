plot1 <- function(){
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
    lsAdoGlActPower <- as.numeric(dfPower$Global_active_power)
    lsAdoGlActPowerNNA <- lsAdoGlActPower[!is.na(lsAdoGlActPower)]
    
    # Set the png device
    # Files will be stored in the current working directory
    png(filename = "plot1.png",
        width = 480, height = 480, units = "px", pointsize = 12,
        bg = "white", res = NA, restoreConsole = TRUE,
        type = "windows")
    hist(lsAdoGlActPowerNNA,
         main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)",
         ylab = "Frequency",
         col="red")
    dev.off()
    options(warn=0)
}