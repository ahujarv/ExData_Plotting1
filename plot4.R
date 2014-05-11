plot4 <- function() {
        ## Get the dataframe to work with
        dataSet <- getData()
        
        ## Convert values of Global_active_power, Voltage and 
        ## Global_reactive_power to numeric
        gap <- with(dataSet, as.numeric(as.character(Global_active_power)))
        voltage <- with(dataSet, as.numeric(as.character(Voltage)))
        grp <- with(dataSet, as.numeric(as.character(Global_reactive_power)))
        
        ## Open png device with given width and height
        png("plot4.png", width=480, height=480)
        
        ## Set rows and columns for the plots
        par(mfcol = c(2, 2), bg="transparent")
        
        ## Plot Global_active_power
        plot(dataSet$Time, gap, type="l", xlab="", 
             ylab="Global Active Power", bg="transparent")
        
        ## Plot Sub_metering_1, Sub_metering_2 and Sub_metering_3
        ## First plot Sub_metering_1 data
        plot(dataSet$Time, dataSet$Sub_metering_1, type="l", xlab="", 
             ylab="Energy sub metering", bg="transparent")
        
        ## Add line for Sub_metering_2 
        lines(dataSet$Time, dataSet$Sub_metering_2, col="red")
        
        ## Add line for Sub_metering_3
        lines(dataSet$Time, dataSet$Sub_metering_3, col="blue")
        
        ## Set text and color for Legend
        legendText <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        legendColor <- c("black", "red", "blue")
        
        ## Add legend
        legend("topright", lty = "solid", col=legendColor, legend=legendText, 
               bty='n')
        
        ## Plot Voltage data
        plot(dataSet$Time, voltage, type="l", xlab="datetime", 
             ylab="Voltage", bg="transparent")
        
        ## Plot Global_reactive_power  
        plot(dataSet$Time, grp, type="l", xlab="datetime", 
             ylab="Global_reactive_power", bg="transparent")
        
        ## Turn off the png device
        dev.off()
}

getData <- function() {
        ## Location of the input zip file
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        
        ## Download the file in working directory, if the file does not already
        ## exist, else use the zip file from the working directory
        if(!file.exists("household_power_consumption.zip")) {
                srcFile <- download.file(fileUrl, "household_power_consumption.zip")                
        } else {
                srcFile <- "household_power_consumption.zip"
        }
        
        ## Unzip the downloaded file 
        inputFile <- unz(srcFile, "household_power_consumption.txt")
        
        ## Read the  dataset from the input file into a data frame
        ## Account for the separator character, which is ";" 
        ## Account for missing values which are indicated by "?" 
        hpc <- read.table(inputFile, header=TRUE, 
                          sep=";", na.strings = "?")
        
        ## Subset the dataset for the requied dates
        subhpc <- hpc[as.character(hpc$Date) %in% c("1/2/2007", "2/2/2007"),]
        
        ## Get the values in Date Column and convert them to be of class Date
        dates <- with(subhpc, as.Date(Date, format="%d/%m/%Y")) 
        
        ## Get the values in Date and Time coloumns
        ## Concatenate these values and convert into Date/Time class
        dateAndTime <- with(subhpc,
                            strptime(paste(Date, Time),
                                     "%d/%m/%Y %H:%M:%S"))
        
        ## Use the vectors of dates and dateAndTime created above along with 
        ## other columns from original dataset to create a new dataframe.  This
        ## dataframe contains the subset of data required with Date/Time columns
        ## in appropriate format.  The separator and na character has also been
        ## taken care of.
        inphpc <- data.frame(Date=dates, Time=dateAndTime, subhpc[3], subhpc[4],
                             subhpc[5], subhpc[6], subhpc[7], subhpc[8], subhpc[9]) 
        
        ## Return the dataframe
        inphpc 
}