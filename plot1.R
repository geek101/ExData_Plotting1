source("data.R")

# Plots Global Active Power frequency via histogram
# Use this if data.frame is already available.
# Controls generation of png file via pngImg param.
plot1Data <- function(plotdata, pngImg=FALSE) {
    hist(plotdata$Global_active_power[!is.na(plotdata$Global_active_power)], 
         main=("Global Active Power"), 
         xlab=paste("Global Active Power", "(kilowatts)", sep=""), 
         col="red")
    if (pngImg) {
        dev.copy(png, file=paste(pngdir, "plot1.png", sep=""), 
                 width = 480, height = 480, units = "px", bg = "white")
        dev.off()
    }
}

# Helper to read the data from csv, generates the png.
# Usage: plot1("household_power_consumption.txt", "1/2/2007", "2/2/2007")
plot1 <- function(fname, datefrom, dateto) {
    op <- par(mfrow = c(1, 1), mar = c(5.1, 5.1, 4.1, 2.1))
    plot1Data(readData(fname, datefrom, dateto), pngImg=TRUE)
    par(op)
}