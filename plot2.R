source("data.R")

# Plots DateTime vs Global Active Power
# Remove the NA items before plotting!
# pngImg param controls the image generation.
plot2Data <- function(plotdata, pngImg=FALSE) {
    with(plotdata, { 
        plot(x=DateTime[!is.na(Global_active_power)], 
        y=Global_active_power[!is.na(Global_active_power)],
        type="l", xlab="",
        ylab=paste("Global Active Power", "(kilowatts)", sep="")) })
    if (pngImg) {
        dev.copy(png, file=paste(pngdir, "plot2.png", sep=""), 
                 width = 480, height = 480, units = "px", bg = "white")
        dev.off()
    }
}

# Helper function to read data from file and call the
# function to plot and generate image
# Usage: plot2("household_power_consumption.txt", "1/2/2007", "2/2/2007")
plot2 <- function(fname, datefrom, dateto) {
    op <- par(mfrow = c(1, 1), mar = c(5.1, 5.1, 4.1, 2.1))
    plot2Data(readData(fname, datefrom, dateto), pngImg=TRUE)
    par(op)
}