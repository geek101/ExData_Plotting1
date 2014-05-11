source("data.R")
source("plot1.R")
source("plot2.R")
source("plot3.R")

# Function to generate graph DateTime vs Voltage.
# pngImg parameter controls the generation of image.
plotVoltData <- function(plotdata, pngImg=FALSE) {
    with(plotdata, { 
        plot(x=DateTime[!is.na(Voltage)], y=Voltage[!is.na(Voltage)], type="l", xlab="",
             ylab="Voltage") })
    if (pngImg) {
        dev.copy(png, file=paste(pngdir, "plotVolt.png", sep=""), 
                 width = 480, height = 480, units = "px", bg = "white")
        dev.off()
    }
}

# Helper to read data,plot and generate image.
# Usage: plotVolt("household_power_consumption.txt", "1/2/2007", "2/2/2007")
plotVolt <- function(fname, datefrom, dateto) {
    op <- par(mfrow = c(1, 1), mar = c(5.1, 5.1, 2.1, 2.1))
    plotVoltData(readData(fname, datefrom, dateto), pngImg=TRUE)
    par(op)
}

# Function to generate graph DateTime vs Global_reactive_power
# pngImg parameter controls the generation of image.
plotReactiveData <- function(plotdata, pngImg=FALSE) {
    yrange <- range(
        c(plotdata$Global_reactive_power
            [!is.na(plotdata$Global_reactive_power)]))
    yv = seq(yrange[1], yrange[2], by=0.1 )
    with(plotdata, { 
        plot(x=DateTime[!is.na(Global_reactive_power)], 
             y=Global_reactive_power[!is.na(Global_reactive_power)], 
             type="l", xlab="", ylab="Global_reactive_power")
        axis(2, at=yv, cex=0.5)
        })
    if (pngImg) {
        dev.copy(png, file=paste(pngdir, "plotReact.png", sep=""), 
                 width = 480, height = 480, units = "px", bg = "white")
        dev.off()
    }
}

# Helper to read data,plot and generate image.
# Usage: plotReactive("household_power_consumption.txt", "1/2/2007", "2/2/2007")
plotReactive <- function(fname, datefrom, dateto) {
    op <- par(mfrow = c(1, 1), mar = c(5.1, 5.1, 2.1, 2.1))
    plotReactiveData(readData(fname, datefrom, dateto), TRUE)
    par(op)
}

# Generate the 4 following graphs
# 1. DateTime vs Global Active Power
# 2. DateTime vs Voltage
# 3. DateTime vs Sub Metring [1,2 3]
# 4. DateTime vs Global_reactive_power
# pngImg parameter controls the image generation.
plot4Data <- function(plotdata, pngImg=FALSE) {
    # par to allow for 2x2 , more left margin!.
    op <- par(mfrow = c(2, 2), mar = c(4, 5, 1, 1), oma=c(0,0,2,0))
    plot2Data(plotdata)               ## DateTime vs Global Active Power
    plotVoltData(plotdata)            ## DateTime vs Voltage 
    mtext(side=1, "datetime", line=3) ## X-axis label for both the above
    plot3Data(plotdata, legendBoundary=FALSE, cex=0.5) # DateTime vs Sub Metring [1,2 3]
    plotReactiveData(plotdata)        ## DateTime vs Global_reactive_power
    mtext(side=1, "datetime", line=3) ## X-axis label for both the above
    if (pngImg) {
        dev.copy(png, file=paste(pngdir, "plot4.png", sep=""),
                 width = 480, height = 480, units = "px", bg = "white")
        dev.off()
    }
    par(op)
}

# Helper to read data,plot and generate image.
# Usage: plot4("household_power_consumption.txt", "1/2/2007", "2/2/2007")
plot4 <- function (fname, datafrom, dateto){
    plot4Data(readData(fname, datefrom, dateto), pngImg=TRUE)
}