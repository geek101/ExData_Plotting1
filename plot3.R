source("data.R")

# Generate 3 graphs in the same plot for 
# DateTime vs Sub Metring [1,2 3]
# Remove NA before ploting.
# Controls png image generation via pngImg parameter.
# Controls Legend print via lengnd parameter.
# Controls font size of legend via cex parameter.
plot3Data <- function(plotdata, legendBoundary=TRUE, cex=0.5, pngImg=FALSE) {
    ydata <- plotdata[, c(6:ncol(plotdata))]
    cset <- c("black", "red", "blue")
    lset <- names(plotdata[c(6:ncol(plotdata))])
    yrange <- range(c(plotdata$Sub_metering_1[!is.na(plotdata$Sub_metering_1)], 
                      plotdata$Sub_metering_2[!is.na(plotdata$Sub_metering_2)],
                      plotdata$Sub_metering_3[!is.na(plotdata$Sub_metering_3)]))
    with(plotdata, {
         plot(DateTime[!is.na(Sub_metering_1)], 
              y=Sub_metering_1[!is.na(Sub_metering_1)], ylim = yrange,
              type="l", col=cset[1], xlab="", ylab="Energy Sub metering")
        lines(x=DateTime, y=Sub_metering_2[!is.na(Sub_metering_2)], 
              type="l", col=cset[2])
        lines(x=DateTime, y=Sub_metering_3[!is.na(Sub_metering_3)], 
              type="l", col=cset[3])
        if (!legendBoundary) legend("topright", lty = 1, bty="n",
                           col=cset, legend=lset, cex=cex)
        else
            legend("topright", lty = 1,
                   col=cset, legend=lset, cex=cex)
        })
    if (pngImg) {
        dev.copy(png, file=paste(pngdir, "plot3.png", sep=""), 
                 width = 480, height = 480, units = "px", bg = "white")
        dev.off()
    }
}

# Helper to read data,plot and generate image.
# Usage: plot3("household_power_consumption.txt", "1/2/2007", "2/2/2007")
plot3 <- function(fname, datefrom, dateto) {
    op <- par(mfrow = c(1, 1), mar = c(5.1, 5.1, 4.1, 2.1))
    plot3Data(readData(fname, datefrom, dateto), pngImg=TRUE)
    par(op)
}