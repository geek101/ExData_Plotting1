# readData() , helps with reading the input
# given the data range.
# Algo:
#   use readLines() and grep to isolate the lines
#   we want to import via read.csv2().
#   read.csv2() uses ";" for seperator so we are good.
#   After reading the raw data, conver the date and time
#   text fields to Date class and replace "?" with NA.
# fname - Filename to readfrom.
# datefrom - starting date (has to match date format in file)
# dateto - ending date (has to match date from in file)
# example: rd <- readData("household_power_consumption.txt", "1/2/2007", "2/2/2007")
# TODO: Support real date range, currently only supports two dates! not
# a range.
readData <- function(fname, datefrom, dateto) {
    # Build regex to grep for the right lines.
    exp1 <- paste("^", dateto, sep="")
    exp2 <- paste("^", datefrom, sep="")
    exp <- paste(exp2, exp1, sep="|")
    ## Get the lines that match the date range.
    linesFeb <- grep(exp, 
                     lines <- readLines(fname, n = -1))
    ## Till where we have to skip so we do not have to import all lines.
    skiptill <- linesFeb[1] - 1
    ## Number of rows to read so we can skip the rest!.
    rowstoread <-length(linesFeb)
    ## Save the header for later.
    header <- read.csv2(fname, nrows=1)
    ## Get the data that matched both the dates above,
    ## read the TODO!.
    mydata <- read.csv2(fname,
                        header=FALSE, skip=skiptill, nrows=rowstoread, 
                        colClasses = c(rep("character", ncol(header))))
    ## copy the header.
    names(mydata) <- names(header)
    
    ## Iterate through our frame for all rows.
    ## Convert date and time char to d/t format.
    ## Convert values to numbers and ? to NA
    v3 <- paste(mydata[[1]], mydata[[2]], sep = " ")
    vc <- strptime(v3, format="%d/%m/%Y %H:%M:%S")

    ## Replace "?" with NA.
    vl <- mydata[, c(3:ncol(header))]
    for(i in 1:ncol(vl)) {
        vq <- vl[[i]] == "?"
        vl[[i]][vq] == "NA"
        ## Convert to numeric so we can use the data.
        vl[[i]] <- as.numeric(vl[[i]])
    }

    ## Build the new data frame so we can easily plot the data.
    retdata = data.frame(DateTime=vc, vl)
    return(retdata)
}

# Global Vars.
# Set these if u want to change for all that use them.
fname="household_power_consumption.txt"
datefrom="1/2/2007"
dateto="2/2/2007"
pngdir="figure/"