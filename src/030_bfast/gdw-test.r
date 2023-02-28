# Small test of BFAST Lite on Google Dynamic world data
library(zoo)
library(bfast)

source("../utils/utils.r")

input = "../../data/ee-chart-eyasi.csv"

gdata = read.csv(input)
# Remove duplicates
gdata = gdata[!duplicated(gdata), ]

# Remove NAs
gdata = gdata[complete.cases(gdata),]

# Parse dates
Sys.setlocale("LC_TIME", "C")
gdata$date = as.Date(gdata[["system.time_start"]], format="%b %e, %Y")

Classnames = names(gdata)[2:(ncol(gdata)-1)]

# Aggregate images on the same date
gdata = aggregate(as.formula(paste("cbind(", paste(Classnames, collapse=","), ")~date")), gdata, median)

# Plot
plot(bare~date, gdata, type="l", ylim=c(0,1))
for (i in 2:length(Classnames))
{
    lines(as.formula(paste(Classnames[i], "~ date")), gdata, col=i)
}

# Preprocess for bfast

# What's the frequency?
gdata$date[-1] - gdata$date[-nrow(gdata)] # All over the place! So assume daily

PointTS = as.zooreg(zoo(gdata[, Classnames], gdata[, "date"]))
Result = PointTS
mag_threshold = 0
    for (classidx in 1:length(Classnames))
    {
        InData = ts(as.ts(PointTS[,classidx]), start=c(2015, 179), frequency = 365.25)
        # Run model
        bfl = try(bfastlite(InData, h=floor(nrow(gdata)/5.357974), formula=response~trend, order=1))
        if (class(bfl) != "bfastlite") { # If it crashed, don't return any prediction
            if (fallback) return(RFSlice)
            return(NULL)
        }
        # Filter out breaks by magnitude (<20)
        BreakNo = if (!is.na(bfl$breakpoints$breakpoints[1])) length(bfl$breakpoints$breakpoints) else 0
        Magnitude=0
        if (BreakNo > 0)
        {
            Magnitude = magnitude(bfl$breakpoints, breaks=BreakNo)$Mag[,"RMSD"]
            while(min(Magnitude) < mag_threshold && BreakNo > 0)
            {
                BreakNo = BreakNo-1
                Magnitude = if (BreakNo > 0) magnitude(bfl$breakpoints, breaks=BreakNo)$Mag[,"RMSD"] else 0
            }
        }
        Result[,classidx] = as.numeric(fitted(bfl$breakpoints, breaks=BreakNo))
        #Result[is.na(InData),classidx] = NA
        print(plot(bfl, main=Classnames[classidx], sub=paste(round(Magnitude, 2), collapse = ", "), breaks=BreakNo))
    }
        coredata(Result) = as.matrix(ScalePredictions(coredata(Result)))
        plot(Result)

Result.df = as.data.frame(Result)
Result.df$date = time(Result)
# Plot
plot(bare~date, Result.df, type="l", ylim=c(0,100))
for (i in 2:length(Classnames))
{
    lines(as.formula(paste(Classnames[i], "~ date")), Result.df, col=i)
}
