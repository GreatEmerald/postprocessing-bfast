# Postprocess RF predictions using BFAST
# This is our new method in which we try to make use of BFAST Lite models to better predict LC change

# Input: RF fraction predictions on validation data
# Output: The same schema, but the predicions are altered based on the BFAST Lite model
library(sf)
library(zoo)
library(bfast)
library(pbapply)

source("../utils/utils.r")
source("../utils/covariate-names.r")
source("../utils/load-sampling-data.r")

# Read in reference and predictions
RFFile = "../../data/predictions/summer/median_predictions.rds"
#ReferenceFile = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
OutFile = "../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions.rds"
if (!dir.exists(dirname(OutFile)))
    dir.create(dirname(OutFile))

RFPredictions = readRDS(RFFile)
RFPredictions$dataYear = lubridate::year(RFPredictions$timestamp.x)

#Reference = read.csv(ReferenceFile)
#Reference = RenameReferenceData(Reference)
#Reference = TidyData(Reference)

# Try to normalise and sort predictions - not sure if it's useful here or harmful, needs testing
RFPredictions = RFPredictions[order(RFPredictions$location_id, RFPredictions$timestamp.x),]

ScaledPredictions = RFPredictions
ScaledPredictions[GetCommonClassNames()] = ScalePredictions(ScaledPredictions[GetCommonClassNames()])

# Function for taking a data.frame of a location and producing a BFAST Lite model fit as an output
# location_id is an integer of the location ID of interest
# ... are bfastlite() parameters
# fallback=TRUE means that on error, we return unchanged data. Else, we return NA
FitBFL = function(location_id, scaled=TRUE, mag_threshold=20, plot=FALSE, formula=response~trend, h=23, fallback=FALSE, ...)
{
    RFSlice = if (scaled) ScaledPredictions[ScaledPredictions$location_id == location_id, ] else
        RFPredictions[RFPredictions$location_id == location_id, ]
    # Remove the unreliable 1 year at the edges of the time series
    RFSlice = RFSlice[RFSlice$timestamp.x > as.Date("2014-03-18") &
                      RFSlice$timestamp.x < as.Date("2020-07-14"),]
    
    # If too cloudy, return no prediction
    if (nrow(RFSlice) < h*2)
    {
        if (fallback) return(RFSlice)
        return(NULL)
    }
    #{
    #    RFSlice[,GetCommonClassNames()] = NA
    #    return(RFSlice)
    #}
    
    PointTS = as.zooreg(zoo(RFSlice[, GetCommonClassNames()],
                            RFSlice[, "timestamp.x"]))
    Result = PointTS
    for (classidx in 1:length(GetCommonClassNames()))
    {
        InData = ts(as.ts(PointTS[,classidx]), start=c(2014, 5), frequency = 365.25/16)
        # Remove the unreliable start and end year
        #InData[1:23] = NA
        #InData[(length(InData)-23):length(InData)] = NA
        # Run model
        bfl = try(bfastlite(InData, h=h, formula=formula, ...))
        if (class(bfl) != "bfastlite") { # If it crashed, don't return any prediction
            if (fallback) return(RFSlice)
            return(NULL)
        }
        # Filter out breaks by magnitude (<20)
        BreakNo = length(bfl$breakpoints$breakpoints)
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
        if (plot) plot(bfl, main=paste(location_id, GetCommonClassNames()[classidx]), sub=paste(round(Magnitude), collapse = ", "), breaks=BreakNo)
    }
    coredata(Result) = as.matrix(ScalePredictions(coredata(Result)))
    RFSlice[,GetCommonClassNames()] = as.data.frame(Result)
    return(RFSlice)
}

#system.time(FitBFL(2808764))*length(unique(RFPredictions$location_id)) # Single-threaded would take 1.5 hours
#FitBFL(sample(RFPredictions$location_id, 1), plot=TRUE) # Takes 15 minutes

# Run the function over all locations and save the result
BFResult = pblapply(unique(RFPredictions$location_id), FitBFL, scaled=TRUE, mag_threshold=30, formula=response~trend+harmon, order=2, fallback=TRUE, h=12, cl=10)
Result = do.call(rbind, BFResult)
saveRDS(Result, OutFile)

if (FALSE)
{
# 2808764 - deforestation
# 2996157 - reforestation?
# 2129065 - grass to bare, but reference says no change grass
plot(tree ~ as.Date(paste0(dataYear, "-01-01")), Reference[Reference$location_id == 2808764,], ylim=c(0,100), xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")), type="l", main=2808764)
for (classidx in 2:length(GetCommonClassNames()))
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ as.Date(paste0(dataYear, '-01-01'))")), Reference[Reference$location_id == 2808764,], col=classidx+1)
for (classidx in 1:length(GetCommonClassNames()))
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ timestamp.x")), ScaledPredictions[ScaledPredictions$location_id == 2808764,], col=classidx)
legend("top", legend=GetCommonClassNames(), fill=1:length(GetCommonClassNames()))

}
