# Recreate the usual way a land cover researcher would use BFAST Lite with their land cover maps
# (i.e. CGLOPS workflow): first classify, then use BFAST Lite,
# and only reclassify if BFAST Lite says that there is a break based on a VI.
# To simulate yearly data, we take the median prediction over a year.

library(sf)
library(zoo)
library(bfast)
library(pbapply)

source("../utils/utils.r")
source("../utils/covariate-names.r")
source("../utils/load-sampling-data.r")

# Read in reference and predictions
RFFile = "../../data/predictions/summer/median_predictions.rds"
NDVIFile = "../../data/wur_validation_features/_NDVI.gpkg"
#ReferenceFile = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
OutFile = "../../data/predictions/summer-bfbaseline-m02-harmon-scaled/median_predictions.rds"
if (!dir.exists(dirname(OutFile)))
    dir.create(dirname(OutFile))

RFPredictions = readRDS(RFFile)
RFPredictions$dataYear = lubridate::year(RFPredictions$timestamp.x)

NDVI = st_read(NDVIFile)

#Reference = read.csv(ReferenceFile)
#Reference = RenameReferenceData(Reference)
#Reference = TidyData(Reference)

# Try to normalise and sort predictions - not sure if it's useful here or harmful, needs testing
RFPredictions = RFPredictions[order(RFPredictions$location_id, RFPredictions$timestamp.x),]

ScaledPredictions = RFPredictions
ScaledPredictions[GetCommonClassNames()] = ScalePredictions(ScaledPredictions[GetCommonClassNames()])

BaselineBFL = function(location_id, scaled=TRUE, mag_threshold=0, plot=FALSE, formula=response~trend, h=23, ...)
{
    RFSlice = if (scaled) ScaledPredictions[ScaledPredictions$location_id == location_id, ] else
        RFPredictions[RFPredictions$location_id == location_id, ]
    # Remove the unreliable 1 year at the edges of the time series
    RFSlice = RFSlice[RFSlice$timestamp.x > as.Date("2014-03-18") &
                          RFSlice$timestamp.x < as.Date("2020-07-14"),]
    
    # If too cloudy, return no prediction
    if (nrow(RFSlice) < h*2)
        return(NULL)
    
    PointTS = as.zooreg(zoo(RFSlice[, GetCommonClassNames()],
                            RFSlice[, "timestamp.x"]))
    NDVITS = SFToZoo(NDVI[NDVI$location_id == location_id,])
    NDVITS = window(NDVITS, start=as.Date("2014-03-18"), end=as.Date("2020-07-14"))
    
    Result = PointTS
    
    InData = ts(as.ts(as.zoo(NDVITS[,1])), start=c(2014, 5), frequency = 365.25/16)
    
    # Run model
    bfl = try(bfastlite(InData, h=h, formula=formula, ...))
    if (class(bfl) != "bfastlite") # If it crashed, don't return any prediction
        return(NULL)
    # Filter out breaks by magnitude
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
    if (plot) plot(bfl, main=location_id, sub=paste(round(Magnitude, 2), collapse = ", "), breaks=BreakNo)
    
    BreakFactors = breakfactor(bfl$breakpoints)
    
    for (class in GetCommonClassNames())
    {
        for (Segment in levels(BreakFactors))
        {
            Result[BreakFactors==Segment, class] = median(Result[BreakFactors==Segment, class])
        }
    }
    coredata(Result) = as.matrix(ScalePredictions(coredata(Result)))
    RFSlice[,GetCommonClassNames()] = as.data.frame(Result)
    return(RFSlice)
}

BFResult = pblapply(unique(RFPredictions$location_id), BaselineBFL, mag_threshold=0.2, scaled=TRUE, formula=response~trend+harmon, cl=10)
Result = do.call(rbind, BFResult)
saveRDS(Result, OutFile)

if (FALSE) {
    
Out = BaselineBFL(2808764, plot=TRUE)
plot(tree ~ timestamp.x, Out, ylim=c(0,100), type="l")
for (classidx in 2:length(GetCommonClassNames()))
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ timestamp.x")), Out, col=classidx)
legend("top", legend=GetCommonClassNames(), fill=1:length(GetCommonClassNames()))

}
