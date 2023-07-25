# Postprocess RF predictions using BFAST
# This is our new method in which we try to make use of BFAST Lite models to better predict LC change

# Input: RF fraction predictions on validation data
# Output: The same schema, but the predicions are altered based on the BFAST Lite model
library(sf)
library(zoo)
library(Rbeast)
library(pbapply)

source("../utils/utils.r")
source("../utils/covariate-names.r")
source("../utils/load-sampling-data.r")

# Read in reference and predictions
#RFFile = "../../data/predictions/dynamicworld/dynamicworld.rds"
RFFile = "../../data/predictions/summer/mean_predictions_scaled.rds"
#ReferenceFile = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
OutFile = "../../data/predictions/summer-beast/mean-predictions.rds"
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

# Function for taking a data.frame of a location and producing a Beast model fit as an output
# location_id is an integer of the location ID of interest
# ... are beast() parameters
# fallback=TRUE means that on error, we return unchanged data. Else, we return NA
# For Landsat 8 data, use start=c(2014, 5), frequency = 365.25/16
# For Dynamic World, use start=c(2014, 15), frequency=365.25/5
# Note that the start will not always be correct due to missing data at the beginning, but it does not matter
FitBeast = function(location_id, scaled=TRUE, plot=FALSE, fallback=FALSE,
    start=c(2014, 5), frequency = 365.25/16,
    ...)
{
    RFSlice = if (scaled) ScaledPredictions[ScaledPredictions$location_id == location_id, ] else
        RFPredictions[RFPredictions$location_id == location_id, ]
    # Remove the unreliable 1 year at the edges of the time series
    RFSlice = RFSlice[RFSlice$timestamp.x > as.Date("2014-03-18") &
                      RFSlice$timestamp.x < as.Date("2020-07-14"),]
    
    
    PointTS = as.zooreg(zoo(RFSlice[, GetCommonClassNames()],
                            RFSlice[, "timestamp.x"]))
    Result = PointTS
    for (classidx in 1:length(GetCommonClassNames()))
    {
        InData = ts(as.ts(PointTS[,classidx]), start=start, frequency = frequency)
        # Remove the unreliable start and end year
        #InData[1:23] = NA
        #InData[(length(InData)-23):length(InData)] = NA
        # Run model
        beastpred = try(beast(InData))
        if (class(beastpred) != "beast" || is.null(beastpred$trend$Y)) { # If it crashed, don't return any prediction
            if (fallback) return(RFSlice)
            return(NULL)
        }
        beastfit = if(!is.null(beastpred$season$Y)) beastpred$trend$Y + beastpred$season$Y else beastpred$trend$Y
        
        if (plot) plot(beastpred$time, beastfit, main=paste(location_id, GetCommonClassNames()[classidx]), type="l")
        
        # Remove predictions at NA locations
        beastfit[is.na(InData)] = NA
        beastfit = na.omit(beastfit) # TODO: fix case when we have all NAs
        
        Result[,classidx] = beastfit
        #Result[is.na(InData),classidx] = NA
        
    }
    coredata(Result) = as.matrix(ScalePredictions(coredata(Result)))
    RFSlice[,GetCommonClassNames()] = as.data.frame(Result)
    return(RFSlice)
}

#system.time(FitBFL(2808764))*length(unique(RFPredictions$location_id)) # Single-threaded would take 1.5 hours
#FitBFL(sample(RFPredictions$location_id, 1), plot=TRUE) # Takes 15 minutes

# Run the function over all locations and save the result
BFResult = pblapply(unique(RFPredictions$location_id), FitBeast,
    scaled=TRUE, fallback=TRUE, start=c(2014, 5), frequency=365.25/16, cl=10)
warnings()
    
dtypes = sapply(BFResult, function(x)class(x))
errors = which(dtypes == "try-error")
print(BFResult[errors])
if (length(errors) > 0) {
    warning(paste("Errors encountered in", round(length(errors) / length(dtypes) * 100), "% of the cases"))
    BFResult = BFResult[-errors]
}

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
