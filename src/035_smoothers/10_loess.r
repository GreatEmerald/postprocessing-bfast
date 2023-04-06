# Create a LOESS simple smoother postprocessor as a comparison with BFAST

source("../utils/utils.r")
source("../utils/covariate-names.r")
source("../utils/load-sampling-data.r")

# Read in reference and predictions
RFFile = "../../data/predictions/dynamicworld/dynamicworld.rds"
#ReferenceFile = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
OutFile = "../../data/predictions/dynamicworld-loess-s05/predictions.rds"
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

# Function for taking a data.frame of a location and producing a LOESS fit as an output
# location_id is an integer of the location ID of interest
# ... are loess() parameters
# fallback=TRUE means that on error, we return unchanged data. Else, we return NA
FitLOESS = function(location_id, scaled=TRUE, plot=FALSE, fallback=FALSE,
    ...)
{
    RFSlice = if (scaled) ScaledPredictions[ScaledPredictions$location_id == location_id, ] else
        RFPredictions[RFPredictions$location_id == location_id, ]
    # Remove the unreliable 1 year at the edges of the time series
    RFSlice = RFSlice[RFSlice$timestamp.x > as.Date("2014-03-18") &
                      RFSlice$timestamp.x < as.Date("2020-07-14"),]
    
    # Remove any remaining NAs
    RFSlice = RFSlice[complete.cases(RFSlice[c("timestamp.x", GetCommonClassNames())]),]
    
    # If too cloudy, return no prediction
    if (nrow(RFSlice) < 6)
    {
        if (fallback) return(RFSlice)
        return(NULL)
    }
    #{
    #    RFSlice[,GetCommonClassNames()] = NA
    #    return(RFSlice)
    #}
    
    PointTS = RFSlice[, GetCommonClassNames()]
    Timestamps = as.numeric(RFSlice[["timestamp.x"]])
    Result = PointTS
    
    for (classidx in 1:length(GetCommonClassNames()))
    {
        InData = PointTS[[classidx]]
        # Run model
        lres = try(loess(InData~Timestamps, ...))
        if (class(lres) != "loess") { # If it crashed, don't return any prediction
            if (fallback) return(RFSlice)
            return(NULL)
        }
        
        Result[,classidx] = fitted(lres)
        if (plot) {
            plot(RFSlice[["timestamp.x"]], InData, main=paste(location_id, GetCommonClassNames()[classidx]), type="l", ylim=c(0,100))
            lines(RFSlice[["timestamp.x"]], Result[[classidx]], col="red")
        }
    }
    Result = ScalePredictions(Result)
    RFSlice[,GetCommonClassNames()] = Result
    return(RFSlice)
}

# Run the function over all locations and save the result
LOESSResult = pblapply(unique(RFPredictions$location_id), FitLOESS, scaled=TRUE, fallback=TRUE, span=0.5, cl=10)
warnings()

dtypes = sapply(LOESSResult, function(x)class(x))
errors = which(dtypes == "try-error")
print(LOESSResult[errors])
if (length(errors) > 0) {
    warning(paste("Errors encountered in", round(length(errors) / length(dtypes) * 100), "% of the cases"))
    LOESSResult = LOESSResult[-errors]
}

Result = do.call(rbind, LOESSResult)
saveRDS(Result, OutFile)
