# Analyse methods based on the correspondence in trends between years
# Calculate a 4-year trend and RMSD (and MAD?) from the reference,
# then compre it with the trend and RMSD from the predictions.
# 
# Also can calculate the total amount of change between years regardless of class.

library(pbapply)

source("../utils/load-sampling-data.r")
source("../utils/covariate-names.r")
source("../utils/utils.r")
source("../utils/accuracy-statistics.r")

ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
InputDir = "../../data/predictions" # Where to look for predictions

ReferenceTrendOutput = "../../data/reference-trends.csv"

ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)

# If you don't want to aggregate to yearly:
#InputPostfix = ".rds"
# If you do:
InputPostfix = "-val.csv"

InputPattern = glob2rx(paste0("*", InputPostfix))
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

# Calculate multi-year trends and add their statistics as new columns to the df
# df can be either reference or prediction chunk of a particular sample_id
# e.g. GetTrend(ReferenceData[ReferenceData$location_id == 2996157,])
GetTrend = function(df)
{
    # Figure out whether we have timestamp.x (predictions) or dataYear (reference)
    Time = if (!is.null(df$timestamp.x)) as.Date(df[["timestamp.x"]]) else YearToTOSDate(df[["dataYear"]], df[["subpix_mean_y"]])
    # If we have "class.x", then rename it to "class"
    Classes.x = paste0(GetCommonClassNames(), ".x")
    names(df)[names(df) %in% Classes.x] = substr(names(df)[names(df) %in% Classes.x], 1, nchar(names(df)[names(df) %in% Classes.x])-2)
    Stats = sapply(df[GetCommonClassNames()], CalcTrend, Time)
    # Calculate overall metrics: sum of absolute numbers
    Stats = cbind(Stats, Overall = rowSums(abs(Stats)))
    # Flatten everything to one row
    melted = melt(Stats)
    FlattenedStats = melted[["value"]]
    names(FlattenedStats) = paste(melted[[1]], melted[[2]], sep=".")
    # Append the location id (not year or other data, because we have aggregated values here)
    Result = cbind(df[1,c("location_id"),drop=FALSE], t(FlattenedStats))
    return(Result)
}

# Calculate multi-year trends from a vector of values.
# fraction is a numeric vector
# e.g. CalcTrend(1:10, 2011:2020)
CalcTrend = function(Fraction, Time)
{
    Fraction = na.omit(Fraction)
    # Is the sd zero (i.d. no change) or not enough data? Then don't bother and return zero trend
    if (length(Fraction) < 2 || sd(Fraction) < 0.0001)
        return(c(ChangeTrend=0, ChangeTotal=0, ChangeRMSD=0, ChangeMAD=0))
    # Otherwise, make an lm and get the coefficients
    model = lm(Fraction ~ Time)
    Trend = coef(model)[2]
    names(Trend) = NULL
    # Get the sum of changes
    ChangeTotal = sum(abs(Fraction[-1] - Fraction[-length(Fraction)]))
    # Get RMSD and MAD
    Stats = AccuracyStats(fitted(model), Fraction)
    return(c(ChangeTrend=Trend, ChangeTotal=ChangeTotal, ChangeRMSD = Stats[["RMSE"]], ChangeMAD = Stats[["MAE"]]))
}

# Utility to convert year to top-of-season date
# year is integer
# y is numeric coordinate in WGS84
# e.g. YearToTOSDate(c(2015, 2016), c(-1, -1))
YearToTOSDate = function(year, y)
{
    # South hemisphere: return January 1
    if (any(y < 0)) return(as.Date(paste0(year, "-01-01")))
    # North hemisphere: return July 1
    return(as.Date(paste0(year, "-07-01")))
}

if (!file.exists(ReferenceTrendOutput))
{
    # Get trends for the validation
    ValTrends = pbby(ReferenceData, as.factor(ReferenceData[["location_id"]]), GetTrend)
    ValTrends = do.call(rbind, ValTrends)

    # Make sure the result is already sorted
    stopifnot(all(ValTrends[["location_id"]] == sort(ValTrends[["location_id"]])))

    # Save
    write.csv(ValTrends, ReferenceTrendOutput)

    # Explore data
    opar = par(no.readonly=TRUE)
    par(mfrow=c(4,8))
    for (var in names(ValTrends)[-1])
    {
        NoZero = ValTrends[[var]]
        NoZero[NoZero == 0] = NA
        hist(NoZero, main=var)
    }
    par(opar)
} else ValTrends = read.csv(ReferenceTrendOutput, row.names="X")

# Iterate over all predictions and save the results as CSVs
GetPredictionTrend = function(InputFile)
{
    OutFile = sub(InputPostfix, "-trends-yearly.csv", InputFile)
    if (file.exists(OutFile))
        return(OutFile)
    
    Prediction = if (InputPostfix == ".rds")
        readRDS(InputFile) else read.csv(InputFile)
    
    Results = by(Prediction, as.factor(Prediction[["location_id"]]), GetTrend)
    Results = do.call(rbind, Results)
    write.csv(Results, OutFile, row.names = FALSE)
    return(OutFile)
}

PredTrends = pblapply(InputFiles, GetPredictionTrend, cl=12)

## Check the correspondence between trends of reference and predictions
GetTrendStats = function(PredFile)
{
    OutFile = sub("\\-trends-yearly\\.csv$", "-trend-stats-yearly.csv", PredFile)
    OutFileRel = sub("\\-trends-yearly\\.csv$", "-trend-stats-yearly-rel.csv", PredFile)
    if (file.exists(OutFile) && file.exists(OutFileRel))
        return(OutFile)
    
    PredTrend = read.csv(PredFile)
    # Match IDs
    CommonIDs = intersect(PredTrend[["location_id"]], ValTrends[["location_id"]])
    PredTrend = PredTrend[PredTrend$location_id %in% CommonIDs,]
    ValTrend = ValTrends[ValTrends$location_id %in% CommonIDs,]
    stopifnot(all(PredTrend[["location_id"]] == ValTrend[["location_id"]]))

    TrendStats = AccuracyStatTable(PredTrend[-1], ValTrend[-1])
    write.csv(TrendStats, OutFile)
    TrendStatsRel = AccuracyStatTable(PredTrend[-1], ValTrend[-1], relative=TRUE)
    write.csv(TrendStatsRel, OutFileRel)
    return(OutFile)
}

PredStats = pblapply(PredTrends, GetTrendStats, cl=12)

if (FALSE) {
    # When reding the results, separate stats per item because of different magnitudes:
    AccuracyStatisticsPlots(PredTrend[grep("Trend", names(PredTrend))], ValTrend[grep("Trend", names(ValTrend))])
    AccuracyStatisticsPlots(PredTrend[grep("Total", names(PredTrend))], ValTrend[grep("Total", names(ValTrend))])
    AccuracyStatisticsPlots(PredTrend[grep("RMSD", names(PredTrend))], ValTrend[grep("RMSD", names(ValTrend))])
    AccuracyStatisticsPlots(PredTrend[grep("MAD", names(PredTrend))], ValTrend[grep("MAD", names(ValTrend))])
}
