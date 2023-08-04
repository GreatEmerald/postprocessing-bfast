# Calculate statistics from an SCM for a given model
# Also get a corrplot while we're at it
library(SCM)
library(corrplot)

source("../utils/accuracy-statistics.r")
source("../utils/covariate-names.r")

# Model data to validate
InputFile = "../../data/predictions/summer/mean_predictions_scaled-val.csv"

# Calculate SCM statistics and/or correlation plot.
# InputFile: filename of a -val.csv.
# GetSCM: whether to calculate SCM. Set to FALSE for change.
GetStats = function(InputFile, GetSCM=TRUE)
{
    DataTable = read.csv(InputFile)
    Predictions = DataTable[paste0(GetCommonClassNames(), ".x")] / 100
    names(Predictions) = GetCommonClassNames()
    Observations = DataTable[paste0(GetCommonClassNames(), ".y")] / 100
    names(Observations) = GetCommonClassNames()
    Stats = AccuracyStatisticsPlots(Predictions, Observations)
    if (GetSCM)
    {
        ConMat = SCM(Predictions, Observations, totals=TRUE, plot=TRUE)
        return(ConMat)
    }
    return(Stats)
}

RFStats = GetStats("../../data/predictions/summer/mean_predictions_scaled-val.csv")
RFChangeStats = GetStats("../../data/predictions/summer/mean_predictions-change_val_scaled.csv", FALSE)

BFLStats = GetStats("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-val.csv")
BFLChangeStats = GetStats("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-change_val_scaled.csv", FALSE)

DWStats = GetStats("../../data/predictions/dynamicworld/dynamicworld-val.csv")
DWChangeStats = GetStats("../../data/predictions/dynamicworld/dynamicworld-change_val_scaled.csv", FALSE)

DWStats = GetStats("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-val.csv")
DWChangeStats = GetStats("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-change_val_scaled.csv", FALSE)

