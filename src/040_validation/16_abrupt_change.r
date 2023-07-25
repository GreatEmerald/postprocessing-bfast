# Calculate abrupt change statistics for every model
library(pbapply)

source("../utils/covariate-names.r")
source("../utils/accuracy-statistics.r")

PredictionFilePattern = glob2rx("*-change_val_scaled.csv")
PredictionDir = "../../data/predictions/"

ChangeFiles = list.files(PredictionDir, PredictionFilePattern, recursive=TRUE, full.names = TRUE)

EvaluateGradualChange = function(ChangeFile)
{
    Changes = read.csv(ChangeFile)

    # Filter out no change and 100% change in the validation
    Changes$Change = rowSums(abs(Changes[paste0(GetCommonClassNames(), ".y")]))/2
    Changes = Changes[Changes$Change>99, ]

    #hist(Changes$Change[Changes$Change>99], breaks="Scott")


    ChangeOutFile = sub("-change_val_scaled\\.csv$", "-abrupt_change_stats.csv", ChangeFile)
    ChangeOutFileRel = sub("-change_val_scaled\\.csv$", "-abrupt_change_stats-rel.csv", ChangeFile)
    Truth = Changes[paste0(GetCommonClassNames(),".y")]
    Prediction = Changes[paste0(GetCommonClassNames(),".x")]
    StatTable = AccuracyStatTable(Prediction, Truth)
    write.csv(StatTable, ChangeOutFile)
    StatTableRel = AccuracyStatTable(Prediction, Truth, relative=TRUE)
    write.csv(StatTableRel, ChangeOutFileRel)
    return(ChangeOutFile)
}

pblapply(ChangeFiles, EvaluateGradualChange)
