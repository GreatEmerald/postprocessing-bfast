library(reshape2)
library(ggplot2)
source("utils.r")
# Visualise change statistics
# As per Rob, overall + per class

## New method for static map accuracy

InputDir = "../../data/predictions"
InputPattern = glob2rx("*.csv")
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)
# Remove -val and -change files etc.
InputFiles = grep("(-val\\.csv|-change_.*\\.csv|-trend-stats\\.csv|-trends\\.csv)", InputFiles, value=TRUE, invert=TRUE)

ModelMap = c(`dynamicworld/dynamicworld.csv` = "Dynamic World",
             `dynamicworld-loess/predictions.csv` = "Dynamic World + LOESS",
             `dynamicworld-loess-s05/predictions.csv` = "Dynamic World + LOESS (a=0.5)",
             #`dynamicworld/dynamicworld-m10.csv` = "Dynamic World (10% threshold)",
             #`dynamicworld-bfl/predictions.csv` = "Dynamic World + BFL (h=72)",
             `dynamicworld-bfl-h016/predictions.csv` = "Dynamic World + BFL (h=0.16)",
             `dynamicworld-bfl-harmon1-h016/predictions.csv` = "Dynamic World + BFL (h=0.16, order=1)",
             `dynamicworld-bfl-harmon3-h016/predictions.csv` = "Dynamic World + BFL (h=0.16, order=3)",
             `dynamicworld-bfl-m30-harmon2-h016/predictions.csv` = "Dynamic World + BFL (h=0.16, mag>30)",
             `dynamicworld-bfl-m30-trend-h016/predictions.csv` = "Dynamic World + BFL (h=0.16, mag>30, trend)",
             `summer/mean_predictions.csv` = "Random Forest mean vote",
             #`summer/median_predictions.csv` = "Random Forest median vote",
             `summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions.csv` = "RF mean + BFL (h=12)",
             `summer-bfl-m30-harmon2-scaled-h016-fb/mean-predictions.csv` = "RF mean + BFL (h=0.16)",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions.csv` = "RF median + BFL (h=12)",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions.csv` = "RF mean + NDVI BFL"
             #`summer-bfbaseline-m02-harmon-scaled/median_predictions.csv` = "RF median + NDVI BFL"
             )

VisualiseModelStats(InputFiles=InputFiles, ModelMap = ModelMap, main="Static map accuracy", filename="../../output/2023-03-27-accuracy-stats.png")

## New method for change accuracy

InputDir = "../../data/predictions"
InputPattern = glob2rx("*-change_stats_scaled.csv")
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

ModelMap = c(`dynamicworld/dynamicworld-change_stats_scaled.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-change_stats_scaled.csv` = "Dynamic World + LOESS",
             `dynamicworld-loess-s05/predictions-change_stats_scaled.csv` = "Dynamic World + LOESS (a=0.5)",
             #`dynamicworld/dynamicworld-m10-change_stats_scaled.csv` = "Dynamic World (10% threshold)",
             #`dynamicworld-bfl/predictions-change_stats_scaled.csv` = "Dynamic World + BFL (h=72)",
             `dynamicworld-bfl-h016/predictions-change_stats_scaled.csv` = "Dynamic World + BFL (h=0.16)",
             `dynamicworld-bfl-harmon1-h016/predictions-change_stats_scaled.csv` = "Dynamic World + BFL (h=0.16, order=1)",
             `dynamicworld-bfl-harmon3-h016/predictions-change_stats_scaled.csv` = "Dynamic World + BFL (h=0.16, order=3)",
             `dynamicworld-bfl-m30-harmon2-h016/predictions-change_stats_scaled.csv` = "Dynamic World + BFL (h=0.16, mag>30)",
             `dynamicworld-bfl-m30-trend-h016/predictions-change_stats_scaled.csv` = "Dynamic World + BFL (h=0.16, mag>30, trend)",
             `summer/mean_predictions-change_stats_scaled.csv` = "Random Forest mean vote",
             #`summer/median_predictions-change_stats_scaled.csv` = "Random Forest median vote",
             `summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-change_stats_scaled.csv` = "RF mean + BFL (h=12)",
             `summer-bfl-m30-harmon2-scaled-h016-fb/mean-predictions-change_stats_scaled.csv` = "RF mean + BFL (h=0.16)",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions-change_stats_scaled.csv` = "RF median + BFL (h=12)",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-change_stats_scaled.csv` = "RF mean + NDVI BFL"
             #`summer-bfbaseline-m02-harmon-scaled/median_predictions-change_stats_scaled.csv` = "RF median + NDVI BFL"
             )

VisualiseModelStats(InputDir, InputPattern, ModelMap, main="Per-class pooled yearly change", filename="../../output/2023-03-27-change-accuracy-stats.png")

## Old method for change accuracy

InputList = lapply(InputFiles, read.csv)
InputList = lapply(InputList, melt, variable.name = "Statistic")
InputList = lapply(1:length(InputList), function(idx) {
        InputList[[idx]]$model = paste(basename(dirname(InputFiles[idx])), basename(InputFiles[idx]))
        InputList[[idx]]
    })
StatTable = do.call(rbind, InputList)

# Filter table:
# Models of interest
StatTable = StatTable[StatTable$model %in% c(
 "summer mean_predictions_scaled-change_stats_scaled.csv",
 "summer-window median_predictions-change_stats_scaled.csv",
 "summer-bfbaseline-m02-harmon-scaled mean_predictions-change_stats_scaled.csv",
# "summer-bfl-m20-harmon-scaled mean_predictions-change_stats.csv",
 "summer-bfl-m30-harmon2-scaled-h12-fb mean_predictions-change_stats_scaled.csv",
 "dynamicworld dynamicworld-change_stats_scaled.csv",
 "dynamicworld-bfl predictions-change_stats_scaled.csv",
 "dynamicworld dynamicworld-m10-change_stats_scaled.csv"
 ),]
# Should be ordered by levels(factor(StatTable$model))
StatTable$model = factor(StatTable$model, labels=c(
    "Dynamic World",
    "Dynamic World (threshold 10)",
    "Dynamic World + BFAST Lite (defaults)",
    "Dense RF regression (mean)",
    "Baseline yearly RF model",
    "BFAST Lite model",
    "Dense RF regression (median)"
    ))
#StatTable$model = ordered(StatTable$model, levels=unique(StatTable$model)[c(1,5,2,3,4)])
# Take only RMSE, MAE, ME
PlotTable = StatTable[!StatTable$Statistic %in% c("RMSEAdj", "ME"),]
# Take only the overall accuracy
OverallTable = PlotTable[PlotTable$X == "Overall",]

# Everything
ggplot(PlotTable, aes(x=X, y=value, fill=model)) +
    geom_bar(stat="identity", position="dodge") +
    facet_wrap(~Statistic) +
    ggtitle("Per-class pooled yearly change")
 ggsave("2023-03-06-change-per-class.pdf")
    
ggplot(OverallTable, aes(x=Statistic, y=value, fill=model)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(label=round(OverallTable$value, 2), position=position_dodge(width = .9), vjust = 0) +
    ggtitle("Overall pooled yearly change")

ggsave("2023-03-06-change-statistics.pdf")

## RMSE (overall) per year

InputFiles = list.files(InputDir, pattern=glob2rx("*-change_20*.csv"), recursive = TRUE, full.names = TRUE)

InputList = lapply(InputFiles, read.csv)
InputList = lapply(InputList, melt, variable.name = "Statistic")
InputList = lapply(1:length(InputList), function(idx) {
        FileParser = basename(InputFiles[idx])
        FileParser = unlist(strsplit(FileParser, "-change_"))
        FileParser[2] = sub(".csv", "", FileParser[2], fixed=TRUE)
        InputList[[idx]]$model = paste(basename(dirname(InputFiles[idx])), FileParser[1])
        InputList[[idx]]$period = FileParser[2]
        InputList[[idx]]
    })
StatTable = do.call(rbind, InputList)

# Filter table:
# Models of interest
StatTable = StatTable[StatTable$model %in% c(
 "summer mean_predictions_scaled",
 "summer-window median_predictions",
 "summer-bfbaseline-m02-harmon-scaled mean_predictions",
# "summer-bfl-m20-harmon-scaled mean_predictions",
 "summer-bfl-m30-harmon2-scaled-h12-fb mean_predictions",
 "dynamicworld dynamicworld-change_stats_scaled.csv"
 ),]
StatTable$model = factor(StatTable$model, labels=c(
    "Dynamic World",
    "Dense RF regression (mean)",
    "Baseline yearly RF model",
    "BFAST Lite model",
    "Dense RF regression (median)"
    ))
StatTable$model = ordered(StatTable$model, levels=unique(StatTable$model)[c(1,4,2,3)])
# Take only RMSE, MAE, ME
PlotTable = StatTable[!StatTable$Statistic %in% c("RMSEAdj", "ME"),]
# Take only the overall accuracy
OverallTable = PlotTable[PlotTable$X == "Overall",]
OverallTable[["value"]] = round(OverallTable[["value"]], 2)

ggplot(OverallTable, aes(x=period, y=value, fill=model)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(aes(label=value), position=position_dodge(width = .9), vjust = 0) +
    facet_wrap(~Statistic) +
    ggtitle("Overall separate yearly change")
ggsave("2022-07-27-change-per-year.pdf")
