library(reshape2)
library(ggplot2)
# Visualise change statistics
# As per Rob, overall + per class

InputDir = "../../data/predictions"
InputFiles = list.files(InputDir, pattern=glob2rx("*-change_stats.csv"), recursive = TRUE, full.names = TRUE)

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
 "summer mean_predictions_scaled-change_stats.csv",
 "summer-window median_predictions-change_stats.csv",
 "summer-bfbaseline-m02-harmon-scaled mean_predictions-change_stats.csv",
# "summer-bfl-m20-harmon-scaled mean_predictions-change_stats.csv",
 "summer-bfl-m30-harmon2-scaled-h12-fb mean_predictions-change_stats.csv"
 ),]
StatTable$model = factor(StatTable$model, labels=c(
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

# Everything
ggplot(PlotTable, aes(x=X, y=value, fill=model)) +
    geom_bar(stat="identity", position="dodge") +
    facet_wrap(~Statistic) +
    ggtitle("Per-class pooled yearly change")
ggsave("2022-07-27-change-per-class.pdf")
    
ggplot(OverallTable, aes(x=Statistic, y=value, fill=model)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(label=round(OverallTable$value, 2), position=position_dodge(width = .9), vjust = 0) +
    ggtitle("Overall pooled yearly change")

ggsave("2022-07-27-change-statistics.pdf")

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
 "summer-bfl-m30-harmon2-scaled-h12-fb mean_predictions"
 ),]
StatTable$model = factor(StatTable$model, labels=c(
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
