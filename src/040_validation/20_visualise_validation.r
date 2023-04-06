# Analyse and visualise validation statistics
library(reshape2)
library(ggplot2)

InputDir = "../../data/predictions"
InputFiles = list.files(InputDir, pattern=glob2rx("*.csv"), recursive = TRUE, full.names = TRUE)
# Remove -val and -change files
InputFiles = grep("(-val\\.csv|-change_.*\\.csv)", InputFiles, value=TRUE, invert=TRUE)

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
 "summer mean_predictions_scaled.csv",
 "summer median_predictions.csv",
 "summer-bfbaseline-m02-harmon-scaled mean_predictions.csv",
 "summer-bfbaseline-m02-harmon-scaled median_predictions.csv",
# "summer-bfl-m20-harmon-scaled mean_predictions.csv",
 "summer-bfl-m30-harmon2-scaled-h12-fb mean_predictions.csv",
 "summer-bfl-m30-harmon2-scaled-h12-fb median_predictions.csv",
 "dynamicworld dynamicworld.csv",
 "dynamicworld dynamicworld-m10.csv",
 "dynamicworld-bfl predictions.csv"
 ),]
# Should be ordered by levels(factor(StatTable$model))
StatTable$model = factor(StatTable$model, labels=c(
    "Dynamic World (threshold 10)",
    "Dynamic World",
    "Dynamic World + BFAST Lite (defaults)",
    "Dense RF regression (mean)",
    "Dense RF regression (median)",
    "Baseline yearly RF model (mean)",
    "Baseline yearly RF model (median)",
    "BFAST Lite model (mean)",
    "BFAST Lite model (median)"
    ))
#StatTable$model = ordered(StatTable$model, levels=unique(StatTable$model)[c(1,4,2,3)])
# Take only RMSE, MAE, ME
PlotTable = StatTable[!StatTable$Statistic %in% c("RMSEAdj", "ME"),]
# Take only the overall accuracy
OverallTable = PlotTable[PlotTable$X == "Overall",]

# Everything
#ggplot(PlotTable, aes(x=X, y=value, fill=Statistic)) +
#    geom_bar(stat="identity", position="dodge") +
#    facet_wrap(~model)

# Only overall
ggplot(OverallTable, aes(x=Statistic, y=value, fill=model)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(label=round(OverallTable$value, 2), position=position_dodge(width = .9), vjust = 0)

ggsave("2023-03-06-statistics.pdf")

# Scaled is always better, harmon2 is always better, m20 is always better
# fallback is always better
