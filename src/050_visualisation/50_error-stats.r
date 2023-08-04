# Combine change and trends into one figure

library(reshape2)
library(ggplot2)
library(grid)
source("utils.r")

## Static map accuracy

InputDir = "../../data/predictions"
InputPattern = glob2rx("*-static-stats*.csv")
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

ModelMap = c(`dynamicworld/dynamicworld-static-stats.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-static-stats.csv` = "Dynamic World + LOESS",
             `dynamicworld-bfl-m30-trend-h016/predictions-static-stats.csv` = "Dynamic World + BFAST Lite",# (h=0.16, mag>30, trend)",
             `summer/mean_predictions-static-stats.csv` = "Random Forest regression",
             `summer-loess/mean_predictions-static-stats.csv` = "Random Forest + LOESS",
             `summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-static-stats.csv` = "Random Forest + BFAST Lite",# (h=0.16, mag>30, trend)",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-static-stats.csv` = "RF + NDVI-only BFAST Lite",
             `summer-beast/mean-predictions-static-stats.csv` = "Random Forest + BEAST"
             )

MapError = VisualiseModelStats(InputFiles=InputFiles, ModelMap = ModelMap, main="", Statistics=c("MAE", "ME"))

## Change accuracy

InputDir = "../../data/predictions"
InputPattern = glob2rx("*-change_stats_scaled.csv")
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

ModelMap = c(`dynamicworld/dynamicworld-change_stats_scaled.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-change_stats_scaled.csv` = "Dynamic World + LOESS",
             `dynamicworld-bfl-m30-trend-h016/predictions-change_stats_scaled.csv` = "Dynamic World + BFAST Lite",# (h=0.16, mag>30, trend)",
             `summer/mean_predictions-change_stats_scaled.csv` = "Random Forest regression",
             `summer-loess/mean_predictions-change_stats_scaled.csv` = "Random Forest + LOESS",
             `summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-change_stats_scaled.csv` = "Random Forest + BFAST Lite", #(h=0.16, mag>30, trend)",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-change_stats_scaled.csv` = "RF + NDVI-only BFAST Lite",
             `summer-beast/mean-predictions-change_stats_scaled.csv` = "Random Forest + BEAST"
             )

ChangeError = VisualiseModelStats(InputDir, InputPattern, ModelMap = ModelMap, main="",  Statistics=c("MAE", "ME"))

## Trend error

# What to plot
InputDir = "../../data/predictions"
InputPattern = glob2rx("*-trend-stats-yearly-ChangeTrend.csv")

# What models we already have
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

# What models to visualise
ModelMap = c(`dynamicworld/dynamicworld-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + LOESS",
             `dynamicworld-bfl-m30-trend-h016/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFAST Lite",
             `summer/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest regression",
             `summer-loess/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest + LOESS",
             `summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest + BFAST Lite",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "RF + NDVI-only BFAST Lite",
             `summer-beast/mean-predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest + BEAST"
             )

TrendError = VisualiseModelStats(InputFiles=InputFiles, ModelMap=ModelMap, main="", RemovePattern="ChangeTrend\\.",  Statistics=c("MAE", "ME"))

## Variability error

InputPattern = glob2rx("*-trend-stats-yearly-ChangeRMSD.csv")

# What models we already have
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

# What models to visualise
ModelMap = c(`dynamicworld/dynamicworld-trend-stats-yearly-ChangeRMSD.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-trend-stats-yearly-ChangeRMSD.csv` = "Dynamic World + LOESS",
             `dynamicworld-bfl-m30-trend-h016/predictions-trend-stats-yearly-ChangeRMSD.csv` = "Dynamic World + BFAST Lite",
             `summer/mean_predictions-trend-stats-yearly-ChangeRMSD.csv` = "Random Forest regression",
             `summer-loess/mean_predictions-trend-stats-yearly-ChangeRMSD.csv` = "Random Forest + LOESS",
             `summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trend-stats-yearly-ChangeRMSD.csv` = "Random Forest + BFAST Lite",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-trend-stats-yearly-ChangeRMSD.csv` = "RF + NDVI-only BFAST Lite",
             `summer-beast/mean-predictions-trend-stats-yearly-ChangeRMSD.csv` = "Random Forest + BEAST"
             )

VariabilityError = VisualiseModelStats(InputFiles=InputFiles, ModelMap=ModelMap, main="", RemovePattern="ChangeRMSD\\.", Statistics=c("MAE", "ME"))

## Combine into one

pdf("../../output/2023-07-27-erors.pdf", 12.8, 9.06)
grid.newpage()
Combination = grid.draw(rbind(ggplotGrob(MapError), ggplotGrob(ChangeError), ggplotGrob(TrendError), ggplotGrob(VariabilityError), size = "max"))
dev.off()
