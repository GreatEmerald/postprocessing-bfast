source("utils.r")

# What to plot
InputDir = "../../data/predictions"
InputPattern = glob2rx("*-trend-stats-yearly-ChangeTrend.csv")

# What models we already have
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

# What models to visualise
ModelMap = c(`dynamicworld/dynamicworld-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + LOESS",
             #`dynamicworld-loess-s05/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + LOESS (a=0.5)",
             #`dynamicworld/dynamicworld-m10-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World (10% threshold)",
             #`dynamicworld-bfl/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFL (h=72)",
             #`dynamicworld-bfl-h016/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFL (h=0.16)",
             #`dynamicworld-bfl-harmon1-h016/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFL (h=0.16, order=1)",
             #`dynamicworld-bfl-harmon3-h016/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFL (h=0.16, order=3)",
             #`dynamicworld-bfl-m30-harmon2-h016/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFL (h=0.16, mag>30)",
             `dynamicworld-bfl-m30-trend-h016/predictions-trend-stats-yearly-ChangeTrend.csv` = "Dynamic World + BFAST Lite",
             `summer/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest regression",
             #`summer/mean_predictions_scaled-trend-stats-yearly-ChangeTrend.csv` = "Random Forest mean vote (scaled)",
             #`summer/median_predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest median vote",
             `summer-loess/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest + LOESS",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "RF mean + BFL (h=12)",
             #`summer-bfl-m30-harmon2-scaled-h016-fb/mean-predictions-trend-stats-yearly-ChangeTrend.csv` = "RF mean + BFL (h=0.16)",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions-trend-stats-yearly-ChangeTrend.csv` = "RF median + BFL (h=12)",
             `summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest + BFAST Lite",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-trend-stats-yearly-ChangeTrend.csv` = "RF + NDVI-only BFAST Lite",
             `summer-beast/mean-predictions-trend-stats-yearly-ChangeTrend.csv` = "Random Forest + BEAST"
             #`summer-bfbaseline-m02-harmon-scaled/median_predictions-trend-stats-yearly-ChangeTrend.csv` = "RF median + NDVI BFL"
             )

VisualiseModelStats(InputFiles=InputFiles, ModelMap=ModelMap, main="", RemovePattern="ChangeTrend\\.",  Statistics=c("MAE", "ME"), filename="../../output/2023-07-25-trend-stats.pdf") # Trends (aggregated)
#VisualiseModelStats(InputDir, InputPattern, ModelMap=ModelMap, main="Total amount of change (aggregated)", VariablePattern=glob2rx("ChangeTotal.*"), filename="../../output/2023-04-25-total-change-stats.png")


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

VisualiseModelStats(InputFiles=InputFiles, ModelMap=ModelMap, main="", RemovePattern="ChangeRMSD\\.", Statistics=c("MAE", "ME"), filename="../../output/2023-07-25-variance-stats.pdf") # Variance (RMSD, aggregated)
#VisualiseModelStats(InputDir, InputPattern, ModelMap=ModelMap, main="Variance (MAD, aggregated)", VariablePattern=glob2rx("ChangeMAD.*"))

# Relative stats

InputPattern = glob2rx("*-trend-stats-yearly-rel.csv")

# What models we already have
list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

# What models to visualise
ModelMap = c(`dynamicworld/dynamicworld-trend-stats-yearly-rel.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + LOESS",
             #`dynamicworld-loess-s05/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + LOESS (a=0.5)",
             #`dynamicworld/dynamicworld-m10-trend-stats-yearly-rel.csv` = "Dynamic World (10% threshold)",
             #`dynamicworld-bfl/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + BFL (h=72)",
             #`dynamicworld-bfl-h016/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + BFL (h=0.16)",
             #`dynamicworld-bfl-harmon1-h016/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + BFL (h=0.16, order=1)",
             #`dynamicworld-bfl-harmon3-h016/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + BFL (h=0.16, order=3)",
             #`dynamicworld-bfl-m30-harmon2-h016/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + BFL (h=0.16, mag>30)",
             `dynamicworld-bfl-m30-trend-h016/predictions-trend-stats-yearly-rel.csv` = "Dynamic World + BFL (h=0.16, mag>30, trend)",
             `summer/mean_predictions-trend-stats-yearly-rel.csv` = "Random Forest mean vote",
             #`summer/mean_predictions_scaled-trend-stats-yearly-rel.csv` = "Random Forest mean vote (scaled)",
             #`summer/median_predictions-trend-stats-yearly-rel.csv` = "Random Forest median vote",
             `summer-loess/mean_predictions-trend-stats-yearly-rel.csv` = "RF mean + LOESS",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-trend-stats-yearly-rel.csv` = "RF mean + BFL (h=12)",
             #`summer-bfl-m30-harmon2-scaled-h016-fb/mean-predictions-trend-stats-yearly-rel.csv` = "RF mean + BFL (h=0.16)",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions-trend-stats-yearly-rel.csv` = "RF median + BFL (h=12)",
             `summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trend-stats-yearly-rel.csv` = "RF mean + BFL (h=0.16, mag>30, trend)",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-trend-stats-yearly-rel.csv` = "RF mean + NDVI BFL",
             `summer-beast/mean-predictions-trend-stats-yearly-rel.csv` = "RF mean + BEAST"
             #`summer-bfbaseline-m02-harmon-scaled/median_predictions-trend-stats-yearly-rel.csv` = "RF median + NDVI BFL"
             )

VisualiseModelStats(InputDir, InputPattern, ModelMap=ModelMap, main="Trends (aggregated, relative)", VariablePattern=glob2rx("ChangeTrend.*"), filename="../../output/2023-04-26-relative-trend-stats.png")
VisualiseModelStats(InputDir, InputPattern, ModelMap=ModelMap, main="Total amount of change (aggregated, relative)", VariablePattern=glob2rx("ChangeTotal.*"), filename="../../output/2023-04-26-total-change-relative-stats.png")
VisualiseModelStats(InputDir, InputPattern, ModelMap=ModelMap, main="Variance (RMSD, aggregated, relative)", VariablePattern=glob2rx("ChangeRMSD.*"), filename="../../output/2023-04-26-relative-variance-stats.png")
VisualiseModelStats(InputDir, InputPattern, ModelMap=ModelMap, main="Variance (MAD, aggregated, relative)", VariablePattern=glob2rx("ChangeMAD.*"))
