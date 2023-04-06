source("utils.r")

# What to plot
InputDir = "../../data/predictions"
InputPattern = glob2rx("*-trend-stats.csv")

# What models we already have
list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

# What models to visualise
ModelMap = c(`dynamicworld/dynamicworld-trend-stats.csv` = "Dynamic World",
             `dynamicworld-loess/predictions-trend-stats.csv` = "Dynamic World + LOESS",
             `dynamicworld-loess-s05/predictions-trend-stats.csv` = "Dynamic World + LOESS (a=0.5)",
             #`dynamicworld/dynamicworld-m10-trend-stats.csv` = "Dynamic World (10% threshold)",
             #`dynamicworld-bfl/predictions-trend-stats.csv` = "Dynamic World + BFL (h=72)",
             `dynamicworld-bfl-h016/predictions-trend-stats.csv` = "Dynamic World + BFL (h=0.16)",
             `dynamicworld-bfl-harmon1-h016/predictions-trend-stats.csv` = "Dynamic World + BFL (h=0.16, order=1)",
             `dynamicworld-bfl-harmon3-h016/predictions-trend-stats.csv` = "Dynamic World + BFL (h=0.16, order=3)",
             `dynamicworld-bfl-m30-harmon2-h016/predictions-trend-stats.csv` = "Dynamic World + BFL (h=0.16, mag>30)",
             `dynamicworld-bfl-m30-trend-h016/predictions-trend-stats.csv` = "Dynamic World + BFL (h=0.16, mag>30, trend)",
             `summer/mean_predictions-trend-stats.csv` = "Random Forest mean vote",
             #`summer/mean_predictions_scaled-trend-stats.csv` = "Random Forest mean vote (scaled)",
             #`summer/median_predictions-trend-stats.csv` = "Random Forest median vote",
             `summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-trend-stats.csv` = "RF mean + BFL (h=12)",
             `summer-bfl-m30-harmon2-scaled-h016-fb/mean-predictions-trend-stats.csv` = "RF mean + BFL (h=0.16)",
             #`summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions-trend-stats.csv` = "RF median + BFL (h=12)",
             `summer-bfbaseline-m02-harmon-scaled/mean_predictions-trend-stats.csv` = "RF mean + NDVI BFL"
             #`summer-bfbaseline-m02-harmon-scaled/median_predictions-trend-stats.csv` = "RF median + NDVI BFL"
             )

VisualiseModelStats(InputDir, InputPattern, ModelMap, main="Trends", VariablePattern=glob2rx("ChangeTrend.*"), filename="../../output/2023-03-24-trend-stats.png")
VisualiseModelStats(InputDir, InputPattern, ModelMap, main="Total amount of change", VariablePattern=glob2rx("ChangeTotal.*"), filename="../../output/2023-03-24-total-change-stats.png")
VisualiseModelStats(InputDir, InputPattern, ModelMap, main="Variance (RMSD)", VariablePattern=glob2rx("ChangeRMSD.*"), filename="../../output/2023-03-24-variance-stats.png")
VisualiseModelStats(InputDir, InputPattern, ModelMap, main="Variance (MAD)", VariablePattern=glob2rx("ChangeMAD.*"))
