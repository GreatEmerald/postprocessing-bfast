# Create a map of hexagons showing LC change
# A bit similar to Brown et al. (2022)

library(ggplot2)

source("../utils/load-sampling-data.r")

# Reference CSVs
ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)

PlotTrends = function(CSVFilenames, location_col="location_id", data_col="ChangeTotal.Overall", OutFilename = NULL, width=NA, height=NA, MaxValue = 50000)
{
    CombinedDF = NULL
    for (i in 1:length(CSVFilenames))
    {
        Input = read.csv(CSVFilenames[i])
        Output = merge(Input[c(data_col, location_col)], ReferenceData[c("subpix_mean_x", "subpix_mean_y", "location_id")], by.x=location_col, by.y="location_id")
        Output$Model = names(CSVFilenames)[i]
        CombinedDF = rbind(CombinedDF, Output)
    }
    CombinedDF$Model = ordered(CombinedDF$Model, levels=names(CSVFilenames))
    
    OutPlot = ggplot(CombinedDF, aes(subpix_mean_x, subpix_mean_y)) + stat_summary_hex(bins=c(100, 100),aes(z = .data[[data_col]]),fun = "sum") + facet_wrap(vars(Model), ncol=2) +
    ylim(-90, 90) +
    xlim(-180, 180) +
    theme_void() +
    scale_fill_distiller(palette="Oranges", trans = "reverse", guide=guide_colourbar("Sum of change (%)"), limits=c(MaxValue, 0), oob=scales::squish)
    
    if (!is.null(OutFilename))
        ggsave(OutFilename, OutPlot, width=width, height=height)
        
    return(OutPlot)
}

PlotTrends(c(
    "Random Forest regression" = "../../data/predictions/summer/mean_predictions_scaled-trends-yearly.csv",
    "Random Forest + BFAST Lite"="../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trends-yearly.csv",
    "Dynamic World" = "../../data/predictions/dynamicworld/dynamicworld-trends-yearly.csv",
    "Dynamic World + BFAST Lite" = "../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-trends-yearly.csv",
    Reference="../../data/reference-trends.csv"),
    OutFilename = "../../output/2023-08-21-hexmap.pdf", MaxValue=50000, width=11.5, height=8.44
)

