# Transition matrices between years
library(SCM)
library(pbapply)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)

source("../utils/covariate-names.r")
source("../utils/utils.r")

# A transition matrix is an SCM that goes from one year to another
# It can be calculated on both reference and predictions

# Load predictions and validations (yearly, i.e. -val.csv files)
# Run SCM on both predictions and validations for the total duraction (2015-2019)

InputDir = "../../data/predictions" # Where to look for predictions
InputPostfix = "-val.csv"

StartYear = 2016 # Note: DW does not have too much data in 2015, so 2016 may be a better option
EndYear = 2019

InputPattern = glob2rx(paste0("*", InputPostfix))
InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

RFFile = grep("summer/mean_predictions_scaled-val.csv", InputFiles, value=TRUE)
BFASTFile = grep("summer-bfl-m30-trend-scaled-h016-fb", InputFiles, value=TRUE)
DWFile = grep("dynamicworld-val.csv", InputFiles, value=TRUE)
DWBFFile = grep("dynamicworld-bfl-m30-trend-h016", InputFiles, value=TRUE)

GetTransitionMatrices = function(InputFile, interactive=FALSE, diff=FALSE, relative=FALSE)
{
    OutputFileRoot = sub("-val\\.csv$", paste("-transition", StartYear, EndYear, sep="_"), InputFile)
    InputData = read.csv(InputFile)
    # Filter to only 2015 and only 2019
    DataStart = InputData[InputData$dataYear == StartYear,]
    DataEnd = InputData[InputData$dataYear == EndYear,]

    # Make sure these are aligned to each other
    CommonIDs = intersect(DataStart$location_id, DataEnd$location_id)
    DataStart = DataStart[DataStart$location_id %in% CommonIDs,]
    DataEnd = DataEnd[DataEnd$location_id %in% CommonIDs,]

    stopifnot(all(DataStart[["location_id"]] == DataEnd[["location_id"]]))

    # Calculate change matrix, predicted = start, observed = end
    PredNames = paste0(GetCommonClassNames(), ".x")
    ValNames = paste0(GetCommonClassNames(), ".y")
    # Scale predictions to 100% (needed for the SCM to work)
    PredictionStart = ScalePredictions(DataStart[,PredNames], FALSE)/100
    PredictionEnd = ScalePredictions(DataEnd[,PredNames], FALSE)/100
    ValStart = ScalePredictions(DataStart[,ValNames], FALSE)/100
    ValEnd = ScalePredictions(DataEnd[,ValNames], FALSE)/100

    # Get SCMs
    PROD = function(s, r, k, l) s[k]*r[l] # Non-NaN version of PROD_D

    PredMat = SCM(PredictionStart, PredictionEnd)
    #SCM(PredictionStart, PredictionEnd, agreement=NULL, disagreement=PROD, plot=TRUE)

    ValMat = SCM(ValStart, ValEnd)
    #SCM(ValStart, ValEnd, agreement=NULL, disagreement=PROD, plot=TRUE)

    DiffMat = PredMat[["P"]] - ValMat[["P"]]
    
    # Save them
    if (!interactive)
    {
        write.csv(PredMat[["P"]], paste0(OutputFileRoot, "-prediction.csv"))
        write.csv(ValMat[["P"]], paste0(OutputFileRoot, "-reference.csv"))
    }

    # Plot side by side

    myPalette = colorRampPalette(rev(brewer.pal(11, "Spectral")))
    sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0, if (relative) 1.5 else 400), oob=scales::squish)
    scDiff <- scale_fill_gradientn(colours = myPalette(100), limits=if (relative) c(-1.5, 1.5) else c(-300, 300), oob=scales::squish)

    AxisNames = c(paste("From", StartYear), paste("To", EndYear))
    
    if (relative) {
        PredMat[["P"]] = PredMat[["P"]] / sum(PredMat[["P"]]) * 100
        ValMat[["P"]] = ValMat[["P"]] / sum(ValMat[["P"]]) * 100
        DiffMat = DiffMat / sum(DiffMat) * 100
    }
    
    LegendName = if (relative) "Area (%)" else "Fraction sum"
    
    PredDF = melt(PredMat[["P"]], value.name=LegendName, varnames=AxisNames)
    ValDF = melt(ValMat[["P"]], value.name=LegendName, varnames=AxisNames)
    DiffDF = melt(DiffMat, value.name=LegendName, varnames=AxisNames)
    
    for (Axis in AxisNames)
    {
        levels(PredDF[[Axis]]) = PrettifyNames(levels(PredDF[[Axis]]))
        levels(ValDF[[Axis]]) = PrettifyNames(levels(ValDF[[Axis]]))
        levels(DiffDF[[Axis]]) = PrettifyNames(levels(DiffDF[[Axis]]))
    }

    PredPlot = ggplot(PredDF, aes_string(paste0("`To ", EndYear, "`"), paste0("`From ", StartYear, "`"), fill=paste0("`", LegendName, "`"))) +
        geom_tile() + sc + geom_text(aes(label=if (relative) round(`Area (%)`, 2) else round(`Fraction sum`))) + ggtitle("Prediction")
    RefPlot  = ggplot(ValDF, aes_string(paste0("`To ", EndYear, "`"), paste0("`From ", StartYear, "`"), fill=paste0("`", LegendName, "`"))) +
        geom_tile() + sc + geom_text(aes(label=if (relative) round(`Area (%)`, 2) else round(`Fraction sum`))) + ggtitle("Reference")
    DiffPlot  = ggplot(DiffDF, aes_string(paste0("`To ", EndYear, "`"), paste0("`From ", StartYear, "`"), fill=paste0("`", LegendName, "`"))) +
        geom_tile() + scDiff + geom_text(aes(label=if (relative) round(`Area (%)`, 2) else round(`Fraction sum`))) + ggtitle("Error")
    if (!interactive)
    {
        # grid.arrange(PredPlot, RefPlot) # To visualise it
        ggsave(paste0(OutputFileRoot, ".pdf"), arrangeGrob(PredPlot, RefPlot), width=7.85, height=4.14)
        ggsave(paste0(OutputFileRoot, ".png"), arrangeGrob(PredPlot, RefPlot), width=7.85, height=4.14)
        return(OutputFileRoot)
    } else {
        if (diff) return(DiffPlot)
        return(list(PredPlot, RefPlot))
    }
}

## Create matrices for every measurement
#pblapply(InputFiles, GetTransitionMatrices)

## Create absolute matrix

RFPlot = GetTransitionMatrices(RFFile, TRUE)
#ggsave("../../output/2023-05-26-rf-transition-matrix.pdf")
BFPlot = GetTransitionMatrices(BFASTFile, TRUE)
#ggsave("../../output/2023-05-26-bf-transition-matrix.pdf")

Legend = get_legend(RFPlot[[1]])
ggdraw(plot_grid(plot_grid(RFPlot[[2]] + theme(legend.position='none'), RFPlot[[1]] + theme(legend.position='none') + ggtitle("Random Forest regression"), ncol=1),
                 plot_grid(Legend, BFPlot[[1]] + theme(legend.position='none') + ggtitle("Random Forest + BFAST Lite"), ncol=1)))
ggsave("../../output/2023-08-04-change-matrices.pdf", width=11, height=4.2)


## Create relative matrix

RFPlot = GetTransitionMatrices(RFFile, TRUE, relative=TRUE)
BFPlot = GetTransitionMatrices(BFASTFile, TRUE, relative=TRUE)

Legend = get_legend(RFPlot[[1]])
ggdraw(plot_grid(plot_grid(RFPlot[[2]] + theme(legend.position='none'), RFPlot[[1]] + theme(legend.position='none') + ggtitle("Random Forest regression"), ncol=1),
                 plot_grid(Legend, BFPlot[[1]] + theme(legend.position='none') + ggtitle("Random Forest + BFAST Lite"), ncol=1)))
ggsave("../../output/2023-08-04-change-matrices-relative.pdf", width=11, height=4.2)

DWPlot = GetTransitionMatrices(DWFile, TRUE)
DWBFPlot = GetTransitionMatrices(DWBFFile, TRUE)

