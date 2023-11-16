# Load the harmonised predictions of two sets, a reference and testee, and test
# whether the testee is significantly different from the reference.
# This is testing the residuals, so we also use the reference data.
#library(lme4)
library(lmerTest)
library(multcomp)
library(future)
plan("multicore")

source("../utils/covariate-names.r")

Classes = GetCommonClassNames()

# Keep only common locations between all entries and produce residual tables
# Input is list of data.frames to preprocess
# YearVar is the year variable, "dataYear" for regular errors and "Years" for change, NULL to copy location_id
# For trends: the reference file is separate, and we need to select a variable to check (VarName.Class)
GetResidualTables = function(InputDFs, YearVar="dataYear", VarName=NULL)
{
    # Create a joint ID for year and location for each DF
    InputDFs = lapply(InputDFs, function(x) {
        if (is.null(VarName))
        {
            stopifnot(YearVar %in% names(x))
            x[["location_year"]] = paste0(x[["location_id"]], x[[YearVar]])
        } else x[["location_year"]] = x[["location_id"]]
        return(x)
    })
    
    # Drop rows that don't match (if any)
    CommonIDs = Reduce(intersect, lapply(InputDFs, function(x) x[["location_year"]]))
    DFSizes = sapply(InputDFs, nrow)
    #print(c("DF sizes before:", DFSizes))
    FilteredDFs = lapply(InputDFs, function(x) x[x$location_year %in% CommonIDs,])
    
    # Make sure the tables align
    locations = lapply(FilteredDFs, function(x) x[["location_year"]])
    stopifnot(all(duplicated(locations)[-1]))
    
    DFSizes2 = sapply(FilteredDFs, nrow)
    #print(c("DF sizes after:", DFSizes2))
    print(c("DF size change (check if not 0):", DFSizes2-DFSizes))
    
    if (!is.null(VarName))
    {
        ReferenceTable = read.csv("../../data/reference-trends.csv")
        ReferenceTable = ReferenceTable[ReferenceTable$location_id %in% CommonIDs,]
    }
        
    # Calculate residuals and keep location_year (as factor)
    OutputDFs = lapply(FilteredDFs, function(x) {
        if (!is.null(VarName)) # Trends
        {
            Prediction = x[paste0(VarName, ".", Classes)]
            Reference = ReferenceTable[paste0(VarName, ".", Classes)]
        } else { # Map and static errors
            Prediction = x[paste0(Classes,".x")]
            Reference = x[paste0(Classes,".y")]
        }
        cbind(
            abs(Prediction - Reference),
            location_year = as.factor(x[["location_year"]])
        )
    })
    names(OutputDFs) = names(InputDFs)
    
    return(OutputDFs)
}

# Diagnostics - skip in production
if (FALSE) {
    hist(unlist(ReferenceResiduals), breaks="Scott") # Strange distribution, because it's very unlikely to have negative residuals (most are 0 and most predictions are >0)
    hist(abs(unlist(ReferenceResiduals)), breaks="Scott") # Lognormal/Chisquared distribution
    hist(unlist(ReferenceResiduals)^2, breaks="Scott") # Extreme Lognormal/Chisquared distribution
    mean(abs(unlist(ReferenceResiduals))) # MAE

    hist(unlist(TesteeResiduals), breaks="Scott")
    hist(abs(unlist(TesteeResiduals)), breaks="Scott")

    # Histogram of differences between residuals
    hist(abs(unlist(ReferenceResiduals)) - abs(unlist(TesteeResiduals)), breaks="Scott") # Looks normal-ish
    hist(unlist(ReferenceResiduals)^2 - unlist(TesteeResiduals)^2, breaks="Scott") # Looks less normal-ish
    # qq plot - actually a ~ rather than normal, it's normal only for middle values, because of kurtosis
    qqnorm(abs(unlist(ReferenceResiduals)) - abs(unlist(TesteeResiduals)))
    qqline(abs(unlist(ReferenceResiduals)) - abs(unlist(TesteeResiduals)))

    # QQ plot of RMSE
    qqnorm(unlist(ReferenceResiduals)^2 - unlist(TesteeResiduals)^2) # Even worse
    qqline(unlist(ReferenceResiduals)^2 - unlist(TesteeResiduals)^2)
}

SignificanceTest = function(ReferenceResiduals, TesteeResiduals, extra=FALSE)
{
    # Overall MAE
    MAEtpval = t.test(abs(unlist(ReferenceResiduals)), abs(unlist(TesteeResiduals)), paired=TRUE)$p.val # With normality assumption p=0.08
    MAEwpval = wilcox.test(abs(unlist(ReferenceResiduals)), abs(unlist(TesteeResiduals)), paired=TRUE)$p.val # Without it p<0.001

    # Overall RMSE
    RMSEtpval = if (extra) t.test(unlist(ReferenceResiduals)^2, unlist(TesteeResiduals)^2, paired=TRUE)$p.val else NA # p<0.001
    RMSEwpval = if (extra) wilcox.test(unlist(ReferenceResiduals)^2, unlist(TesteeResiduals)^2, paired=TRUE)$p.val else NA # p<0.001
    
    # Overall ME
    MEtpval = if (extra) t.test(unlist(ReferenceResiduals), unlist(TesteeResiduals), paired=TRUE)$p.val else NA
    MEwpval = if (extra) wilcox.test(unlist(ReferenceResiduals), unlist(TesteeResiduals), paired=TRUE)$p.val else NA

    return(round(matrix(c(MAEtpval, MAEwpval, RMSEtpval, RMSEwpval, MEtpval, MEwpval), 2, 3, dimnames=list(c("T-test", "Wilcoxon test"),c("MAE", "RMSE", "ME"))), 3))
    
}


## Change - not used

GetChangeResidualTable = function(PredictionTable)
{
    FractionColumns = c(paste0(Classes,".y"),
                        paste0(Classes,".x"))
    ChangesTable = NULL
    TargetYears = seq(min(PredictionTable$dataYear), max(PredictionTable$dataYear), 1)
    for (YearTo in TargetYears[-1])
    {
        YearFrom = YearTo-1
        YearFromTable = PredictionTable[PredictionTable$dataYear == YearFrom,]
        YearToTable = PredictionTable[PredictionTable$dataYear == YearTo,]
        
        if (suppressWarnings(!all(YearToTable$location_id == YearFromTable$location_id)))
        {
            # In some cases we have missing predictions for particular years,
            # remove them from both years in that case
            CommonIDs = intersect(YearFromTable$location_id, YearToTable$location_id)
            YearFromTable = YearFromTable[YearFromTable$location_id %in% CommonIDs,]
            YearToTable = YearToTable[YearToTable$location_id %in% CommonIDs,]
        }
        stopifnot(all(YearToTable$location_id == YearFromTable$location_id))
        
        ChangeTable = YearToTable[FractionColumns] - YearFromTable[FractionColumns]
        ChangeTable$location_id = YearToTable$location_id
        ChangeTable$Years = paste(YearFrom, YearTo, sep="-")
        
        ChangesTable = rbind(ChangesTable, ChangeTable)
    }
    Truth = ChangesTable[paste0(GetCommonClassNames(),".y")]
    Prediction = ChangesTable[paste0(GetCommonClassNames(),".x")]
    return(Prediction - Truth)
}

## ANOVA & Tukey HSD
# We have four factors: model, class, location id and year, but we can merge id and year into one

GetANOVATable = function(ResTables)
{
    ANOVAlist = lapply(ResTables, reshape2::melt, variable.name="Class", id.vars=c("location_year"))
    ANOVAlist = lapply(1:length(ANOVAlist), function(x){
        ANOVAlist[[x]][["Model"]] = names(ANOVAlist)[x]
        return(ANOVAlist[[x]])
    })
    ANOVAtable = do.call(rbind, ANOVAlist)
    ANOVAtable[["Model"]] = as.factor(ANOVAtable[["Model"]])

    return(ANOVAtable)
}
ANOVAtable = GetANOVATable(ResTables)
AOVmodel = lmer(value ~ Model + (1 | Class/location_year), data=ANOVAtable)
anova(AOVmodel)
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))


## Run tests
# RF models
RFTables = GetResidualTables(list(
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-val.csv"),
    "Random Forest + LOESS" = read.csv("../../data/predictions/summer-loess/mean_predictions-val.csv"),
    "Random Forest + BFAST Lite" = read.csv("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-val.csv"),
    "RF + NDVI-only BFAST Lite" = read.csv("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions-val.csv"),
    "Random Forest + BEAST" = read.csv("../../data/predictions/summer-beast/mean-predictions-val.csv")
))
RFANOVAtable = GetANOVATable(RFTables)
RFAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=RFANOVAtable))
RFAOVmodel = value(RFAOVfuture)
anova(RFAOVmodel) # 2323156  8582.2 < 2.2e-16
summary(RFAOVmodel)
summary(glht(RFAOVmodel, linfct = mcp(Model = "Tukey")))

# DW models
DWTables = GetResidualTables(list(
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-val.csv"),
    "Dynamic World + LOESS" = read.csv("../../data/predictions/dynamicworld-loess/predictions-val.csv"),
    "Dynamic World + BFAST Lite" = read.csv("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-val.csv")
))
DWANOVAtable = GetANOVATable(DWTables)
DWAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=DWANOVAtable))
AOVmodel = value(DWAOVfuture)
anova(AOVmodel) # 1934436  4489.9 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF vs DF

RFDWTables = GetResidualTables(list(
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-val.csv"),
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-val.csv")
))
RFDWANOVAtable = GetANOVATable(RFDWTables)
RFDWAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=RFDWANOVAtable))
AOVmodel = value(RFDWAOVfuture)
anova(AOVmodel) # 991982   44358 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF cropland vs DF cropland
RFDWCrops = RFDWANOVAtable[RFDWANOVAtable$Class == "crops.x", ]
RFDWCropAOVfuture = future(lmer(value ~ Model + (1 | location_year), data=RFDWCrops))
AOVmodel = value(RFDWCropAOVfuture)
anova(AOVmodel) # 141709  95.153 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# DW classes
DWClasses = RFDWANOVAtable[RFDWANOVAtable$Model == "Dynamic World",]
DWClassAOVfuture = future(lmer(value ~ Class + (1 | location_year), data=DWClasses))
AOVmodel = value(DWClassAOVfuture)
anova(AOVmodel) # 991963   24256 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Class = "Tukey")))

# Are trees overestimated by DW?
DW = read.csv("../../data/predictions/dynamicworld/dynamicworld-val.csv")
DW$trees.res = DW[["tree.x"]] - DW[["tree.y"]]
t.test(DW[["trees.res"]]) # Significantly underestimated: t = -8.1808, df = 141989, p-value = 2.843e-16
t.test(sample(DW[["trees.res"]], 100000))

## Change

# RF
ChangeRFTables = GetResidualTables(list(
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-change_val_scaled.csv"),
    "Random Forest + LOESS" = read.csv("../../data/predictions/summer-loess/mean_predictions-change_val_scaled.csv"),
    "Random Forest + BFAST Lite" = read.csv("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-change_val_scaled.csv"),
    "RF + NDVI-only BFAST Lite" = read.csv("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions-change_val_scaled.csv"),
    "Random Forest + BEAST" = read.csv("../../data/predictions/summer-beast/mean-predictions-change_val_scaled.csv")
), YearVar="Years")

ChangeRFANOVAtable = GetANOVATable(ChangeRFTables)
ChangeRFAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=ChangeRFANOVAtable))
RFAOVmodel = value(ChangeRFAOVfuture)
anova(RFAOVmodel) # 1858527   41368 < 2.2e-16
summary(RFAOVmodel)
summary(glht(RFAOVmodel, linfct = mcp(Model = "Tukey")))

# DW
ChangeDWTables = GetResidualTables(list(
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-change_val_scaled.csv"),
    "Dynamic World + LOESS" = read.csv("../../data/predictions/dynamicworld-loess/predictions-change_val_scaled.csv"),
    "Dynamic World + BFAST Lite" = read.csv("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-change_val_scaled.csv")
), YearVar="Years")
ChangeDWANOVAtable = GetANOVATable(ChangeDWTables)
ChangeDWAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=ChangeDWANOVAtable))
AOVmodel = value(ChangeDWAOVfuture)
anova(AOVmodel) # 1512615   52296 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF vs DW

RFDWChangeTables = GetResidualTables(list(
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-change_val_scaled.csv"),
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-change_val_scaled.csv")
), YearVar="Years")
RFDWChangeANOVAtable = GetANOVATable(RFDWChangeTables)
RFDWChangeAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=RFDWChangeANOVAtable))
AOVmodel = value(RFDWAOVfuture)
anova(AOVmodel) # 991982   44358 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

## Trends
# RF
TrendRFTables = GetResidualTables(list(
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-trends-yearly.csv"),
    "Random Forest + LOESS" = read.csv("../../data/predictions/summer-loess/mean_predictions-trends-yearly.csv"),
    "Random Forest + BFAST Lite" = read.csv("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trends-yearly.csv"),
    "RF + NDVI-only BFAST Lite" = read.csv("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions-trends-yearly.csv"),
    "Random Forest + BEAST" = read.csv("../../data/predictions/summer-beast/mean-predictions-trends-yearly.csv")
), VarName="ChangeTrend")

TrendRFANOVAtable = GetANOVATable(TrendRFTables)
TrendRFAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=TrendRFANOVAtable))
RFAOVmodel = value(TrendRFAOVfuture)
anova(RFAOVmodel) # 464628  4273.9 < 2.2e-16
summary(RFAOVmodel)
summary(glht(RFAOVmodel, linfct = mcp(Model = "Tukey")))

# DW
TrendDWTables = GetResidualTables(list(
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-trends-yearly.csv"),
    "Dynamic World + LOESS" = read.csv("../../data/predictions/dynamicworld-loess/predictions-trends-yearly.csv"),
    "Dynamic World + BFAST Lite" = read.csv("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-trends-yearly.csv")
), VarName="ChangeTrend")
TrendDWANOVAtable = GetANOVATable(TrendDWTables)
TrendDWAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=TrendDWANOVAtable))
AOVmodel = value(TrendDWAOVfuture)
anova(AOVmodel) # 415891    4905 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF vs DW

RFDWTrendTables = GetResidualTables(list(
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-trends-yearly.csv"),
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-trends-yearly.csv")
), VarName="ChangeTrend")
RFDWTrendANOVAtable = GetANOVATable(RFDWTrendTables)
RFDWTrendAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=RFDWTrendANOVAtable))
AOVmodel = value(RFDWTrendAOVfuture)
anova(AOVmodel) # 213856  2222.3 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF cropland vs DF cropland
RFDWTrendCrops = RFDWTrendANOVAtable[RFDWTrendANOVAtable$Class == "ChangeTrend.crops", ]
RFDWTrendCropAOVfuture = future(lmer(value ~ Model + (1 | location_year), data=RFDWTrendCrops))
AOVmodel = value(RFDWTrendCropAOVfuture)
anova(AOVmodel) # 30550  152.76 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF herbaceous vs DW herbaceous
RFDWTrendGrass = RFDWTrendANOVAtable[RFDWTrendANOVAtable$Class == "ChangeTrend.grassland", ]
RFDWTrendGrassAOVfuture = future(lmer(value ~ Model + (1 | location_year), data=RFDWTrendGrass))
AOVmodel = value(RFDWTrendGrassAOVfuture)
anova(AOVmodel) # 30550  432.75 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

## Variability
# RF
VarRFTables = GetResidualTables(list(
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-trends-yearly.csv"),
    "Random Forest + LOESS" = read.csv("../../data/predictions/summer-loess/mean_predictions-trends-yearly.csv"),
    "Random Forest + BFAST Lite" = read.csv("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trends-yearly.csv"),
    "RF + NDVI-only BFAST Lite" = read.csv("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions-trends-yearly.csv"),
    "Random Forest + BEAST" = read.csv("../../data/predictions/summer-beast/mean-predictions-trends-yearly.csv")
), VarName="ChangeRMSD")

VarRFANOVAtable = GetANOVATable(VarRFTables)
VarRFAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=VarRFANOVAtable))
RFAOVmodel = value(VarRFAOVfuture)
anova(RFAOVmodel) # 464628   20106 < 2.2e-16
summary(RFAOVmodel)
summary(glht(RFAOVmodel, linfct = mcp(Model = "Tukey")))

# DW
VarDWTables = GetResidualTables(list(
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-trends-yearly.csv"),
    "Dynamic World + LOESS" = read.csv("../../data/predictions/dynamicworld-loess/predictions-trends-yearly.csv"),
    "Dynamic World + BFAST Lite" = read.csv("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-trends-yearly.csv")
), VarName="ChangeRMSD")
VarDWANOVAtable = GetANOVATable(VarDWTables)
VarDWAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=VarDWANOVAtable))
AOVmodel = value(VarDWAOVfuture)
anova(AOVmodel) # 415882   29093 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF vs DW

RFDWVarTables = GetResidualTables(list(
    "Random Forest regression" = read.csv("../../data/predictions/summer/mean_predictions_scaled-trends-yearly.csv"),
    "Dynamic World" = read.csv("../../data/predictions/dynamicworld/dynamicworld-trends-yearly.csv")
), VarName="ChangeRMSD")
RFDWVarANOVAtable = GetANOVATable(RFDWVarTables)
RFDWVarAOVfuture = future(lmer(value ~ Model + (1 | Class/location_year), data=RFDWVarANOVAtable))
AOVmodel = value(RFDWVarAOVfuture)
anova(AOVmodel) # 213856  7293.6 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF cropland vs DF cropland
RFDWVarCrops = RFDWVarANOVAtable[RFDWVarANOVAtable$Class == "ChangeRMSD.crops", ]
RFDWVarCropAOVfuture = future(lmer(value ~ Model + (1 | location_year), data=RFDWVarCrops))
AOVmodel = value(RFDWVarCropAOVfuture)
anova(AOVmodel) # 30550  115.05 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))

# RF herbaceous vs DW herbaceous
RFDWVarGrass = RFDWVarANOVAtable[RFDWVarANOVAtable$Class == "ChangeRMSD.grassland", ]
RFDWVarGrassAOVfuture = future(lmer(value ~ Model + (1 | location_year), data=RFDWVarGrass))
AOVmodel = value(RFDWVarGrassAOVfuture)
anova(AOVmodel) # 30550  324.03 < 2.2e-16
summary(AOVmodel)
summary(glht(AOVmodel, linfct = mcp(Model = "Tukey")))
