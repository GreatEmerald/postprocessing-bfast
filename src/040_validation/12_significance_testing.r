# Load the harmonised predictions of two sets, a reference and testee, and test
# whether the testee is significantly different from the reference.
# This is testing the residuals, so we also use the reference data.

source("../utils/covariate-names.r")

## For yearly maps, simple

ReferencePredictions = "../../data/predictions/summer/mean_predictions_scaled-val.csv"
TesteePredictions = "../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-val.csv"

Classes = GetCommonClassNames()
Reference = read.csv(ReferencePredictions)
Testee = read.csv(TesteePredictions)

# Drop rows that don't match (if any)
CommonIDs = intersect(Reference$location_id, Testee$location_id)
Reference = Reference[Reference$location_id %in% CommonIDs,]
Testee = Testee[Testee$location_id %in% CommonIDs,]

ReferenceResiduals = Reference[paste0(Classes,".x")] - Reference[paste0(Classes,".y")]
TesteeResiduals = Testee[paste0(Classes,".x")] - Testee[paste0(Classes,".y")]

# Diagnostics - skip in production
if (FALSE) {
    hist(abs(unlist(ReferenceResiduals)), breaks="Scott") # Lognormal/Chisquared distribution
    mean(abs(unlist(ReferenceResiduals))) # MAE


    hist(abs(unlist(TesteeResiduals)), breaks="Scott")

    # Histogram of differences between residuals
    hist(abs(unlist(ReferenceResiduals)) - abs(unlist(TesteeResiduals)), breaks="Scott") # Looks normal-ish
    # qq plot - actually a ~ rather than normal, it's normal only for middle values, because of kurtosis
    qqnorm(abs(unlist(ReferenceResiduals)) - abs(unlist(TesteeResiduals)))
    qqline(abs(unlist(ReferenceResiduals)) - abs(unlist(TesteeResiduals)))

    # QQ plot of RMSE
    qqnorm(unlist(ReferenceResiduals)^2 - unlist(TesteeResiduals)^2) # Even worse
    qqline(unlist(ReferenceResiduals)^2 - unlist(TesteeResiduals)^2)
}

SignificanceTest = function(ReferenceResiduals, TesteeResiduals)
{
    # Overall MAE
    MAEtpval = t.test(abs(unlist(ReferenceResiduals)), abs(unlist(TesteeResiduals)), paired=TRUE)$p.val # With normality assumption p=0.08
    MAEwpval = wilcox.test(abs(unlist(ReferenceResiduals)), abs(unlist(TesteeResiduals)), paired=TRUE)$p.val # Without it p<0.001

    # Overall RMSE
    RMSEtpval = t.test(unlist(ReferenceResiduals)^2, unlist(TesteeResiduals)^2, paired=TRUE)$p.val # p<0.001
    RMSEwpval = wilcox.test(unlist(ReferenceResiduals)^2, unlist(TesteeResiduals)^2, paired=TRUE)$p.val# p<0.001

    return(round(matrix(c(MAEtpval, MAEwpval, RMSEtpval, RMSEwpval), 2, 2, dimnames=list(c("T-test", "Wilcoxon test"),c("MAE", "RMSE"))), 3))
}

SignificanceTest(ReferenceResiduals, TesteeResiduals)

## Change

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
        
        if (!all(YearToTable$location_id == YearFromTable$location_id))
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

# Overall change
SignificanceTest(GetChangeResidualTable(Reference), GetChangeResidualTable(Testee))
