# Utility for returning all relevant classification accuracy statistics

AccuracyStats = function(predicted, observed, relative=FALSE)
{
    RMSE = sqrt(mean(unlist(predicted - observed)^2))
    MAE = mean(abs(unlist(predicted - observed)))
    ME = mean(unlist(predicted - observed))
    RMSEAdj = sqrt(mean(unlist(predicted - observed - ME)^2))
    Result = data.frame(RMSE, MAE, ME, RMSEAdj)
    if (relative)
        Result = Result/mean(unlist(observed))
    return(Result)
}

AccuracyStatTable = function(predicted, observed, relative=FALSE)
{
    Result = AccuracyStats(predicted, observed, relative=relative)
    row.names(Result) = "Overall"
    for (i in 1:ncol(observed))
    {
        RMSE = sqrt(mean(unlist(predicted[,i] - observed[,i])^2))
        MAE = mean(abs(unlist(predicted[,i] - observed[,i])))
        ME = mean(unlist(predicted[,i] - observed[,i]))
        RMSEAdj = sqrt(mean(unlist(predicted[,i] - observed[,1] - ME)^2))
        ColResult = data.frame(RMSE, MAE, ME, RMSEAdj)
        if (relative)
            ColResult = ColResult/mean(unlist(observed[,i]))
        Result = rbind(Result, ColResult)
        row.names(Result)[i+1] = names(observed[i])
    }
    return(Result)
}

# Simply return a table of differences; used for ANOVA
CalcErrors = function(predicted, observed, ...)
{
    SE = unlist(predicted - observed)^2
    AE = abs(unlist(predicted - observed))
    return(data.frame(AE, SE, ...))
}

# Validation metrics and plots
AccuracyStatisticsPlots = function(predicted, observed, ...)
{
    # RMSE values and correlation
    AST = AccuracyStatTable(predicted, observed)
    print(AST)
    op = par(mfrow=c(2,2))
    barplot(AST$RMSE, names.arg=rownames(AST), main="RMSE")
    barplot(AST$MAE, names.arg=rownames(AST), main="MAE")
    barplot(AST$ME, names.arg=rownames(AST), main="ME")
    try(corrplot::corrplot(cor(predicted, observed), method="ellipse"))
    par(op)
}
