# Create visualisations for accuracy pooled into buckets by change amount
library(ggplot2)
source("../utils/covariate-names.r")
source("../utils/accuracy-statistics.r")

TargetModels = c(
    "2022-09-15-mean-rf"              = "../../data/predictions/summer/mean_predictions_scaled-change_val_scaled.csv",
    "2022-09-15-median-rf"            = "../../data/predictions/summer/median_predictions-change_val_scaled.csv",
    "2022-09-15-mean-ndvi-baseline"   = "../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions-change_val_scaled.csv",
    "2022-09-15-median-ndvi-baseline" = "../../data/predictions/summer-bfbaseline-m02-harmon-scaled/median_predictions-change_val_scaled.csv",
    "2022-09-15-mean-bfl"             = "../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-change_val_scaled.csv",
    "2022-09-15-median-bfl"           = "../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions-change_val_scaled.csv"
    )
for (i in 1:length(TargetModels))
{
    TargetModel = TargetModels[i]

    TargetTable = read.csv(TargetModel, row.names=1)

    # Celculate the amount of change according to validation
    TargetTable$Change = rowSums(abs(TargetTable[paste0(GetCommonClassNames(), ".y")]))/2
    # hist(TargetTable$Change[TargetTable$Change>0], breaks="Scott")

    Predictions = abs(TargetTable[paste0(GetCommonClassNames(), ".x")])
    names(Predictions) = GetCommonClassNames()
    Truth = abs(TargetTable[paste0(GetCommonClassNames(), ".y")])
    names(Truth) = GetCommonClassNames()

    APUPlot = function(predicted, observed, by.observed=FALSE)
    {
        GetASTable = function(Class, by.observed=FALSE)
        {
            PredClass = if (Class != "Overall") predicted[,Class] else unlist(predicted)
            TruthClass = if (Class != "Overall") observed[,Class] else unlist(observed)
            Bins = if (by.observed) round(TruthClass, -1) else round(PredClass, -1)
            ValidationDF = data.frame(Truth=TruthClass, Bins=as.factor(Bins), Predicted=PredClass)
            BinAS = t(sapply(levels(ValidationDF$Bins), function(Bin) {
                ValidationBin = ValidationDF[ValidationDF$Bins == Bin,]
                AS = AccuracyStats(ValidationBin$Predicted, ValidationBin$Truth)
                return(c(unlist(AS), obs=nrow(ValidationBin)/nrow(ValidationDF)*100, obsabs=nrow(ValidationBin), bin=as.numeric(Bin)))
            }))
            BinAS = data.frame(BinAS, class=Class)
            return(BinAS)
        }
        BinAS = lapply(GetCommonClassNames(), GetASTable, by.observed=by.observed)
        BinAS = c(list(GetASTable("Overall", by.observed=by.observed)), BinAS)
        BinAS = do.call("rbind", BinAS)
        # Exclude too small bins
        BinAS = BinAS[BinAS$obsabs > 10,]
        # Reorder and prettify names
        ClassNames = PrettifyNames(BinAS$class)
        BinAS$class = factor(ClassNames, c("Overall", unique(ClassNames[ClassNames != "Overall"])))
        xlabname = if (by.observed) "Observed fraction of change (%)" else "Predicted fraction of change (%)"
        scaleval = 1#1.5    
        GetASTable = function(Class, by.observed=FALSE)
        {
            PredClass = if (Class != "Overall") predicted[,Class] else unlist(predicted)
            TruthClass = if (Class != "Overall") observed[,Class] else unlist(observed)
            Bins = if (by.observed) round(TruthClass, -1) else round(PredClass, -1)
            ValidationDF = data.frame(Truth=TruthClass, Bins=as.factor(Bins), Predicted=PredClass)
            BinAS = t(sapply(levels(ValidationDF$Bins), function(Bin) {
                ValidationBin = ValidationDF[ValidationDF$Bins == Bin,]
                AS = AccuracyStats(ValidationBin$Predicted, ValidationBin$Truth)
                return(c(unlist(AS), obs=nrow(ValidationBin)/nrow(ValidationDF)*100, obsabs=nrow(ValidationBin), bin=as.numeric(Bin)))
            }))
            BinAS = data.frame(BinAS, class=Class)
            return(BinAS)
        } # 300
        ggplot(BinAS, aes(x=bin, y=RMSE)) + geom_line(aes(colour="RMSE")) +
            geom_line(aes(y=MAE, colour="MAE")) + geom_line(aes(y=ME, colour="ME")) +
            geom_line(aes(y=RMSEAdj, colour="RMSEAdj")) +
            geom_col(aes(y=obs/scaleval, fill="Density"), alpha=0, colour="black") +
            scale_y_continuous(sec.axis = sec_axis(~.*scaleval, name = "Probability density (%)")) +
            labs(x=xlabname, y="Statistic (%)") +
            scale_colour_discrete(name = 'Statistic', breaks=c("RMSE", "MAE", "ME", "RMSEAdj")) + scale_fill_manual(name = 'Histogram', values=c("Density"="white")) +
            facet_wrap(vars(class), nrow=2)
    }

    APUPlot(Predictions, Truth, by.observed=TRUE)

    ggsave(paste0(names(TargetModels[i]), "-apu-stats.pdf"))
    
    PlotBox = function(predicted, observed, main="", binpredicted=FALSE, transposeaxes=FALSE,
                   varwidth = TRUE, outlier.size = 0.3, outlier.alpha = 0.1, width=0.2, display.n=FALSE)
    {
        #OneToOne = data.frame(Predicted=seq(0, 100, 10), Bins=1:11)
        if (!binpredicted) {
            TruthBins = unlist(observed)
            TruthBins = round(TruthBins, -1)
            ValidationDF = data.frame(Truth=unlist(observed), Bins=as.factor(TruthBins), Predicted=unlist(predicted))
            OneToOne = data.frame(Predicted=as.numeric(levels(ValidationDF$Bins)), Bins=1:length(levels(ValidationDF$Bins)))
            ncount = if (display.n) {
                paste(levels(ValidationDF$Bins),"\n(N=",round(table(ValidationDF$Bins)/1000),"k)",sep="")
            } else waiver()
            ggplot(ValidationDF, aes(Bins, Predicted)) +
                stat_boxplot(geom ='errorbar', width=width) +
                geom_boxplot(varwidth = varwidth, outlier.size = outlier.size, outlier.alpha = outlier.alpha) +
                geom_line(data=OneToOne) +
                xlab("Reference") +
                scale_x_discrete(labels=ncount) +
                ggtitle(main)
        } else {
            PredBins = unlist(predicted)
            PredBins = round(PredBins, -1)
            ValidationDF = data.frame(Truth=unlist(observed), Bins=as.factor(PredBins), Predicted=unlist(predicted))
            OneToOne = data.frame(Predicted=as.numeric(levels(ValidationDF$Bins)), Bins=1:length(levels(ValidationDF$Bins)))
            if (!transposeaxes)
            {
                ncount = if (display.n) {
                    paste(levels(ValidationDF$Truth),"\n(N=",round(table(ValidationDF$Truth)/1000),"k)",sep="")
                } else waiver()
                ggplot(ValidationDF, aes(Truth, Bins)) +
                    stat_boxplot(geom ='errorbar', width=width) +
                    geom_boxplot(varwidth = varwidth, outlier.size = outlier.size, outlier.alpha = outlier.alpha) +
                    geom_line(data=OneToOne, aes(Predicted, Bins)) +
                    ylab("Predicted") + xlab("Reference") +
                    scale_x_discrete(labels=ncount)+
                    ggtitle(main)
            } else {
                ncount = if (display.n) {
                    paste(levels(ValidationDF$Bins),"\n(N=",round(table(ValidationDF$Bins)/1000, 1),"k)",sep="")
                } else waiver()
                ggplot(ValidationDF, aes(Bins, Truth)) +
                    stat_boxplot(geom ='errorbar', width=width) +
                    geom_boxplot(varwidth = varwidth, outlier.size = outlier.size, outlier.alpha = outlier.alpha) +
                    geom_line(data=OneToOne, aes(Bins, Predicted)) +
                    xlab("Predicted") + ylab("Reference") +
                    scale_x_discrete(labels=ncount)+
                    ggtitle(main)
            }
        }
    }
    #pdf(paste0(names(TargetModels[i]), "-boxplot.pdf"))
    PlotBox(Predictions, Truth)
    ggsave(paste0(names(TargetModels[i]), "-boxplot.pdf"))
    #dev.off()
}
