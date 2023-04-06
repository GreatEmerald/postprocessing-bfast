library(ggplot2)
library(reshape2)

# Visualise a model comparison as a bar chart,
# reading from CSVs in the format of the AccucayStatsTable
# 
# InputDir: where the CSVs are (TLD)
# InputPattern: pattern to find only the relevant CSVs
# ModelMap: named character vector, where the name is the model codename
#   in the format "directory filename.csv" and the value is the human readable
#   name. Only the mentioned models will be used.
#   The legend will be ordered in the supplied order.
# VariablePattern: pattern to filter stat variables to (i.e. class names)
# main: the title
# filename: file to save it to
VisualiseModelStats = function(InputDir=NULL, InputPattern=NULL, ModelMap, VariablePattern=NULL,
    main="", filename="", InputFiles=NULL)
{
    if (is.null(InputFiles))
        InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

    # Read files
    InputList = lapply(InputFiles, read.csv)
    # Convert to long format for ggplot
    InputList = lapply(InputList, melt, variable.name = "Statistic", id.vars="X")
    # Create a "model" column in the format of "directory filename" for traceback
    InputList = lapply(1:length(InputList), function(idx) {
            InputList[[idx]]$model = paste(basename(dirname(InputFiles[idx])), basename(InputFiles[idx]), sep="/")
            InputList[[idx]]
        })
    StatTable = do.call(rbind, InputList)

    # Filter table:
    # Models of interest
    StatTable = StatTable[StatTable$model %in% names(ModelMap),]
    # Rename into human-readable name
    for (ModelNameIdx in 1:length(ModelMap))
    {
        StatTable[StatTable$model == names(ModelMap[ModelNameIdx]), "model"] = ModelMap[ModelNameIdx]
    }
    # Order as given
    StatTable$model = ordered(StatTable$model, levels=ModelMap)
    
    if (!is.null(VariablePattern))
    {
        # Filter to the variables of interest
        StatTable = StatTable[grep(VariablePattern, StatTable$X),]
    }

    # Plot
    Plot = ggplot(StatTable, aes(x=X, y=value, fill=model)) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_brewer(palette="Set3") +
        facet_wrap(~Statistic) +
        ggtitle(main) +
        theme_dark() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    print(Plot)
    if (filename != "")
        ggsave(filename)
        
    #ggplot(OverallTable, aes(x=Statistic, y=value, fill=model)) +
    #    geom_bar(stat="identity", position="dodge") +
    #    geom_text(label=round(OverallTable$value, 2), position=position_dodge(width = .9), vjust = 0) +
    #    ggtitle("Overall pooled yearly change")
}
