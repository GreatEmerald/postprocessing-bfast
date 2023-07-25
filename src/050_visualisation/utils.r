library(ggplot2)
library(reshape2)
source("../utils/covariate-names.r")

# Visualise a model comparison as a bar chart,
# reading from CSVs in the format of the AccucayStatsTable
# 
# InputDir: where the CSVs are (TLD)
# InputPattern: pattern to find only the relevant CSVs
# InputFiles: filenames to CSVs, overrides InputDir/InputPattern
# ModelMap: named character vector, where the name is the model codename
#   in the format "directory filename.csv" and the value is the human readable
#   name. Only the mentioned models will be used.
#   The legend will be ordered in the supplied order.
# VariablePattern: pattern to filter stat variables to (i.e. class names)
# Statistics: character vector of column names to visualise, NULL means all
# main: the title
# filename: file to save it to
VisualiseModelStats = function(InputDir=NULL, InputPattern=NULL, InputFiles=NULL,
    ModelMap, VariablePattern=NULL, Statistics=NULL, main="", filename="", xlab="Class")
{
    if (is.null(InputFiles))
        InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

    # Read files
    InputList = lapply(InputFiles, read.csv)
    # Filter away statistics we don't need
    if (!is.null(Statistics))
        InputList = lapply(InputList, `[`, c("X", Statistics))
    # Remove unwanted classes
    if (!is.null(VariablePattern))
        InputList = lapply(InputList, function(x) x[grep(VariablePattern, x$X),])
    # Rename to pretty names
    RemovePattern = if (!is.null(VariablePattern)) VariablePattern else "\\.."
    InputList = lapply(InputList, function(x){x$X = PrettifyNames(x$X, RemovePattern); x})
    # Convert to long format for ggplot
    InputList = lapply(InputList, melt, variable.name = "Statistic", value.name = "Error", id.vars="X")
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
    
#     if (!is.null(VariablePattern))
#     {
#         # Filter to the variables of interest
#         StatTable = StatTable[grep(VariablePattern, StatTable$X),]
#     }

    # Plot
    Plot = ggplot(StatTable, aes(x=X, y=Error, fill=model)) +
        geom_bar(stat="identity", position="dodge", width=0.8) +
        scale_fill_brewer(palette="Set1") +
        facet_wrap(~Statistic) +
        ggtitle(main) +
        xlab("Class") +
        #theme_dark() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    print(Plot)
    if (filename != "")
        ggsave(filename)
        
    #ggplot(OverallTable, aes(x=Statistic, y=Error, fill=model)) +
    #    geom_bar(stat="identity", position="dodge") +
    #    geom_text(label=round(OverallTable$Error, 2), position=position_dodge(width = .9), vjust = 0) +
    #    ggtitle("Overall pooled yearly change")
}
