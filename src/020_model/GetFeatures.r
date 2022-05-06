library(sf)
library(zoo)
library(reshape2)

# Read one feature for a given time span, and format it in a single column.
GetFeatureColumn = function(FeatureName, FeatureFiles,
                            start=as.Date("2015-01-01"), end=as.Date("2016-01-01"),
                            col.only=TRUE)
{
    for (FeatureFile in FeatureFiles)
    {
        FullFeatureName = grep(glob2rx(paste0("*", FeatureName)), st_layers(FeatureFile)$name, value=TRUE)
        if (length(FullFeatureName) > 0)
            break; # Select the first file in which we can find the feature
    }
    
    VI = st_read(FeatureFile, FullFeatureName)
    VI = SFToZoo(VI)
    VI = window(VI, start=start, end=end)
    
    # Melt the entire table!
    VI_table = melt(as.matrix(VI), varnames = c("timestamp", "location_id"), value.name = FeatureName)
    if (col.only) return(VI_table[[3]]) else return(VI_table)
}

# Returns a data.frame of requested features in a format suitable for RF.
# FeatureNames: Layer names in the input GPKG.
# FeatureFiles: GPKG files in which to search for features.
GetFeatures = function(FeatureNames, FeatureFiles, na.rm=TRUE, ...)
{
    FeatureTable = GetFeatureColumn(FeatureNames[1], FeatureFiles, col.only=FALSE, ...)
    if (length(FeatureNames > 1))
    {
        FeatureList = lapply(FeatureNames[2:length(FeatureNames)], GetFeatureColumn, FeatureFiles, ...)
        names(FeatureList) = FeatureNames[2:length(FeatureNames)]
        FeatureTable = cbind(FeatureTable, list2DF(FeatureList))
        rm(FeatureList)
    }
    
    # Remove missing data rows
    if (na.rm)
        FeatureTable = FeatureTable[complete.cases(FeatureTable),]
    
    return(FeatureTable)
}
