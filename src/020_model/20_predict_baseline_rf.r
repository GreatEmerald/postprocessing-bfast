# Predict dense random forest on validation data
# This is used as a baseline and the target to postprocess

library(ranger)

source("../utils/load-sampling-data.r")
source("GetFeatures.r")
source("../utils/covariate-names.r")
source("../utils/utils.r")

SR_GPKG = "../../data/WURChange20152019_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/wur_validation_features"
Feature_GPKGs = c(SR_GPKG,
                  list.files(Feature_GPKG, glob2rx("*.gpkg"), full.names = TRUE))
ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
InputDir = "../../data/models" # Where to look for models
OutputDir = "../../data/predictions" # where to put prediction files

if (!dir.exists(OutputDir))
    dir.create(OutputDir)

# Find all the models and predict them on reference data

ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)
FeatureNames = unlist(sapply(Feature_GPKGs, function(x)st_layers(x)$name), use.names=FALSE)

# Select only features of interest. These should match the model
FeatureNames = FilterFeatureNames(FeatureNames)

# Input into the RF: each time point should become one row, each feature is one column,
# and put into the reference data so that each point is associated with an LC fraction
FeatureTable = GetFeatures(FeatureNames, Feature_GPKGs, start=as.Date("2000-01-01"), end=as.Date("2030-01-01"))

# Add a year column
FeatureTable$timestamp.x = as.Date(FeatureTable$timestamp, "X%Y.%m.%d")
FeatureTable$dataYear = lubridate::year(FeatureTable$timestamp.x)

# Merge with reference data
#FeatureTable = merge(FeatureTable, ReferenceData, by=c("location_id", "dataYear"))

# Rename features
names(FeatureTable) = RenameFeatures(names(FeatureTable))
FeatureNames = RenameFeatures(FeatureNames)

PredictStat = function(ModelFile, Type)
{
    if (length(grep(".rds", ModelFile, fixed=TRUE)) <= 0)
        return()
    
    ClassName = sub(".rds", "", basename(ModelFile), fixed = TRUE)
    print(ModelFile)
    print(ClassName)
    ClassModel = readRDS(ModelFile)
    ClassPrediction = if (Type == "mean")
    {
        predict(ClassModel, FeatureTable)$prediction
    } else if (Type == "median") {
        stepsize = 1000000
        steps = ceiling(nrow(FeatureTable)/stepsize)
        Result = NULL
        for (i in 1:steps) {
            startid = (i-1)*stepsize+1
            stopid = min(startid + stepsize - 1, nrow(FeatureTable))
            print(paste("Handling points", startid, "to", stopid))
            Result = c(Result, c(predict(ClassModel, FeatureTable[startid:stopid,], type="quantiles", quantiles=0.5)$predictions))
            gc()
        }
        Result
        # There appears to be a memory leak and we run out of RAM here.
        # Should save as a file and then load it later
    }
    print(str(ClassPrediction))
    ClassPrediction = data.frame(ClassPrediction)
    names(ClassPrediction) = ClassName
    return(ClassPrediction)
}

# Load each model, predict on the reference data, save predictions (gpkg/rds?)
InputDirs = list.dirs(InputDir, recursive = FALSE)
for (InputSubdir in InputDirs)
{
    ModelFiles = list.files(InputSubdir, recursive = TRUE, full.names = TRUE)
    
    OutSubdir = file.path(OutputDir, basename(InputSubdir))
    if (!dir.exists(OutSubdir))
        dir.create(OutSubdir)
    
    for (type in c("mean", "median"))
    {
        OutFile = file.path(OutSubdir, paste0(type, "_predictions.rds"))
        if (file.exists(OutFile))
            next
        
        OutputCols = lapply(ModelFiles, PredictStat, Type = type)
        OutputCols = do.call(cbind, OutputCols)
        OutputTable = FeatureTable[c("location_id", "timestamp.x")]
        OutputTable = cbind(OutputTable, OutputCols)
        rm(OutputCols); gc()

        saveRDS(OutputTable, OutFile)
        rm(OutputTable); gc()
    }
}

if (FALSE) {
sqrt(mean((FeatureTable$tree_prediction - FeatureTable$tree)^2)) # 19
mean(abs(FeatureTable$tree_prediction - FeatureTable$tree)) # 12

sqrt(mean((FeatureTable$tree_prediction_median - FeatureTable$tree)^2)) # 21
mean(abs(FeatureTable$tree_prediction_median - FeatureTable$tree)) # 9.5

FeatureTableSummer = FeatureTable[(FeatureTable$subpix_mean_y >= 0 &
                                       (FeatureTable$timestamp.x == as.Date("2015-07-14") |
                                        FeatureTable$timestamp.x == as.Date("2016-07-16") |
                                        FeatureTable$timestamp.x == as.Date("2017-07-19") |
                                        FeatureTable$timestamp.x == as.Date("2018-07-22") |
                                        FeatureTable$timestamp.x == as.Date("2019-07-09"))) | 
                                  (FeatureTable$subpix_mean_y <  0 &
                                       (FeatureTable$timestamp.x == as.Date("2015-01-19") |
                                        FeatureTable$timestamp.x == as.Date("2016-01-22") |
                                        FeatureTable$timestamp.x == as.Date("2017-01-08") |
                                        FeatureTable$timestamp.x == as.Date("2018-01-11") |
                                        FeatureTable$timestamp.x == as.Date("2019-01-14"))),]



sqrt(mean((FeatureTableSummer$tree_prediction - FeatureTableSummer$tree)^2)) # 17.5
mean(abs(FeatureTableSummer$tree_prediction - FeatureTableSummer$tree)) # 10.5
# PROBA-V was 19/13, so we are already better

sqrt(mean((FeatureTableSummer$tree_prediction_median - FeatureTableSummer$tree)^2)) # 18.7
mean(abs(FeatureTableSummer$tree_prediction_median - FeatureTableSummer$tree)) # 8.2
# PROBA-V was 22/11, so we are already better
# NB: these don't take into account non-summer predictions, so actually we are worse (19.91/11.33, 21.97/8.68)

TreesLocation = sample(FeatureTable[FeatureTable$tree > 0, "location_id"], 1)

TreesFeatures = FeatureTable[FeatureTable$location_id == TreesLocation,]
TreesFeatures = TreesFeatures[order(TreesFeatures$timestamp.x),]

print(plot(NDVI*100 ~ timestamp.x, data=TreesFeatures, type="l", ylim=c(0,100), main=TreesLocation))
print(lines(tree_prediction ~ timestamp.x, data=TreesFeatures, col="green"))
print(lines(tree_prediction_median ~ timestamp.x, data=TreesFeatures, col="darkgreen"))
print(lines(tree ~ timestamp.x, data=TreesFeatures, col="red"))

}
