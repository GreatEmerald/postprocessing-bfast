library(sf)
library(ranger)
library(reshape2)

source("load-sampling-data.r")
source("GetFeatures.r")
source("covariate-names.r")
source("../utils/utils.r")

# Perform Random Forest training
# Save the output into a model RDS file + metadata on parameters

SR_GPKG = "../../data/IIASATraining2015_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/features/IIASATraining2015_Landsat8_Features"
Feature_GPKGs = c(SR_GPKG,
                  list.files(dirname(Feature_GPKG), glob2rx(paste0(basename(Feature_GPKG), "*")), full.names = TRUE))
TrainingReferenceCSV = "../../data/raw/training_data_2015_100m_20190402_V4.csv"
OutputDir = "../../data/models/"
if (!dir.exists(OutputDir)) dir.create(OutputDir)

# Scenarios to test:
# - summer: train only on peak vegeatation season
# - random1in20: select a random 1 in 20 space-time points
# - all: train on all of the data, assuming the land cover is constant throughout the year
Scenario = "summer"
OutputDir = file.path(OutputDir, Scenario)
if (!dir.exists(OutputDir)) dir.create(OutputDir)

TrainingData = read.csv(TrainingReferenceCSV)
TrainingData = TidyData(TrainingData)
FeatureNames = unlist(sapply(Feature_GPKGs, function(x)st_layers(x)$name), use.names=FALSE)

# Select only features of interest
FeatureNames = FilterFeatureNames(FeatureNames)

# Input into the RF: each time point should become one row, each feature is one column,
# and put into the reference data so that each point is associated with an LC fraction
# This is limited to the year 2015
FeatureTable = GetFeatures(FeatureNames, Feature_GPKGs)

# Merge with reference data
FeatureTable = merge(FeatureTable, TrainingData, by="location_id")

# Rename features
names(FeatureTable) = RenameFeatures(names(FeatureTable))
FeatureNames = RenameFeatures(FeatureNames)

if (Scenario == "summer")
{
    FeatureTable = FeatureTable[(FeatureTable$y >= 0 & strtrim(FeatureTable$timestamp, 11) == "X2015.07.14") | 
                                (FeatureTable$y <  0 & strtrim(FeatureTable$timestamp, 11) == "X2015.01.19"),]
}

Classes = GetCommonClassNames()

for (Class in Classes)
{
    OutFile = paste0(OutputDir, '/', Class, ".rds")
    if (file.exists(OutFile))
        next
    
    # Run RF
    model = ranger(as.formula(paste(Class, "~", paste(FeatureNames, collapse="+"))), FeatureTable,
                   importance = "permutation", quantreg=TRUE)
    
    # Save model to file
    saveRDS(model, OutFile)
}
