# Predict dense random forest on validation data
# This is used as a baseline and the target to postprocess

library(ranger)

source("load-sampling-data.r")
source("GetFeatures.r")
source("covariate-names.r")
source("../utils/utils.r")

SR_GPKG = "../../data/WURChange20152019_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/wur_validation_features"
Feature_GPKGs = c(SR_GPKG,
                  list.files(Feature_GPKG, glob2rx("*.gpkg"), full.names = TRUE))
ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
OutputDir = "../../data/models/"

# Find all the models and predict them on reference data

ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)
FeatureNames = unlist(sapply(Feature_GPKGs, function(x)st_layers(x)$name), use.names=FALSE)

# Select only features of interest. These should match the model
FeatureNames = FilterFeatureNames(FeatureNames)

# Input into the RF: each time point should become one row, each feature is one column,
# and put into the reference data so that each point is associated with an LC fraction
# This is limited to the year 2015
FeatureTable = GetFeatures(FeatureNames, Feature_GPKGs)

# Merge with reference data
FeatureTable = merge(FeatureTable, ReferenceData, by="location_id")

# Rename features
names(FeatureTable) = RenameFeatures(names(FeatureTable))
FeatureNames = RenameFeatures(FeatureNames)

# Load each model, predict on the reference data, save predictions (gpkg/rds?)

