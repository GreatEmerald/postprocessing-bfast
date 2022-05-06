library(sf)
library(corrr)

source("load-sampling-data.r")
source("GetFeatures.r")
source("../utils/utils.r")

# Test features by running random forest models and visualising results

SR_GPKG = "../../data/IIASATraining2015_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/features/IIASATraining2015_Landsat8_Features"
Feature_GPKGs = c(SR_GPKG,
    list.files(dirname(Feature_GPKG), glob2rx(paste0(basename(Feature_GPKG), "*")), full.names = TRUE))
TrainingReferenceCSV = "../../data/raw/training_data_2015_100m_20190402_V4.csv"

TrainingData = read.csv(TrainingReferenceCSV)
TrainingData = TidyData(TrainingData)
FeatureNames = unlist(sapply(Feature_GPKGs, function(x)st_layers(x)$name), use.names=FALSE)

FeatureTable = GetFeatures(FeatureNames, Feature_GPKGs)

# Sample 1 in 20
set.seed(0xbadcafe)
FeatureTable = FeatureTable[sample(1:nrow(FeatureTable), nrow(FeatureTable)/20),]

# Explore feature correlation
CorLong = stretch(shave(correlate(FeatureTable[3:ncol(FeatureTable)])), TRUE)
CorLong = as.data.frame(CorLong[order(abs(CorLong$r), decreasing=TRUE),])

# Drop some features that are highly correlated
# From SR, keep band 4 and 5 and 6 (red-NIR-SWIR)
# From VIs, keep EVI, NIRv, NBR, TCg
# From time, keep 1 year
# From statistics, keep median and IQR
ncol(FeatureTable) # 90
KeepTFeatures = "(EVI|NDVI|NBR|TCg|NDSI)_(68|114)_(IQR|median|quant05|amplitude1|phase1)"
KeepVFeatures = "(EVI|NDVI|NBR|TCg|NDSI)$"
KeepFeatures = c("SR_B4", "SR_B5", "SR_B6",
                 grep(KeepTFeatures, FeatureNames, value = TRUE),
                 grep(KeepVFeatures, FeatureNames, value = TRUE))
length(KeepFeatures) # 15

FeatureTable = GetFeatures(KeepFeatures, Feature_GPKGs)
#FeatureTable = FeatureTable[c("timestamp", "location_id", KeepFeatures)]

CorLong = stretch(shave(correlate(FeatureTable[KeepFeatures])), TRUE)
CorLong = as.data.frame(CorLong[order(abs(CorLong$r), decreasing=TRUE),]) # None over 0.9

# Merge with reference data
FeatureTable = merge(FeatureTable, TrainingData, by="location_id")

# Run RF
library(ranger)
model = ranger(as.formula(paste("tree ~", paste(KeepFeatures, collapse="+"))), FeatureTable,
               importance = "permutation", quantreg=TRUE)

barplot(importance(model), las=2)

# Test predicting a full time series

FullFeatures = GetFeatures(KeepFeatures, Feature_GPKGs, start=as.Date("2000-01-01"), end=as.Date("2030-01-01"))
TreesLocation = sample(FeatureTable[FeatureTable$tree == 100, "location_id"], 5) # 1951225 1754018 2108897 1994254 1927555
ShrubsLocation = sample(FeatureTable[FeatureTable$shrub == 100, "location_id"], 5)

for (location in ShrubsLocation)
{
    TreesFeatures = FullFeatures[FullFeatures$location_id == location, ]
    TreesTime = as.Date(TreesFeatures$timestamp, format="X%Y.%m.%d")
    TreesNIRv = TreesFeatures$IIASATraining2015_Landsat8_Features_NIRv
    print(plot(TreesNIRv/100 ~ TreesTime, type="l", ylim=c(0,100), main=location))
    TreesPrediction = predict(model, TreesFeatures)$prediction
    print(lines(TreesPrediction ~ TreesTime, col="green"))
    TreesMedian = c(predict(model, TreesFeatures, type="quantiles", quantiles=0.5)$predictions)
    print(lines(TreesMedian ~ TreesTime, col="darkgreen"))
}

# Try selecting only peak vegetation condition to train on: July in northern hemisphere, January in southern
FeatureTable = GetFeatures(KeepFeatures, Feature_GPKGs)
FeatureTable = merge(FeatureTable, TrainingData, by="location_id")
FeatureTable = FeatureTable[(FeatureTable$y >= 0 & strtrim(FeatureTable$timestamp, 11) == "X2015.07.14") | 
                            (FeatureTable$y <  0 & strtrim(FeatureTable$timestamp, 11) == "X2015.01.19"),]

# Run RF
model = ranger(as.formula(paste("tree ~", paste(KeepFeatures, collapse="+"))), FeatureTable,
               importance = "permutation", quantreg=TRUE)

barplot(importance(model), las=2) # Red is much more important now, as is TCg, other VIs less important

TreesLocation = c(1951225, 1754018, 2108897, 1994254, 1927555)
for (location in TreesLocation)
{
    TreesFeatures = FullFeatures[FullFeatures$location_id == location, ]
    TreesTime = as.Date(TreesFeatures$timestamp, format="X%Y.%m.%d")
    TreesNDVI = TreesFeatures$IIASATraining2015_Landsat8_Features_NDVI
    print(plot(TreesNDVI*200 ~ TreesTime, type="l", ylim=c(0,100), main=location))
    TreesPrediction = predict(model, TreesFeatures)$prediction
    print(lines(TreesPrediction ~ TreesTime, col="green"))
    TreesMedian = c(predict(model, TreesFeatures, type="quantiles", quantiles=0.5)$predictions)
    print(lines(TreesMedian ~ TreesTime, col="darkgreen"))
}

# Try the longer timescale
KeepTFeatures = "(EVI|NIRv|NBR|TCg)_(23|68)_(IQR|median)"
KeepVFeatures = "(EVI|NIRv|NBR|TCg)$"
KeepFeatures = c("SR_B4", "SR_B5", "SR_B6",
                 grep(KeepTFeatures, FeatureNames, value = TRUE),
                 grep(KeepVFeatures, FeatureNames, value = TRUE))

# Try selecting only peak vegetation condition to train on: July in northern hemisphere, January in southern
FeatureTable = GetFeatures(KeepFeatures, Feature_GPKGs)
FeatureTable = merge(FeatureTable, TrainingData, by="location_id")
FeatureTable = FeatureTable[(FeatureTable$y >= 0 & strtrim(FeatureTable$timestamp, 11) == "X2015.07.14") | 
                            (FeatureTable$y <  0 & strtrim(FeatureTable$timestamp, 11) == "X2015.01.19"),]

# Run RF
model = ranger(as.formula(paste("tree ~", paste(KeepFeatures, collapse="+"))), FeatureTable,
               importance = "permutation", quantreg=TRUE)

opar = par(mar=c(15,4,4,2))
barplot(importance(model), las=2) # 3 years seems to be even more important than 1 year, still EVI and TCg most important
par(opar)

FullFeatures = GetFeatures(KeepFeatures, Feature_GPKGs, start=as.Date("2000-01-01"), end=as.Date("2030-01-01"))
for (location in TreesLocation)
{
    TreesFeatures = FullFeatures[FullFeatures$location_id == location, ]
    TreesTime = as.Date(TreesFeatures$timestamp, format="X%Y.%m.%d")
    TreesNIRv = TreesFeatures$IIASATraining2015_Landsat8_Features_NIRv
    print(plot(TreesNIRv/100 ~ TreesTime, type="l", ylim=c(0,100), main=location))
    TreesPrediction = predict(model, TreesFeatures)$prediction
    print(lines(TreesPrediction ~ TreesTime, col="green"))
    TreesMedian = c(predict(model, TreesFeatures, type="quantiles", quantiles=0.5)$predictions)
    print(lines(TreesMedian ~ TreesTime, col="darkgreen"))
}
# Differences are very minor, it is slightly more smooth
