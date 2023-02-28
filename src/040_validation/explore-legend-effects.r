# If we make the legend easier for the classifier, are the results better (and BFAST Lite more useful)?

source("../utils/covariate-names.r")

RFMedian = read.csv("../../data/predictions/summer/median_predictions-change_val_scaled.csv")
BFLPredictions = read.csv("../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions-change_val_scaled.csv")
BFLMean = read.csv("../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions-change_val_scaled.csv")
RFPredictions = read.csv("../../data/predictions/summer/mean_predictions-change_val_scaled.csv")
BFBaseline = read.csv("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions-change_val_scaled.csv")

plot(water.y~water.x, RFMedian)
plot(water.y~water.x, BFLPredictions)
hist(BFLPredictions$water.x, breaks="Scott")
hist(BFLPredictions$water.y, breaks="Scott")
# 0.08 for RFMedian and 0.10 for BFLPredictions
for (Class in GetCommonClassNames())
{
    print(paste(Class, cor(RFMedian[[paste0(Class, ".y")]], RFMedian[[paste0(Class, ".x")]])^2))
}

for (Class in GetCommonClassNames())
{
    print(paste(Class, cor(BFLPredictions[[paste0(Class, ".y")]], BFLPredictions[[paste0(Class, ".x")]])^2))
}

RFMedian$any.y = rowSums(abs(RFMedian[paste0(GetCommonClassNames(), ".y")]))
RFMedian$any.x = rowSums(abs(RFMedian[paste0(GetCommonClassNames(), ".x")]))
plot(any.y~any.x, RFMedian)
cor(RFMedian$any.y, RFMedian$any.x)^2 # 0.04, not as good as water alone

BFLPredictions$any.y = rowSums(abs(BFLPredictions[paste0(GetCommonClassNames(), ".y")]))
BFLPredictions$any.x = rowSums(abs(BFLPredictions[paste0(GetCommonClassNames(), ".x")]))
plot(any.y~any.x, BFLPredictions)
cor(BFLPredictions$any.y, BFLPredictions$any.x)^2 # 0.07, not as good as water alone but double that of RFMedian

BFBaseline$any.y = rowSums(abs(BFBaseline[paste0(GetCommonClassNames(), ".y")]))
BFBaseline$any.x = rowSums(abs(BFBaseline[paste0(GetCommonClassNames(), ".x")]))
plot(any.y~any.x, BFBaseline)
cor(BFBaseline$any.y, BFBaseline$any.x)^2

MergeCols = function(classes.to.merge, model)
{
    model$merged.y = rowSums(model[paste0(classes.to.merge, ".y")])
    model$merged.x = rowSums(model[paste0(classes.to.merge, ".x")])
    print(plot(merged.y~merged.x, model))
    print(cor(model$merged.y, model$merged.x)^2)
}

Vegetated = c("tree", "shrub", "grassland", "crops")
MergeCols(Vegetated, RFPredictions) # 0.06
MergeCols(Vegetated, BFBaseline) # 0.07
MergeCols(Vegetated, RFMedian) # 0.04
MergeCols(Vegetated, BFLPredictions) # 0.04
Unvegetated = c("urban_built_up", "bare", "water")
MergeCols(Unvegetated, RFPredictions) # 0.06
MergeCols(Unvegetated, BFBaseline) # 0.07
MergeCols(Unvegetated, RFMedian) # 0.04
MergeCols(Unvegetated, BFLPredictions) # 0.05

# Let's get a balanced set of change vs no change
changesamples = which(RFMedian$any.y > 0)
set.seed(0xfedbeef)
nochangesamples = sample(which(RFMedian$any.y <= 0), length(changesamples))
RFMedian = RFMedian[c(changesamples, nochangesamples),]
BFLPredictions = BFLPredictions[c(changesamples, nochangesamples),]
BFLMean = BFLMean[c(changesamples, nochangesamples),]
RFPredictions = RFPredictions[c(changesamples, nochangesamples),]
changesamples = which(BFBaseline$any.y > 0)
set.seed(0xfedbeef)
nochangesamples = sample(which(BFBaseline$any.y <= 0), length(changesamples))
BFBaseline = BFBaseline[c(changesamples, nochangesamples),]

MergeCols(Vegetated, RFPredictions) # 0.18
MergeCols(Vegetated, BFLMean) # 0.12
MergeCols(Vegetated, BFBaseline) # 0.15
MergeCols(Vegetated, RFMedian) # 0.12
MergeCols(Vegetated, BFLPredictions) # 0.12
MergeCols(Unvegetated, RFPredictions) # 0.18
MergeCols(Unvegetated, BFLMean) # 0.12
MergeCols(Unvegetated, BFBaseline) # 0.15
MergeCols(Unvegetated, RFMedian) # 0.11
MergeCols(Unvegetated, BFLPredictions) # 0.12

# Best are water and trees, rest are very difficult
MergeCols("water", RFPredictions) # 0.28
MergeCols("water", BFLMean) # 0.18
