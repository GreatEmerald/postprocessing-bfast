# Create a graph for LPS22 poster
# Show validation data, dense predictions, BFAST predictions and baseline BFAST

source("../utils/load-sampling-data.r")
source("../utils/covariate-names.r")

# Load validation data
ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
# We are interested in point 2996157 - Uruguay eucalyptus plantation, seedlings at March 2018
location_id = 2996157

ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)

ReferenceData = ReferenceData[ReferenceData$location_id == location_id,]

# Predictions: baseline dense RF
RFPredictions = readRDS("../../data/predictions/summer/mean_predictions_scaled.rds")
RFPredictions = RFPredictions[RFPredictions$location_id == location_id,]

RFMedian = readRDS("../../data/predictions/summer-window/median_predictions.rds")
RFMedian = RFMedian[RFMedian$location_id == location_id,]
RFMedian = RFMedian[order(RFMedian$timestamp.x),]

# Baseline BFAST
BFBaseline = readRDS("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions.rds")
BFBaseline = BFBaseline[BFBaseline$location_id == location_id,]

# Best BFAST Lite model so far
BFLPredictions = readRDS("../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/mean_predictions.rds")
BFLPredictions = BFLPredictions[BFLPredictions$location_id == location_id,]

# Plot each

pdf("2022-05-13-a-reference.pdf", height = 3.5)
plot(tree ~ as.Date(paste0(dataYear, "-01-15")), ReferenceData, ylim=c(0,100),
     xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")), type="b",
     ylab="Land cover fraction (%)", xlab="",
     main="Reference data")
for (classidx in 2:4)
    points(as.formula(paste(GetCommonClassNames()[classidx], "~ as.Date(paste0(dataYear, '-01-15'))")), ReferenceData, col=classidx, type="b")
legend("left", legend=GetCommonClassNames()[1:4], fill=1:4)
dev.off()

pdf("2022-05-13-b-rfmean.pdf", height = 3.5)
plot(tree ~ timestamp.x, RFPredictions, ylim=c(0,100),
     xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")), type="l",
     ylab="Land cover fraction (%)", xlab="",
     main="Dense Random Forest predictions")
for (classidx in 2:4)
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ timestamp.x")), RFPredictions, col=classidx)
dev.off()

pdf("2022-05-13-e-rfmedian.pdf", height = 3.5)
plot(tree ~ timestamp.x, RFMedian, ylim=c(0,100),
     xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")), type="l",
     ylab="Land cover fraction (%)", xlab="",
     main="Median voting Random Forest predictions")
for (classidx in 2:4)
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ timestamp.x")), RFMedian, col=classidx)
dev.off()

pdf("2022-05-13-c-bfastlite.pdf", height = 3.5)
plot(tree ~ timestamp.x, BFLPredictions, ylim=c(0,100),
     xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")), type="l",
     ylab="Land cover fraction (%)", xlab="",
     main="BFAST Lite model output")
for (classidx in 2:4)
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ timestamp.x")), BFLPredictions, col=classidx)
dev.off()

pdf("2022-05-13-d-bfastbaseline.pdf", height = 3.5)
plot(tree ~ timestamp.x, BFBaseline, ylim=c(0,100),
     xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")), type="l",
     ylab="Land cover fraction (%)", xlab="",
     main="Baseline yearly predictions")
for (classidx in 2:4)
    lines(as.formula(paste(GetCommonClassNames()[classidx], "~ timestamp.x")), BFBaseline, col=classidx)
dev.off()
