# Visualise individual pixel trajectories
# Based on the LPS code
library(ggplot2)

source("../utils/load-sampling-data.r")
source("../utils/covariate-names.r")

# Load validation data
ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
PlotOutDir = "../../output/trajectories/"

ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)

#DynamicWorld = FlattenGPKG("../../data/WURChange20152019_DynamicWorld_TS.gpkg")
#DynamicWorld = RenameReferenceData(DynamicWorld)
#DynamicWorld = ReclassifyAndScale(DynamicWorld)

# Load models to plot
RFPredictions = readRDS("../../data/predictions/summer/mean_predictions_scaled.rds")
#RFMedian = readRDS("../../data/predictions/summer/median_predictions.rds")
#BFBaseline = readRDS("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions.rds")
#BFLPredictions = readRDS("../../data/predictions/summer-bfl-m30-harmon2-scaled-h12-fb/median_predictions.rds")
BFLPredictions = readRDS("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions.rds")
DynamicWorld = readRDS("../../data/predictions/dynamicworld/dynamicworld.rds")
DWBFL = readRDS("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions.rds")
BEAST = readRDS("../../data/predictions/summer-beast/mean-predictions.rds")
LOESS = readRDS("../../data/predictions/dynamicworld-loess/predictions.rds")
BFBaseline = readRDS("../../data/predictions/summer-bfbaseline-m02-harmon-scaled/mean_predictions.rds")

#models = list(RFPredictions=RFPredictions, RFMedian=RFMedian, BFBaseline=BFBaseline,
#    BFLMedianPredictions=BFLPredictions, DynamicWorld = DynamicWorld, DWBFL = DWBFL)

models = list(`Dynamic World` = DynamicWorld, `Dynamic World + LOESS`=LOESS, `Dynamic World + BFAST Lite` = DWBFL,
    `Random Forest regression`=RFPredictions, `Random Forest + BFAST Lite`=BFLPredictions,
    `RF + NDVI-only BFAST Lite`=BFBaseline,  `Random Forest + BEAST`=BEAST)

PlotTrajectory = function(location_id, models, classes=GetCommonClassNames(),
    legend.pos="left", xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")),
    grid=c(3, 3), title="", size=dev.size())
{
    # Filter all data to the location id
    ReferenceData = ReferenceData[ReferenceData$location_id == location_id,]
    sub = title
    title = paste("Reference data for location ", location_id)
    
    # Make a grid
    opar = par(no.readonly=TRUE)
    par(mfrow=grid)
    
    
    # Plot reference data
    plot(as.formula(paste(classes[1], '~ as.Date(paste0(dataYear, "-01-15"))')), ReferenceData, ylim=c(0,100),
        xlim=xlim, type="b",
        ylab="Land cover fraction (%)", xlab="",
        main=title, sub=sub, xaxt = "n")
    axis(1, at = seq(as.Date("2015-01-01"), as.Date("2020-01-01"), "years"), labels=2015:2020)
    
    if (length(classes) > 1)
        for (classidx in 2:length(classes))
            points(as.formula(paste(classes[classidx], "~ as.Date(paste0(dataYear, '-01-15'))")), ReferenceData, col=classidx, type="b")
    legend(legend.pos, legend=classes, fill=1:length(classes))
    
    # Plot each model
    for (modelidx in 1:length(models))
    {
        model = models[[modelidx]]
        model = model[model$location_id == location_id,]
        model = model[order(model$timestamp.x),]

        plot(as.formula(paste(classes[1], '~ timestamp.x')), model, ylim=c(0,100),
            xlim=xlim, type="l",
            ylab="Land cover fraction (%)", xlab="",
            main=names(models)[modelidx],
            xaxt = "n")
        axis(1, at = seq(as.Date("2015-01-01"), as.Date("2020-01-01"), "years"), labels=2015:2020)
        
        if (length(classes) > 1)
            for (classidx in 2:length(classes))
                lines(as.formula(paste(classes[classidx], "~ timestamp.x")), model, col=classidx)
    }
    # Save in files
    dev.copy2pdf(file = file.path(PlotOutDir, paste0(Sys.Date(), "-", location_id, "-", sub, ".pdf")))
    png(filename = file.path(PlotOutDir, paste0(Sys.Date(), "-", location_id, "-", sub, ".png")), width=size[1], height=size[2], units="in", res=100)
    dev.set(dev.prev()) # Go back to active plot
    dev.copy() # Copy to the next device, i.e. PNG
    dev.off() # Turn off the PNG device to get back to the active plotting device again
    par(opar)
}

# Accessory function of getting the location of an ID in a format easy to enter into Google Earth
# For GEE you need invert=TRUE
GetLocation = function(location_id, invert=FALSE)
{
    coorder = if (!invert) c("subpix_mean_y", "subpix_mean_x") else c("subpix_mean_x", "subpix_mean_y")
    toString(ReferenceData[ReferenceData$location_id == location_id, coorder][1,])
}

# We are interested in point 2996157 - Uruguay eucalyptus plantation, seedlings at March 2018
PlotTrajectory(2996157, models, classes=c("tree", "shrub", "grassland", "crops"), title="Uruguay eucalyptus", grid=c(2,3))
PlotTrajectory(2996157, models, classes="grassland", title="Uruguay eucalyptus", grid=c(2,3))
# Lake Chad, the area is sometimes covered by aquatic plants and sometimes it's water.
# There was actually no major change here, the semantics are whether it's grassland (covered by plants) or not, which changes seasonally.
PlotTrajectory(1954894, models, classes=c("water", "grassland"), title = "Lake Chad", grid=c(2,3))
GetLocation(1954894)
# Johannesburg city construction. 2015-05 grass+trees, 2015-07 bare, 2016-09 finished (built+bare), 2017-05 built+grassy, no change since then
# Misdetected as increase in grass and crops in 2016
PlotTrajectory(1955403, models, classes=c("urban_built_up", "bare", "tree", "grassland"), title="Johannesburg construction", grid=c(2,3))
GetLocation(1955403)
# Forest regrowth in Madagascar. Grass/shrubs in 2016, small trees in 2019, trees in 2021.
# Detected as grass to trees transition instead
GetLocation(1955715)
PlotTrajectory(1955715, models, classes=c("shrub", "tree", "grassland", "crops"), title="Madagascar regrowth", legend.pos="topleft", grid=c(2,3))
# Warsaw new construction. 2016-09 grass+trees, 2016-11 foundations, 2017-05 building and grass+bare, 2020-03 urban with a bit of grass
# Median is pretty much accurate, only it detects grass instead of urban
GetLocation(1956373)
PlotTrajectory(1956373, models, classes=c("urban_built_up", "bare", "tree", "grassland", "crops"), title="Warsaw construction", legend.pos="top", grid=c(2,3))
# Tree management in Norway. 2015 entirely trees, 2019-08 mostly clear-cut grass, stable after that
# Median is fairly correct here; reference says tere was bare soil but I can't confirm that
GetLocation(1971306)
PlotTrajectory(1971306, models, classes=c("shrub", "tree", "grassland", "crops", "bare"), title="Norway thinning", legend.pos="top", grid=c(2,3))
# Clear cut in Sweden, 2015 full of trees, 2018 entirely grass
# Mostly correctly predicted by median and BFL very useful here, only a bit confused with crops
GetLocation(1971640)
PlotTrajectory(1971640, models, classes=c("shrub", "tree", "grassland", "crops", "bare"), title="Sweden clearcut", grid=c(2,3))
# Sheffield, no change, but nearby area got built up and the roofs of the area in question got updated in 2017-2018.
# Correctly detected as no change, but BFAST did not smooth it out enough. Predicted a bit of tree loss (could be true)
GetLocation(1971769)
PlotTrajectory(1971769, models, classes=c("urban_built_up", "bare", "tree", "grassland", "crops"), title="Sheffield no change", grid=c(2,3))
# Bristol, no change aside from some new solar panels, correctly detected as no change
# However, the statistics say a lot of change, probably due to algorithm to select peak of season
GetLocation(1971773)
PlotTrajectory(1971773, models, classes=c("urban_built_up", "bare", "tree", "grassland", "crops"), title="Bristol no change", grid=c(2,3))
# Greenhouses in Spain next to VERY slowly growing tree plantation. No change.
# RF does not understand what this is, mean is detected as no change, median is low confidence and thus reports jumps due to noise
# => filtering by magnitude should be applied before rescaling!
GetLocation(1971880)
PlotTrajectory(1971880, models, classes=c("urban_built_up", "bare", "tree", "grassland", "crops"), title="Spain greenhouses no change", grid=c(2,3))
# Agriculture in Spain, no change in LC though some change in field
# Mean detects no change well, median gets confused between grass and agriculture between years
GetLocation(1971926)
PlotTrajectory(1971926, models, classes=c("urban_built_up", "bare", "tree", "grassland", "crops"), title="Spain agriculture no change", grid=c(2,3))
# Agriculture in Italy
# Here median predicts mostly correctly no change, only a little bit of confusion with grass.
# Google Dynamic world thinks at some point there are suddenly trees
GetLocation(1972000)
PlotTrajectory(1972000, models, classes=c("urban_built_up", "bare", "tree", "grassland", "crops"), title="Italy agriculture no change", grid=c(2,3))
# Tree plantation in Spain. 2015: crops, 2016: 50% tree plantation (bare + trees), rest crops; 2017: 30% more becomes a plantation; trees continue growing.
# Reference data not quite right, but also predictions, though they show a nice regrowth curve.
# Crops get replaced by grass and then shrubs and then trees according to median before BFL, BFL shows shrubs and trees increasing together
# Google thinks it's just all crops and no change
GetLocation(1972119)
PlotTrajectory(1972119, models, classes=c("shrub", "bare", "tree", "grassland", "crops"), title="Spain plantation growth", grid=c(2,3))
# Harbin, China. Problematic construction site. 2015: bare soil that turns into grass as it gets abandoned. 2016: grass grows into shrubs.
# 2017: starts looking agricultural and then cleared again. 2018: bare soil, -09 planted with grass. 2019: abandoned again, grass grows over bare soil.
# 2020: cleared again for construction.
# Mean shows grass only with seasonality into bare during winter. Median just predicts grass with seasonality.
GetLocation(1995461)
PlotTrajectory(1995461, models, classes=c("urban_built_up", "bare", "shrub", "grassland", "crops"), title="Harbin construction", grid=c(2,3))
# Kazakh agriculture. Half of the pixel is a different field. No changes, but seasonal diffrences (droughts, a nearby river dries up). The reference is incorrect and shows grass transition to crops.
# Confusing between grass and crops also in the predictor.
GetLocation(1995812)
PlotTrajectory(1995812, models, classes=c("shrub", "bare", "tree", "grassland", "crops"), grid=c(2,3))

# Check trees and water
# Construction of Atbara dam in Sudan
# Predicted perfectly, pinpointing time. RFMedian does the best job.
# Dynamic World thinks it has always been water?
GetLocation(2187224)
PlotTrajectory(2187224, models, classes=c("water", "grassland", "shrub", "crops"), title="Sudan dam construction", grid=c(2,3))
# Lake Eyasi (Tanzania) drying out. It is a seasonal lake, sometimes there is water and sometimes not.
# The model predicts water has been there since 2018, reference data thinks it's all dry. But at least in 2018 it's actually water.
GetLocation(2187489)
PlotTrajectory(2187489, models, classes=c("water", "grassland", "bare", "crops"), title="Eyasi lake", grid=c(2,3))

# Check examples of change
ChangeData = read.csv("../../data/reference-trends.csv")
ChangeIDs = ChangeData[ChangeData["ChangeTotal.Overall"] > 0, "location_id"]

ChangeID = 2806410 # 2806410 Australian seasonal lake
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "grassland", "shrub", "crops")) # Water class is a good candidate

ChangeID = 1997679 # 1997679 Russian selective logging
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "grassland", "tree", "shrub")) # Trees are a great example, but BEAST failed

ChangeID = 2804962 # ! Dominica shallow lake
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "grassland", "shrub", "crops")) # Water is a good example, NDVI works best here

ChangeID = 2807107 # ! 2807107 Australia desert river meander
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "bare", "grassland", "shrub")) # Bare is a good example, but all models behave similarly

ChangeID = 2804358 # !! 2804358 Kazakh reservoir wetlands
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "bare", "grassland", "shrub")) # Water is a good example, but RF+BFL falls behind a bit

(ChangeID = sample(ChangeIDs, 1)) # 2996877 Australia desert river meander again
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "bare", "grassland", "shrub")) # All models are relatively poor here

(ChangeID = 2996360) # ! 2996360 Ethiopia dam reservoir
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "bare", "grassland", "shrub", "crops")) # Water is good, but BFL does worse due to an outlier

(ChangeID = sample(ChangeIDs, 1)) # 1954291 Algeria agriculture
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "shrub", "grassland", "crops")) # Barely any change

(ChangeID = sample(ChangeIDs, 1)) # 2131014 Chile tree planation
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("shrub", "bare", "tree", "grassland", "crops")) # Trees are an interesting example, but no BEAST

(ChangeID = sample(ChangeIDs, 1)) # 2996822 Lake Muirhead, seasonal lake in Australia
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "crops", "grassland", "water")) # Mixed, can't tell

(ChangeID = sample(ChangeIDs, 1)) # 2806353 Australian savannah on fire in 2015
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "crops", "tree", "grassland", "shrub")) # Trees are interesting, but little change

(ChangeID = sample(ChangeIDs, 1)) # 2809210 Californian tree plantation
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "crops", "tree", "grassland", "shrub")) # Little change

(ChangeID = sample(ChangeIDs, 1)) # 2804906 Eucalyptus plantation
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "crops", "tree", "grassland", "shrub")) # Trees are very interesting, but no BEAST

(ChangeID = sample(ChangeIDs, 1)) # 2809349 Huanghe floodplain, very confusing
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("bare", "crops", "grassland", "water", "shrub")) # Reference is also confusing

(ChangeID = sample(ChangeIDs, 1)) # 2807105 Australian rivers return
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("water", "bare", "grassland", "shrub")) # Irrigation event, bare is good but no BEAST

(ChangeID = sample(ChangeIDs, 1)) # 1972119 No change Spain agriculture
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "shrub")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2802895 Australia browning
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "shrub")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2806137 Mongolian desert, no change
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "shrub")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2808959 Spain lake dynamics
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "water")) # Water is interesting, but no BEAST

(ChangeID = sample(ChangeIDs, 1)) # 2806124 Russian drying lake
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "water")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2997637 Moscow lake, no change
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "water")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2806159 Seasonal river in China
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "water")) # Confusing

(ChangeID = 2805465) # !! 2805465 Iraq salt lake
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "water")) # Nice example of water, though ground truth is rather unclear

(ChangeID = sample(ChangeIDs, 1)) # 1972554 Italy agriculture, no change but tree loss
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "crops")) # No big change

(ChangeID = sample(ChangeIDs, 1)) # 2996044 Eucalyptus again
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "crops", "shrub")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2807098 Australian seasonal lake again
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "water", "shrub")) # Confusing

(ChangeID = sample(ChangeIDs, 1)) # 2805303 Indus river meander
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "water")) # Confusing, including the reference

(ChangeID = 2806156) # ! 2806156 No change agriculture
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "water")) # Water is a good example, though the reference is odd

(ChangeID = sample(ChangeIDs, 1)) # 2809103 Canadian agriculture/grass near a mine, snowy in winter
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "shrub")) # Confusing

(ChangeID = 2997037) # !! 2997037 Spain forest fire in 2018
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "shrub")) # Trees are interesting, though BFL oversmooths a bit here

(ChangeID = sample(ChangeIDs, 1)) # 1954403 Morocco seasonal variation
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "shrub")) # No change

(ChangeID = sample(ChangeIDs, 1)) # 2997216 China terrace agriculture in 2018 (in the neighbourhood)
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "shrub")) # No change

(ChangeID = 2996889) # 2996889 Australia drying lake
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("crops", "bare", "grassland", "water", "shrub")) # Water is good example, BFL oversmooths again

(ChangeID = 2808606) # 2808606 Australia fire in 2019
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "shrub")) # Trees are an intersting example, but not very clear

(ChangeID = sample(ChangeIDs, 1)) # 1953958 Africa deforestation to smallholder agriculture
GetLocation(ChangeID)
PlotTrajectory(ChangeID, models, classes=c("tree", "bare", "grassland", "shrub", "crops")) # Confusing


# Make a plot of a single class


PlotClass = function(location_id, models, class="urban_built_up",
    xlim=c(as.Date("2015-01-01"), as.Date("2020-01-01")),
    title="", size=dev.size())
{
    # Filter all data to the location id
    ReferenceData = ReferenceData[ReferenceData$location_id == location_id,]
    
    # Add a Date column
    TimeShiftString = if (ReferenceData$subpix_mean_y[1] > 0) "-07-15" else "-01-15"
    ReferenceData$Date = as.Date(paste0(ReferenceData$dataYear, TimeShiftString))
    
    # Rename class to Fraction
    names(ReferenceData)[grep(class, names(ReferenceData))] = "Fraction"
    
    # Create a new var to store all models in one
    PlotData = ReferenceData[c("Date", "Fraction")]
    PlotData$Model = "Reference"
    
    # Add all other model predictions
    for (modelidx in 1:length(models))
    {
        model = models[[modelidx]]
        model = model[model$location_id == location_id,]
        #model = model[order(model$timestamp.x),]
        model$Date = as.Date(model[["timestamp.x"]])
        PlotData = rbind(PlotData,
            data.frame(Date = model$Date, Fraction = model[[class]], Model = names(models)[[modelidx]])
        )
    }
    
    # Order factors
    PlotData$Model = ordered(PlotData$Model, levels=c("Reference", names(models)))
    # Use the same palette as for bar plots, but add reference
    Cols = RColorBrewer::brewer.pal(length(models)+1, "Set1")
    # 5 is the reference colour, bring it to front
    Cols = Cols[c(5,1:4,6:length(Cols))]
        
    Output = ggplot(PlotData, aes(Date, Fraction, group=Model)) + geom_line(aes(colour=Model, linewidth=Model)) +
        scale_colour_manual(values=ModelPalette) +
        scale_x_date(date_breaks="1 year", limits=xlim) +
        scale_linewidth_manual(values=c(1.5, rep(0.5, length(models))))
       # geom_point()
        
    
    # Save in files
    ggsave(file.path(PlotOutDir, paste0(Sys.Date(), "-", location_id, "-", class, ".pdf")), Output, width=size[1], height=size[2], units="in")
    
    #dev.copy2pdf(file = file.path(PlotOutDir, paste0(Sys.Date(), "-", location_id, "-", sub, ".pdf")))
    #png(filename = file.path(PlotOutDir, paste0(Sys.Date(), "-", location_id, "-", sub, ".png")), width=size[1], height=size[2], units="in", res=100)
    #dev.set(dev.prev()) # Go back to active plot
    #dev.copy() # Copy to the next device, i.e. PNG
    #dev.off() # Turn off the PNG device to get back to the active plotting device again
    #par(opar)
    
    return(Output)
}

PlotClass(1971773, models, "urban_built_up", xlim=c(as.Date("2015-01-01"), as.Date("2021-01-01")), title="Built-up class fraction in a suburban area of Kingswood, Bristol, UK")
PlotClass(2804358, models, "water", xlim=c(as.Date("2015-01-01"), as.Date("2021-01-01")), title="Water change in Kapshagay reservoir, delta of river Ile, Kazakhstan")

