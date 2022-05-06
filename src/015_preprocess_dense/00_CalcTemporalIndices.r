# Calculate temporal indices (harmonics, VIs, etc. over time).
# AKA 1D convolutions.
# Input: Geopackage of Landsat surface reflectance time series from GEE.
# Output: A (new) Geopackage with one layer (table) per feature in the same schema as input.

library(sf)
library(zoo)
library(parallel)

source("../utils/utils.r")

SR_GPKG = "../../data/WURChange20152019_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/wur_validation_features/"

# Read all layers into a single list "SR"
SRNames = st_layers(SR_GPKG)$name
SR = lapply(SRNames, function(name) st_read(SR_GPKG, layer=name))

# Save a template for writing, with consistent column names
OutTemplate = SR[[1]]
names(OutTemplate)[datecols(OutTemplate)] = strtrim(names(OutTemplate)[datecols(OutTemplate)], 11)

# Convert into a zoo matrix
SRZ = lapply(lapply(SR, SFToMatrix), MatrixToZoo)
names(SRZ) = SRNames

# Calculate all indices of interest
# L8 TC transformation coeffs from http://dx.doi.org/10.1080/2150704X.2014.915434
OSAVI = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {return(1.16 * ((NIR-RED)/(NIR+RED+0.16)))}
NDMI  = function(BLUE=NULL, GREEN=NULL, RED=NULL, NIR,  SWIR)
    {return((NIR-SWIR)/(NIR+SWIR))}
NDVI  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {return((NIR-RED)/(NIR+RED))}
EVI   = function(BLUE     , GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {return(2.5*((NIR-RED)/((NIR + 6*RED - 7.5*BLUE)+1)))}
NIRv  = function(BLUE=NULL, GREEN=NULL, RED     , NIR,  SWIR=NULL)
    {((NIR-RED) / (NIR+RED))*NIR}
TCb   = function(BLUE,      GREEN,      RED     , NIR,  SWIR1, SWIR2)
    {BLUE*0.3029+GREEN*0.2786+RED*0.4733+NIR*0.5599+SWIR1*0.508+SWIR2*0.1872}
TCg   = function(BLUE,      GREEN,      RED     , NIR,  SWIR1, SWIR2)
    {BLUE*(-0.2941)+GREEN*(-0.243)+RED*(-0.5424)+NIR*0.7276+SWIR1*0.0713+SWIR2*(-0.1608)}
TCw   = function(BLUE,      GREEN,      RED     , NIR,  SWIR1, SWIR2)
    {BLUE*0.1511+GREEN*0.1973+RED*0.3283+NIR*0.3407+SWIR1*(-0.7117)+SWIR2*(-0.4559)}
NDSI  = function(BLUE=NULL, GREEN,      RED=NULL, NIR=NULL, SWIR1, SWIR2=NULL)
    {(GREEN-SWIR1)/(GREEN+SWIR1)}

OutFile = paste(Feature_GPKG, "NDVI.gpkg", sep="_")
if (!file.exists(OutFile))
{
    NDVIz = NDVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    NDVIsf = ZooToSF(NDVIz, OutTemplate)
    st_write(NDVIsf, OutFile)
    rm(NDVIsf, NDVIz)
}

OutFile = paste(Feature_GPKG, "EVI.gpkg", sep="_")
if (!file.exists(OutFile))
{
    EVIz = EVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    EVIsf = ZooToSF(EVIz, OutTemplate)
    st_write(EVIsf, OutFile)
    rm(EVIsf, EVIz)
}
    
#    OSAVIz = OSAVI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
#    OSAVIsf = ZooToSF(OSAVIz, OutTemplate)
#    st_write(OSAVIsf, paste(Feature_GPKG, "OSAVI.gpkg", sep="_"))
#    rm(OSAVIsf, OSAVIz)
    
OutFile = paste(Feature_GPKG, "NIRv.gpkg", sep="_")
if (!file.exists(OutFile))
{
    NIRvz = NIRv(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    NIRvsf = ZooToSF(NIRvz, OutTemplate)
    st_write(NIRvsf, OutFile)
    rm(NIRvsf, NIRvz)
}

OutFile = paste(Feature_GPKG, "NDMI.gpkg", sep="_")
if (!file.exists(OutFile))
{
    NDMIz = NDMI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]])
    NDMIsf = ZooToSF(NDMIz, OutTemplate)
    st_write(NDMIsf, OutFile)
    rm(NDMIsf, NDMIz)
}

OutFile = paste(Feature_GPKG, "NBR.gpkg", sep="_")
if (!file.exists(OutFile))
{
    NBRz = NDMI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B7"]])
    NBRsf = ZooToSF(NBRz, OutTemplate)
    st_write(NBRsf, OutFile)
    rm(NBRsf, NBRz)
}

OutFile = paste(Feature_GPKG, "TCb.gpkg", sep="_")
if (!file.exists(OutFile))
{
    TCbz = TCb(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
    TCbsf = ZooToSF(TCbz, OutTemplate)
    st_write(TCbsf, OutFile)
    rm(TCbsf, TCbz)
}

OutFile = paste(Feature_GPKG, "TCg.gpkg", sep="_")
if (!file.exists(OutFile))
{
    TCgz = TCg(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
    TCgsf = ZooToSF(TCgz, OutTemplate)
    st_write(TCgsf, OutFile)
    rm(TCgsf, TCgz)
}

OutFile = paste(Feature_GPKG, "TCw.gpkg", sep="_")
if (!file.exists(OutFile))
{
    TCwz = TCw(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
    TCwsf = ZooToSF(TCwz, OutTemplate)
    st_write(TCwsf, OutFile)
    rm(TCwsf, TCwz)
}

OutFile = paste(Feature_GPKG, "NDSI.gpkg", sep="_")
if (!file.exists(OutFile))
{
    NDSIz = NDSI(SRZ[["SR_B2"]], SRZ[["SR_B3"]], SRZ[["SR_B4"]], SRZ[["SR_B5"]], SRZ[["SR_B6"]], SRZ[["SR_B7"]])
    NDSIsf = ZooToSF(NDSIz, OutTemplate)
    st_write(NDSIsf, OutFile)
    rm(NDSIsf, NDSIz)
}

# Calculate convolutions: 1-year, 3-year
# A window is centred on the observation, so intervals are:
# 3 year = 68
# 1 year = 23
# Quarter= 6
# A mean would be affected by outliers, so take the median (unless we perform extra filtering)
VIs = c("NDVI", "EVI", "NDSI", "NIRv", "NBR", "TCg") # c("NDVI", "EVI", "NDSI", "NIRv", "NDMI", "NBR", "TCb", "TCg", "TCw")
WinSizes = c(yearly=23, threeyear=68)
Stats = c("quant05", "median", "quant95", "IQR")

# Get a missing data mask
VI = st_read(paste0(Feature_GPKG, "_", VIs[1], ".gpkg"))
VIz = SFToZoo(VI)
MissingData = is.na(VIz)
rm(VIz)

# Calculate rolling statistics over a given window size for a vegetation index
# VI layer name, window size in observations (16-day units), statistic function,
# zoo object to apply to, what file to write to
CalcRollingStat = function(VIname, WinSize, Stat, OutFile = Feature_GPKG)
{
    OutName = paste0(OutFile, "_", paste(VIname, WinSize, Stat, sep="_"), ".gpkg")
    if (file.exists(OutName))
        return() # If the layer already exists, don't do anything
    VIzoo = SFToZoo(st_read(paste0(OutFile, "_", VIname, ".gpkg")))
    StatFun = switch(Stat,
                     quant05=function(...) quantile(..., probs=0.05),
                     quant95=function(...) quantile(..., probs=0.95),
                     get(Stat))
    VI_stat = rollapply(VIzoo, WinSize, StatFun, na.rm = TRUE, partial = TRUE)
    VI_stat = as.matrix(VI_stat) # Workaround for a bug in zoo
    VI_stat[MissingData] = NA
    VI_stat[!is.finite(VI_stat)] = NA
    st_write(ZooToSF(VI_stat, VI), OutName)
    rm(VI_stat)
    return()
}

l <- expand.grid(VIname=VIs, WinSize=WinSizes, Stat=Stats, stringsAsFactors=FALSE)
RollingStats = do.call(mcmapply, c(FUN=CalcRollingStat, as.list(l), mc.cores=5))
