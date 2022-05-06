library(sf)
library(zoo)
library(probaV)
library(parallel)

source("../utils/utils.r")

#SR_GPKG = "../../data/IIASATraining2015_Landsat8_TS.gpkg"
#Feature_GPKG = "../../data/features/IIASATraining2015_Landsat8_Features"
SR_GPKG = "../../data/WURChange20152019_Landsat8_TS.gpkg"
Feature_GPKG = "../../data/wur_validation_features/"

# Calculate a rolling regression of seasonality

# Multivariate regression outputs get concatenated, we want an array instead
# so dim(output) = c(x, ncoefs, y)
# And then we grab each coef as output, i.e. output[,1,] for intercepts

phaser = function(co, si)
{
    tau = 2*pi
    result = atan2(si, co) %% tau
    names(result) = NULL
    return(result)
}
amplituder = function(co, si)
{
    result = sqrt(co^2 + si^2)
    names(result) = NULL
    return(result)
}

# probaV package from install_github("JornDallinga/probaV")
# Output is 12 stats: min max intercept co si co2 si2 trend phase1 amplitude1 phase2 amplitude2
GetHarmonics = function(TS)
{
    if (all(is.na(TS)))
    {
        c(min=NA, max=NA, intercept=NA, co=NA, si=NA, co2=NA, si2=NA, trend=NA,
          phase1=NA, amplitude1=NA, phase2=NA, amplitude2=NA)
    } else {
        HarmCoefs = getHarmMetrics(TS, order=2)
        p1 = phaser(HarmCoefs["co"], HarmCoefs["si"])
        p2 = phaser(HarmCoefs["co2"], HarmCoefs["si2"])
        a1 = amplituder(HarmCoefs["co"], HarmCoefs["si"])
        a2 = amplituder(HarmCoefs["co2"], HarmCoefs["si2"])
        c(HarmCoefs, phase1=p1, amplitude1=a1, phase2=p2, amplitude2=a2)
    }
}

CalcHarmonics = function(VIname, WinSize, OutFile = Feature_GPKG)
{
    OutLayers = c("min", "max", "intercept", "co", "si", "co2", "si2", "trend",
                  "phase1", "amplitude1", "phase2", "amplitude2")
    OutName = paste0(OutFile, "_", paste(VIname, WinSize, OutLayers, sep="_"), ".gpkg")
    if (file.exists(OutName[length(OutLayers)]))
        return() # If the last layer already written, don't do anything
    VIzoo = SFToZoo(st_read(paste0(OutFile, "_", VIname, ".gpkg")))
    #ct = now(); print(ncol(VIzoo)); VIzoo = VIzoo[,1:100]
    HarmCoefs = rollapply(VIzoo, WinSize, GetHarmonics, partial = TRUE, coredata = FALSE)
    # Convert into an array
    HarmCoefs = as.matrix(HarmCoefs)
    dim(HarmCoefs) = c(dim(VIzoo)[1], length(OutLayers), dim(VIzoo)[2])
    rm(VIzoo)
    gc()
    # Make it reasonable, i.e. layers in the last dimension
    HarmCoefs = aperm(HarmCoefs, c(1,3,2))
    HarmCoefs[MissingData] = NA
    HarmCoefs[!is.finite(HarmCoefs)] = NA
    #print(now() - ct)
    for (i in 3:length(OutLayers)) # Discard min/max, already calculated
        st_write(ZooToSF(HarmCoefs[,,i], VI), paste0(OutFile, "_", paste(VIname, WinSize, OutLayers[i], sep = "_"), ".gpkg"))
    rm(HarmCoefs)
    gc()
    return()
}

VIs = c("NDVI", "EVI", "NIRv", "NDMI", "NBR", "TCb", "TCg", "TCw", "NDSI")
WinSizes = c(68, 114) # Three and five years
l <- expand.grid(VIname=VIs, WinSize=WinSizes,stringsAsFactors=FALSE)

VI = st_read(paste0(Feature_GPKG, "_", VIs[1], ".gpkg"))
VIz = SFToZoo(VI)
MissingData = is.na(VIz)
rm(VIz)

# Run! And hopefully don't crash
RollingReg = do.call(mcmapply, c(FUN=CalcHarmonics, as.list(l), mc.cores=4))
