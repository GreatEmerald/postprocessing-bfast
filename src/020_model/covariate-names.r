# Class names that are in both training and validation sets
GetCommonClassNames = function()
{
    return(c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water"))
}

# For plotting, using colours from CGLOPS
GetCommonClassColours = function(prettify=FALSE, darken_amount=0)
{
    Result = c(tree="#09cc00", shrub="#ffbb22", grassland="#ffff4c", crops="#f096ff",
               urban_built_up="#ff0000", bare="#dcdcdc", water="#1919ff", Overall="#000000")
    if (darken_amount > 0)
    {
        DarkResult = colorspace::darken(Result, darken_amount)
        names(DarkResult) = names(Result)
        Result = DarkResult
    }
    if (prettify)
        names(Result) = PrettifyNames(names(Result))
    return(Result)
}

# Strip the prefixes from feature names to get consistent names for all datasets
RenameFeatures = function(FeatureNames)
{
    KeepPatterns = c("(EVI|NDVI|NBR|TCg|NDSI)_(68|114)_(IQR|median|quant05|quant95|amplitude1|phase1)",
    "(EVI|NDVI|NBR|TCg|NDSI)$",
    "SR_B[1-7]$")
    
    for (Pattern in KeepPatterns)
    {
        FoundSubstrs = stringr::str_extract(FeatureNames, Pattern)
        FeatureNames[!is.na(FoundSubstrs)] = FoundSubstrs[!is.na(FoundSubstrs)]
    }
    return(FeatureNames)
}

# Return a vector of features we want to use for the models,
# given a vector of available feature names.
FilterFeatureNames = function(FeatureNames)
{
    KeepTFeatures = "(EVI|NDVI|NBR|TCg|NDSI)_(68|114)_(IQR|median|quant05|quant95|amplitude1|phase1)"
    KeepVFeatures = "(EVI|NDVI|NBR|TCg|NDSI)$"
    KeepFeatures = c("SR_B4", "SR_B5", "SR_B6",
                     grep(KeepTFeatures, FeatureNames, value = TRUE),
                     grep(KeepVFeatures, FeatureNames, value = TRUE))
    return(KeepFeatures)
}

# Generate a vector of feature names
GetFeatureNames = function()
{
    VIs = c("EVI", "NDVI", "NBR", "TCg", "NDSI")
    Timesteps = 68
    Stats = c("IQR", "median", "quant05", "quant95", "amplitude1", "phase1")
    return(c(paste0("SR_B", 4:6),
             VIs,
             apply(expand.grid(VIs, Timesteps, Stats), 1, paste, collapse="_")))
}
