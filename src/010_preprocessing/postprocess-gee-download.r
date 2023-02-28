# Script to convert the GEE extracted points output CSVs into a compact GeoPackage
library(sf)

InputDir = file.path("..", "..", "data", "DynamicWorld_points", "LC_2015-2019_Validation")
DataFile = file.path("..", "..", "data", "raw", "reference_global_100m_orig&change_year2015-2019_20210407.csv")
OutFile = file.path("..", "..", "data", "WURChange20152019_DynamicWorld_TS.gpkg")
#Bands = c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7") # For Landsat 8
Bands = c("water", "trees", "grass", "flooded_vegetation", "crops", "shrub_and_scrub", "built", "bare", "snow_and_ice") # For Dynamic World
id = "location_id" # "sample_id" for validation data
xycols = c("subpix_mean_x", "subpix_mean_y") # x/y column names in the DataFile, "subpix_mean_x", "subpix_mean_y" for validation
integers = FALSE # Whether the data type is integers (Landsat) or floats (Dynamic World)

# We need the original data to make it spatial so we can put it into a .gpkg
OriginalData = st_read(DataFile, options=c(paste0("X_POSSIBLE_NAMES=", xycols[1]), paste0("Y_POSSIBLE_NAMES=",xycols[2])))
st_crs(OriginalData) = 4326
UniqueData = OriginalData[!duplicated(OriginalData[[id]]),]
# Keep only the coordinates and id
UniqueData = UniqueData[,c(xycols[1], xycols[2], id)]

ListBands = function(Band) list.files(InputDir, pattern = glob2rx(paste0("*", Band, ".csv")), full.names = TRUE)
InputFiles = lapply(Bands, ListBands)

ProcessBand = function(Filenames)
{
    SingleBand = NULL
    for (Filename in Filenames)
    {
        SingleFile = read.csv(Filename, stringsAsFactors=FALSE)
        SingleBand[setdiff(names(SingleFile), names(SingleBand))] <- NA
        SingleFile[setdiff(names(SingleBand), names(SingleFile))] <- NA
        SingleBand = rbind(SingleBand, SingleFile)
    }
    # Remove GEE-specific columns
    SingleBand = SingleBand[,! names(SingleBand) %in% c(".geo", "system.index")]
    # Explicitly use integers
    NumericCols = grep(glob2rx("X????.??.??_*"), names(SingleBand))
    SingleBand[, NumericCols] = if (integers) {
        as.integer(round(as.matrix(SingleBand[,NumericCols])))
    } else {
        # Convert to integers 0-100
        as.integer(round(as.matrix(SingleBand[,NumericCols])*100))
    }
    # Sort columns by date
    SingleBand = SingleBand[,order(names(SingleBand))]
    SpatialBand = merge(UniqueData, SingleBand, by=id)

    BandMatch = which(sapply(Bands, function(Band) length(grep(Band, Filenames[1])) > 0))
    BandName = Bands[BandMatch]

    st_write(SpatialBand, OutFile, layer=BandName, append=TRUE)
}

lapply(InputFiles, ProcessBand)
