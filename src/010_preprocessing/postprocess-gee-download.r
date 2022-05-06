# Script to convert the GEE extracted points output CSVs into a compact GeoPackage
library(sf)

InputDir = file.path("..", "..", "data", "GEE_extracted_points")
DataFile = file.path("..", "..", "data", "raw", "refdata_world_africa_included_locations_data20190709.csv")
OutFile = file.path("..", "..", "data", "WURValidation2015_Landsat8_TS.gpkg")
Bands = c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")
id = "sample_id"

# We need the original data to make it spatial so we can put it into a .gpkg
OriginalData = st_read(DataFile, options=c("X_POSSIBLE_NAMES=subpix_mean_x", "Y_POSSIBLE_NAMES=subpix_mean_y"))
UniqueData = OriginalData[!duplicated(OriginalData[[id]]),]
# Keep only the coordinates and id
UniqueData = UniqueData[,c("subpix_mean_x", "subpix_mean_y", id)]

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
    SingleBand[, NumericCols] = as.integer(round(as.matrix(SingleBand[,NumericCols])))
    # Sort columns by date
    SingleBand = SingleBand[,order(names(SingleBand))]
    SpatialBand = merge(UniqueData, SingleBand, by=id)

    BandMatch = which(sapply(Bands, function(Band) length(grep(Band, Filenames[1])) > 0))
    BandName = Bands[BandMatch]

    st_write(SpatialBand, OutFile, layer=BandName, append=TRUE)
}

lapply(InputFiles, ProcessBand)
