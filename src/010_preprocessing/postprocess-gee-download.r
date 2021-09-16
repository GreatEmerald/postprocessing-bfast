# Script to convert the GEE extracted points output CSVs into a compact GeoPackage
library(sf)

InputDir = file.path("..", "data", "GEE_extracted_points")
DataFile = file.path("..", "data", "Data_Global_quoted.csv")
OutFile = file.path("..", "data", "IIASAChange20152018_Landsat8_TS.gpkg")

InputFiles = list.files(InputDir, full.names = TRUE)
# We need the original data to make it spatial so we can put it into a .gpkg
OriginalData = st_read(DataFile, options=c("X_POSSIBLE_NAMES=centroid_x", "Y_POSSIBLE_NAMES=centroid_y"))
UniqueData = OriginalData[!duplicated(OriginalData[["sample_id"]]),]
# Keep only the coordinates and id
UniqueData = UniqueData[,c("centroid_x", "centroid_y", "sample_id")]

ProcessBand = function(Filename)
{
    SingleBand = read.csv(Filename, stringsAsFactors=FALSE)
    # Remove GEE-specific columns
    SingleBand = SingleBand[,! names(SingleBand) %in% c(".geo", "system.index")]
    # Explicitly use integers
    SingleBand[,-c(ncol(SingleBand), ncol(SingleBand)-1)] = as.integer(round(as.matrix(SingleBand[,-c(ncol(SingleBand), ncol(SingleBand)-1)])))
    SpatialBand = merge(UniqueData, SingleBand, by="sample_id")

    BandName = unlist(strsplit(basename(Filename), ".", fixed=TRUE))
    BandName = paste0(BandName[-length(BandName)], collapse=".")

    st_write(SpatialBand, OutFile, layer=BandName, append=TRUE)
}

lapply(InputFiles, ProcessBand)
