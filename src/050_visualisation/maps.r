# Helper to generate QGIS maps
library(sf)
source("../utils/load-sampling-data.r")

ReferenceCSV = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
ReferenceData = read.csv(ReferenceCSV)
ReferenceData = RenameReferenceData(ReferenceData)
ReferenceData = TidyData(ReferenceData)

# Create GPKG out of a data.frame with a location_id column
# The result will be a .gpkg placed next to the input .csv
MakeGPKG = function(CSVFilename, location_col="location_id", overwrite=FALSE)
{
    OutFilename = sub(".csv", ".gpkg", CSVFilename, fixed=TRUE)
    if (file.exists(OutFilename) && !overwrite)
        return(st_read(OutFilename))

    Input = read.csv(CSVFilename)
    
    Output = merge(Input, ReferenceData[c("subpix_mean_x", "subpix_mean_y", "location_id")], by.x=location_col, by.y="location_id")
    OutGPKG = st_as_sf(Output, coords=c("subpix_mean_x", "subpix_mean_y"), dim="XY")
    st_crs(OutGPKG) = 4326
    st_write(OutGPKG, OutFilename)
    return(OutGPKG)
}

MakeGPKG("../../data/reference-trends.csv")
MakeGPKG("../../data/predictions/summer-bfl-m30-trend-scaled-h016-fb/mean-predictions-trends-yearly.csv")
MakeGPKG("../../data/predictions/summer/mean_predictions_scaled-trends-yearly.csv")
MakeGPKG("../../data/predictions/dynamicworld/dynamicworld-trends-yearly.csv")
MakeGPKG("../../data/predictions/dynamicworld-bfl-m30-trend-h016/predictions-trends-yearly.csv")
