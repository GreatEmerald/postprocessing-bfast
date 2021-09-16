# Remove duplicate entries and anonymise for upload to GEE

library(sf)

InputFile = "/mnt/raid/dainius/cglops/cglops-change-detection/data/Polygons_Global.gpkg"
OutputFile = "/mnt/raid/dainius/cglops/cglops-change-detection/data/Polygons_Global_GEE.csv"

InputDB = st_read(InputFile)

CleanData = InputDB[!duplicated(InputDB[["sample_id"]]), c("sample_id", "tile")]
# Unique values only, 4x reduction in size
nrow(InputDB) == nrow(CleanData) * 4

st_write(CleanData, OutputFile, layer_options="GEOMETRY=AS_WKT", delete_dsn=TRUE)
