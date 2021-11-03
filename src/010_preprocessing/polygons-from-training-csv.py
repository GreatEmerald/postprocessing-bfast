#!/usr/bin/env python3
# Script that takes a CSV file as an input and generates an equivalent GPKG with one 100 m polygon per each data point.

import identify_centroids_LC100_grid
import pandas
import geopandas
import shapely.geometry

# The input CSV should be a valid CSV and have at least the x and y fields (could be either the original coordinates or the centroids).
# All other fields will be preserved and written to the GeoPackage output_file.
input_csv = "../../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv"
#output_file = "../../data/WUR_change_validation_2015-2019.gpkg"
output_csv = "../../data/WUR_change_validation_2015-2019_polygons.csv"

input_df = pandas.read_csv(input_csv)

# Remove all fields with invalid x or y
sub_df = input_df.dropna(axis=0, how="any", subset=["sample_x", "sample_y"])
# Remove all duplicates
sub_df = sub_df.drop_duplicates(subset=["location_id"])

# A function to calculate our bounding box
def GetCentroidInfo(df):
    # Calculate centroids (optional if we already have them, but a bit higher precision)
    df["centroid_x"], df["centroid_y"], df["tile"] = identify_centroids_LC100_grid.getCentroid_in_UTMgrid(df.sample_x, df.sample_y, 100)
    # Get the bbox coordinates as a tuple of tuples:
    df["bbox"] = identify_centroids_LC100_grid.getBoundingBox(df["centroid_x"], df["centroid_y"], 100)
    return(df)

centroid_df = sub_df.apply(GetCentroidInfo, axis=1)

# Check that the result is almost the same as our precalculated centroids
(sub_df["subpix_mean_x"] - centroid_df["centroid_x"]).abs().mean()

# Convert to GeoPandas
crs = 'epsg:4326'
BboxPolys = list(centroid_df.bbox.apply(shapely.geometry.Polygon))
ResultDF = geopandas.GeoDataFrame(centroid_df, geometry=BboxPolys, crs=crs)

# Drop the no-longer-needed bbox field
ResultDF = ResultDF.drop("bbox", axis=1)

# Check output as GPKG
#ResultDF.to_file(output_file, driver="GPKG")

# Write to CSV again, but only tile and id
ResultDF[["tile", "location_id", "geometry"]].to_file(output_csv, driver="CSV", GEOMETRY="AS_WKT")
