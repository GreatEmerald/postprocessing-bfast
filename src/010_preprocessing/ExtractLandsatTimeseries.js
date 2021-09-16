// For updates, see https://code.earthengine.google.com/?accept_repo=users/GreatEmerald/cglops-break-detection

var SampleLocations = ee.FeatureCollection("users/GreatEmerald/CGLOPSChangeTrainingData-2020-100m-v2"),
    aoi = 
    /* color: #98ff00 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-120.12569675085112, 36.989861107702794],
          [-120.12569675085112, 36.50790174514166],
          [-119.38686618444487, 36.50790174514166],
          [-119.38686618444487, 36.989861107702794]]], null, false);

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    
    Copyright 2021 Dainius Masiliunas
*/

// Extract time series of Landsat 8 over CGLOPS polygons.
// Aggregates to the 100 m polygons and merges observations on the same day.
//aoi = LandsatPaths.filter(ee.Filter.eq("ROW", 3)).geometry();

// BUG: The above seems to create non-unique locations (repeats). Better to use UTM zones.
// (i.e. filter points by the first two digits of "tile")

var UTMZone = "Global";

var MySubset = SampleLocations; // This runs the code globally (which works!).
// If you want a subset, uncomment one of:
//.filterMetadata("tile", "starts_with", UTMZone);
//filterBounds(aoi); // AOI is a polygon covering one or more Landsat tile(s)

var ExportBands = ["SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7", "ST_B10"];

var StartDate = '2013-03-18';
var EndDate = '2021-08-15';

// Define dates
var dataset = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
    .filterDate(StartDate, EndDate).filterBounds(MySubset)
    .sort("system:time_start"); // Sort by Date

// Mask clouds
var qa = dataset.select("QA_PIXEL");


// Merge all images taken on the same date

/* A less efficient implementation
function mosaicByDate(imcol){
  // imcol: An image collection
  // returns: An image collection
  var imlist = imcol.toList(imcol.size());

  var unique_dates = imlist.map(function(im){
    return ee.Image(im).date().format("YYYY-MM-dd");
  }).distinct();

  var mosaic_imlist = unique_dates.map(function(d){
    d = ee.Date(d);

    var im = imcol
      .filterDate(d, d.advance(1, "day"))
      .mosaic();

    return im.set(
        "system:time_start", d.millis(), 
        "system:id", d.format("YYYY-MM-dd"),
        "system:index", d.format("YYYY-MM-dd"));
  });

  return ee.ImageCollection(mosaic_imlist);
}*/

// A more efficient implementation
function temporalCollection(collection, start, count, interval, units) {
  // Create a sequence of numbers, one for each time interval.
  var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));

  var originalStartDate = ee.Date(start);

  return ee.ImageCollection(sequence.map(function(i) {
    // Get the start date of the current sequence.
    var startDate = originalStartDate.advance(ee.Number(interval).multiply(i), units);

    // Get the end date of the current sequence.
    var endDate = originalStartDate.advance(
      ee.Number(interval).multiply(ee.Number(i).add(1)), units);

    return collection.filterDate(startDate, endDate).mean()
        .set('system:time_start', startDate.millis(),
             'system:time_end', endDate.millis(),
             "system:index", startDate.format("YYYY-MM-dd"),
             "system:id", startDate.format("YYYY-MM-dd"));
  }));
}

function DropNullImages(collection) {
  collection = collection.map(function (image){
    return ee.Image(image.set('bandCount',image.bandNames().size()));
  });
  
  return collection.filterMetadata("bandCount","not_equals",0);
}

// Debug visualisation START

var visualizationBand = {
  bands: ["SR_B4", "SR_B3", "SR_B2"],
  min: 7000,
  max: 15000,
};

var visualizationQA = {
  min: 0,
  max: 30000,
};

Map.centerObject(aoi);

Map.addLayer(qa, visualizationQA, "Landsat QA");
//Map.addLayer(cloudfree, visualizationBand, "Landsat band");
Map.addLayer(aoi, null, "Area of interest");

// Debug visualisation END

// Export bands one by one
function ExtractFromBand(bandname) {
  var cloudfree = dataset.map(function(image) {
    var initial = image.select(bandname).toInt16().mask(image.select('QA_PIXEL') // NB: only for bands that are Int16!
      .remap([5440,21824,21888,21952,22080,22144,30048,54596,54852],[1,1,1,1,1,1,1,1,1],0)); // 5440,21824=clear -> 1 = not masked
    return initial.copyProperties(initial,ee.List(['system:time_start']));
  });
  
  // Make a 16-day mean composite. Yes, Landsat is already 16-day,
  // but sometimes the days cross over, which results in a table of mostly NAs.
  // Force the result to have 192 columns.
  
  cloudfree = temporalCollection(cloudfree, StartDate, 192, 16, 'day');//mosaicByDate(cloudfree);
  cloudfree = DropNullImages(cloudfree);
  
  var point = cloudfree.toBands()
    .reduceRegions(MySubset, ee.Reducer.mean(), 30);
  //print(point);
  Export.table.toDrive(point, UTMZone + "_" + bandname, "GEE_extracted_points");
}
ExportBands.map(ExtractFromBand);

// */
