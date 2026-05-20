// code to extract 1km climate and soil variables for pulp expansion modelling
// author: jason benedict
// project: pulp deforestation paper
// may 2026

var kalisuma_1km = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/kalisuma_1km_grid");

/**
 * Script to extract variables to 1km grid - Sumatera & Kalimantan
 * Variables: Topography (FABDEM), Climate (TerraClimate), Soil (OpenLandMap & GAEZ)
 */

// topography: FABDEM (30m Bare-Earth DEM)
var fabdem = ee.ImageCollection("projects/sat-io/open-datasets/FABDEM");
var elevation = fabdem.mosaic().setDefaultProjection('EPSG:3857',null,30).rename('elevation');
var slope = ee.Terrain.slope(elevation).rename('slope');

// climate: TerraClimate (1981-2010 Climatology)
var climate = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")
  .filterDate('1981-01-01', '2010-12-31')
  .select(['tmmx', 'tmmn', 'pr', 'pet', 'def'])
  .mean();
  
// scale temperatures (TerraClimate stored as Celsius * 10)
var climateScaled = climate.select(['tmmx', 'tmmn','pet', 'def']).multiply(0.1)
  .addBands(climate.select(['pr']));

// load OpenLandMap Soil Assets (Surface Layer 0cm)
var clay = ee.Image("OpenLandMap/SOL/SOL_CLAY-WFRACTION_USDA-3A1A1A_M/v02").select('b0').rename('clay_content');
var ph = ee.Image("OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02").select('b0').rename('soil_ph').multiply(0.1);
var soilStack = clay.addBands(ph);

// Load the GAEZ image using your asset link and select the first band
var gaez = ee.Image('users/jasonjb82/idn_pulp_defor/aez_v9v2red_CRUTS32_Hist_8110_100_avg').select([0]);

// Map your original classes to grouped numeric IDs
var originalClasses = [2, 3, 6, 27, 28, 25, 26, 32, 33];
var newGroupIDs     = [1, 1, 1,  2,  2,  3,  3,  4,  4]; 

// gaez_cat == 1 ~ "class_noLimitations",
// gaez_cat == 2 ~ "class_hydromorphic",
// gaez_cat == 3 ~ "class_terrain",
// gaez_cat == 4 ~ "class_other",
// gaez_cat == 0 ~ "class_unclassified",

// Remap the raster values
var gaezGrouped = gaez.remap({
  from: originalClasses,
  to: newGroupIDs,
  defaultValue: 0 // Assigns 0 to any unlisted original class
}).rename('gaez_grouped_id');

// define a 'No Data' value
var noData = -9999;

// combine and unmask all layers
var finalStack = elevation.addBands(slope)
  .addBands(climateScaled)
  .addBands(soilStack)
  .addBands(gaezGrouped)
  .unmask(noData);

// spatial extraction
// we use scale: 30 to ensure we don't lose the high-res detail of the FABDEM
// create a coarse grid (e.g., 3-degree blocks) to act as chunks
var lonGrid = ee.List.sequence(95, 120, 3); // longitude range for indonesia
var latGrid = ee.List.sequence(-11, 5, 3);  // latitude range for indonesia

// map over the grid coordinates to create export tasks
lonGrid.evaluate(function(lons) {
  latGrid.evaluate(function(lats) {
    lons.map(function(lon) {
      lats.map(function(lat) {
        
        // define the bounding box for this chunk
        var chunkRegion = ee.Geometry.Rectangle([lon, lat, lon + 3, lat + 3]);
        
        // filter your points to only those within this box
        var pointsSubset = kalisuma_1km.filterBounds(chunkRegion);
        
        // only export if the chunk actually contains points
        var count = pointsSubset.size();
        
        // GEE Client-side check to see if we should trigger an export
        count.evaluate(function(c) {
          if (c > 0) {
            var sampled = finalStack.sampleRegions({
              collection: pointsSubset,
              scale: 30,
              geometries: false // speed boost: set to false if you have pixel_id
            });

            // ==========================================================
            // Handle nulls (out-of-bounds points) and output as numerics
            // ==========================================================
            var sampledCleaned = sampled.map(function(feature) {
              // get the numeric ID (might be null if totally off the map)
              var val = feature.get('gaez_grouped_id');
  
              // Safely check if it is null using GEE server-side logic. 
              // if null, force it to -9999. Otherwise, keep the numeric value.
              var safeVal = ee.Algorithms.If(
                ee.Algorithms.IsEqual(val, null),
                -9999,
                val
              );
  
              // set it to 'gaez_cat' to match your preferred CSV column name
              return feature.set('gaez_cat', safeVal);
            });
            // ==========================================================

            Export.table.toDrive({
              collection: sampledCleaned, // Exporting the null-cleaned collection
              folder: 'GEE_indo_pulp',
              description: 'kalisuma_topo_cli_soil_gaez_1km_extract_lon' + lon + '_lat' + lat,
              fileFormat: 'CSV',
              selectors: [
                'pixel_id', 
                'elevation', 
                'slope', 
                'tmmx', 
                'tmmn',
                'pr', 
                'pet', 
                'def', 
                'clay_content', 
                'soil_ph',
                'gaez_cat' // Outputs 1, 2, 3, 4, 0, or -9999
              ]
            });
          }
        });
      });
    });
  });
});
  
// UI: add layers to map for a quick visual check
Map.centerObject(kalisuma_1km, 6);
Map.addLayer(elevation, {min: 0, max: 5000, palette: ['green', 'yellow', 'red']}, 'Elevation');
Map.addLayer(slope, {min: 0, max: 50, palette: ['green', 'yellow', 'red']}, 'Slope Visualization');
Map.addLayer(gaezGrouped, {min: 0, max: 4, palette: ['grey', 'green', 'blue', 'orange', 'red']}, 'GAEZ Grouped Classes');
Map.addLayer(kalisuma_1km.style({fillColor: '00000000', color: 'blue'}), {}, '1km Grid Overlay');