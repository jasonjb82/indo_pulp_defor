// code to extract 1km landuse variables for pulp expansion modelling
// author: jason benedict
// project: pulp deforestation paper
// may 2026

var kalisuma_1km = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/kalisuma_1km_grid"),
    river = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/IDN_rivers_250k");

// rasterize the rivers
// create binary raster - 1 where there is a river, 0 otherwise.
var riverImage = river.map(function(f) { return f.set('constant', 1); })
  .reduceToImage({
    properties: ['constant'],
    reducer: ee.Reducer.first()
  });

// calculate sistance (in meters)
// fastDistanceTransform calculates the distance to the nearest non-zero pixel.
var distanceToRiver = riverImage
  .fastDistanceTransform({
    neighborhood: ee.Number(50000 / 30).round(), //
    units: 'pixels',
    metric: 'squared_euclidean'
  })
  .sqrt() // fastDistanceTransform usually returns squared distance for this metric
  .multiply(30) // multiply by your pixel size (30m) to get meters
  .rename('dist_water_m');

// sample the distance at your Grid Points
var gridWithDist = distanceToRiver.sampleRegions({
  collection: kalisuma_1km, 
  scale: 30,    
  geometries: true
});

// spatial extraction
// we use scale: 30 to ensure we don't lose the high-res detail of the FABDEM
// create a coarse grid (e.g., 1-degree blocks) to act as chunks
var lonGrid = ee.List.sequence(95, 120, 3); // longitude range for indonesia
var latGrid = ee.List.sequence(-11, 5, 3);  // latitude range for indonesia

// map over the grid coordinates to create export tasks
lonGrid.evaluate(function(lons) {
  latGrid.evaluate(function(lats) {
    lons.map(function(lon) {
      lats.map(function(lat) {
        
        // define the bounding box for this chunk
        var chunkRegion = ee.Geometry.Rectangle([lon, lat, lon + 3, lat + 3])
                            .buffer(100).bounds();
        
        // filter your points to only those within this box
        var pointsSubset = kalisuma_1km.filterBounds(chunkRegion);
        
        // only export if the chunk actually contains points
        var count = pointsSubset.size();
        
        // GEE Client-side check to see if we should trigger an export
        count.evaluate(function(c) {
          if (c > 0) {
           var sampled = distanceToRiver.sampleRegions({
              collection: pointsSubset,
              scale: 30,
              tileScale: 4,     // maximize parallel processing
              geometries: false  // keep memory low
            });
            
            Export.table.toDrive({
              collection: sampled,
              folder: 'GEE_indo_pulp',
              description: 'kalisuma_riverdist_1km_lon' + lon + '_lat' + lat,
              fileFormat: 'CSV',
              selectors: [
                'pixel_id',
                'dist_water_m']             
            });
          }
        });
      });
    });
  });
});

// define a palette: blue for water, transitioning to red for "far away"
var distanceVis = {
  min: 0,
  max: 20000, // Stretch the colors over 20km
  palette: ['#0000ff', '#00ffff', '#ffff00', '#ff0000']
};

// add to map
Map.addLayer(distanceToRiver, distanceVis, 'distance to rivers (20km stretch)');

// Also add your original rivers to check for accuracy
Map.addLayer(river, {color: 'black'}, 'Original River Lines');

