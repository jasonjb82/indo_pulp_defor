// code to extract 1km climate and soil variables for pulp expansion modelling
// author: jason benedict
// project: pulp deforestation paper
// may 2026

var cm_dist_2017 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/dist_cm_2017"),
    cm_dist_2022 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/dist_cm_2022"),
    kalisuma_1km = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/kalisuma_1km_grid");

// convert the 10km points into a Coarse Image
// We use a 10km scale (10000m) to match your input grid spacing

// create 2017 surface
var distImage2017 = cm_dist_2017.reduceToImage({
  properties: ['dist_km'],
  reducer: ee.Reducer.first()
}).resample('bilinear').reproject({
  crs: 'EPSG:3857',
  scale: 1000
}).rename('dist_mill_2017');

// create 2022 surface
var distImage2022 = cm_dist_2022.reduceToImage({
  properties: ['dist_km'],
  reducer: ee.Reducer.first()
}).resample('bilinear').reproject({
  crs: 'EPSG:3857',
  scale: 1000
}).rename('dist_mill_2022');

// combine into a 2-band stack
var combinedDist = distImage2017.addBands(distImage2022);

// define 3-degree chunks
var lonGrid = ee.List.sequence(95, 120, 3); 
var latGrid = ee.List.sequence(-11, 6, 3);  

// nested loops to create the export tasks
lonGrid.evaluate(function(lons) {
  latGrid.evaluate(function(lats) {
    lons.map(function(lon) {
      lats.map(function(lat) {
        
        var chunkRegion = ee.Geometry.Rectangle([lon, lat, lon + 3, lat + 3]);
        
        // filter your 1km points to this specific 1-degree box
        var pointsSubset = kalisuma_1km.filterBounds(chunkRegion);
        
        pointsSubset.size().evaluate(function(count) {
          if (count > 0) {
            
            // sample the interpolated surface
            var sampled = combinedDist.sampleRegions({
              collection: pointsSubset,
              scale: 1000,
              tileScale: 16,     // maximize parallel processing
              geometries: false  // keep memory low
            });

            Export.table.toDrive({
              collection: sampled,
              folder: 'GEE_indo_pulp',
              description: 'mill_dist_1km_lon' + lon + '_lat' + lat,
              fileFormat: 'CSV',
              selectors: ['pixel_id', 'dist_mill_2017','dist_mill_2022']
            });
          }
        });
      });
    });
  });
});

// visualize to check the "smoothness"
Map.centerObject(kalisuma_1km, 6);
Map.addLayer(distImage2017, {min: 0, max: 1000, palette: ['white', 'yellow' ,'orange', 'red']}, '2017 Interpolated Dist');
Map.addLayer(cm_dist_2017.style({pointSize: 3, color: 'black'}), {}, 'original 10km Nodes');
