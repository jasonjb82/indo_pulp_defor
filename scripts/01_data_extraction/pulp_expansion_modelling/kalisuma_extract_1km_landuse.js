// code to extract 1km landuse variables for pulp expansion modelling
// author: jason benedict
// project: pulp deforestation paper
// may 2026

var gaveau = ee.Image("users/trasegis/STORAGE/ID/LULC/gaveau_timber_2023"),
    kalisuma_10km = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/kali_suma_10km_grid"),
    forest = ee.Image("users/jasonjb82/idn_pulp_defor/REGBRNIDNMYS_FC_2001to2022_TTM_20230523_Class1to6"),
    peat = ee.FeatureCollection("projects/trase/STORAGE/ID/LULC/GAVEAU_PALMOIL_WOODPULP/gambut_indonesia_2019"),
    op_conc = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/op_concessions"),
    hti = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/id_industrial_woodpulp_plantations"),
    kh_2022 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/kh_2022"),
    hti_2016 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/hti_2016"),
    hti_2022 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/hti_2022"),
    wdpa = ee.FeatureCollection("WCMC/WDPA/current/polygons"),
    iopp_gav24 = ee.Image("projects/trase/STORAGE/ID/LULC/GAVEAU_PALMOIL_WOODPULP/gaveau_iopp_expansion_2024"),
    kalisuma_1km = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/kalisuma_1km_grid"),
    itp_gav24 = ee.Image("projects/trase/STORAGE/ID/LULC/GAVEAU_PALMOIL_WOODPULP/gaveau_itp_expansion_2024"),
    conc_2017 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/conc_rr_2017"),
    conc_2022 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/conc_rr_2022");
	

// palm oil ----------------------------
// setup Parameters
var start = 2001, end = 2024;     
var startOut = 2004, endOut = 2024;  

// create the annual expansion stack
var op_exp = ee.Image(0).select([]);  

for (var year = start; year <= end; year++) {
  var yy = year - 2000;
  // Combine all the specific land cover class transitions
  var mask = iopp_gav24.eq(2100 + yy)
    .or(iopp_gav24.eq(2400 + yy))
    .or(iopp_gav24.eq(2600 + yy))
    .or(iopp_gav24.eq(4300 + yy))
    .or(iopp_gav24.eq(4500 + yy))
    .rename('op_exp_' + year);
    
  op_exp = op_exp.addBands(mask);
}

// calculate cumulative palm clearing
// start with the 2000 baseline
var iopp2000 = iopp_gav24.eq(4000).unmask(0);
var palmStack = iopp2000.rename('palm_2000'); 
var cum = iopp2000;  

for (var y = 2001; y <= 2024; y++) {
  var yy = y - 2000;
  var annualExp = iopp_gav24.eq(2100 + yy)
    .or(iopp_gav24.eq(2400 + yy))
    .or(iopp_gav24.eq(2600 + yy))
    .or(iopp_gav24.eq(4300 + yy))
    .or(iopp_gav24.eq(4500 + yy))
    .unmask(0);
    
  cum = cum.or(annualExp);
  if (y == 2017 || y == 2022) {
    palmStack = palmStack.addBands(cum.rename('palm_' + y));
  }
}

// wood pulp ----------------------------
// setup Parameters
var start = 2001, end = 2024;     
var startOut = 2004, endOut = 2024;  

// create the annual expansion stack
var itp_exp = ee.Image(0).select([]);  

for (var year = start; year <= end; year++) {
  var yy = year - 2000;
  // Combine all the specific land cover class transitions
  var mask = itp_gav24.eq(1100 + yy)
    .or(itp_gav24.eq(1400 + yy))
    .or(itp_gav24.eq(1600 + yy))
    .or(itp_gav24.eq(3300 + yy))
    .rename('itp_exp_' + year);
    
  itp_exp = itp_exp.addBands(mask);
}

// calculate cumulative pulp clearing
// start with the 2000 baseline
var itp2000 = itp_gav24.eq(3000).or(itp_gav24.eq(3016)).unmask(0);
var pulpStack = itp2000.rename('pulp_2000'); 
var cum = itp2000;  

for (var y = 2001; y <= 2024; y++) {
  var yy = y - 2000;
  var annualExp = itp_gav24.eq(1100 + yy)
    .or(itp_gav24.eq(1400 + yy))
    .or(itp_gav24.eq(1600 + yy))
    .or(itp_gav24.eq(3300 + yy))
    .unmask(0);
    
  cum = cum.or(annualExp);
  if (y == 2017 || y == 2022) {
    pulpStack = pulpStack.addBands(cum.rename('pulp_' + y));
  }
}

//Map.addLayer(itp2000,{},"pulp 2000")

/**
 * define forest logic function
 * Forest is present if:
 * - It is "remaining forest" (100, 400, 600)
 * - OR it is "future loss" (Loss year > target year)
 */
var noData = -9999;

// improved forest logic
var getForestMask = function(year) {
  var yrShort = year - 2000;
  // Unmask individual components to 0 so the .or() logic doesn't fail
  var remaining = forest.eq(100).or(forest.eq(400)).or(forest.eq(600)).unmask(0);
  var mineralLoss = forest.gt(100 + yrShort).and(forest.lt(200)).unmask(0);
  var mangroveLoss = forest.gt(400 + yrShort).and(forest.lt(500)).unmask(0);
  var peatLoss = forest.gt(600 + yrShort).and(forest.lt(700)).unmask(0);
  
  return remaining.or(mineralLoss).or(mangroveLoss).or(peatLoss)
    .rename('forest_' + year);
};

var forest2017 = getForestMask(2017);
var forest2022 = getForestMask(2022);


// peat -----------------------------------------
var peatImage = peat.map(function(f) { return f.set('constant', 1); })
  .reduceToImage({
    properties: ['constant'],
    reducer: ee.Reducer.first()
  }).unmask(0).rename('peat');
  
// po concession----------------------------------
var opImage = op_conc.map(function(f) { return f.set('constant', 1); })
  .reduceToImage({
    properties: ['constant'],
    reducer: ee.Reducer.first()
  }).unmask(0).rename('op_conc');

// hti (2016) -------------------------------------
//var htiImage2016 = hti_2016.map(function(f) { return f.set('constant', 1); })
//  .reduceToImage({
//    properties: ['constant'],
//    reducer: ee.Reducer.first()
//  }).unmask(0).rename('hti_2016');
  
var htiImage2016 = conc_2017
  // Drop the .map() step and go straight to rasterizing
  .reduceToImage({
    properties: ['risk_code'],    // Point directly to your existing column
    reducer: ee.Reducer.first()   // Take the first overlapping value
  })
  .unmask(0)                      // Keep 0 for areas with no data (no risk)
  .rename('hti_risk_2017');       // Renamed slightly to reflect the new data
  
// hti (2022) -------------------------------------
//var htiImage2022 = hti_2022.map(function(f) { return f.set('constant', 1); })
//  .reduceToImage({
//    properties: ['constant'],
//    reducer: ee.Reducer.first()
//  }).unmask(0).rename('hti_2022');
  
var htiImage2022 = conc_2022
  // Drop the .map() step and go straight to rasterizing
  .reduceToImage({
    properties: ['risk_code'],    // Point directly to your existing column
    reducer: ee.Reducer.first()   // Take the first overlapping value
  })
  .unmask(0)                  // Keep 0 for areas with no data (no risk)
  .rename('hti_risk_2022');       // Renamed slightly to reflect the new data
  
// filter for indonesia and officially designated areas
var idnProtected = wdpa
  .filter(ee.Filter.eq('ISO3', 'IDN'))
  .filter(ee.Filter.eq('STATUS','Designated'))
  .filter(ee.Filter.eq('REALM','Terrestrial'))
  //.filter(ee.Filter.eq('MARINE', '0'));
  
// wdpa --------------------------------------------
var wdpaImage = idnProtected
  .map(function(f) { return f.set('constant', 1); })
  .reduceToImage({
    properties: ['constant'],
    reducer: ee.Reducer.first()
  })
  .unmask(0).rename('wdpa');
  
// combine all bands --------------------------------
var finalStack = ee.Image.constant(noData).rename('base_drop')
  .addBands([
    pulpStack.select(['pulp_2017', 'pulp_2022']),
    forest2017, forest2022, 
    palmStack.select(['palm_2017', 'palm_2022']),
    peatImage, opImage, htiImage2016, htiImage2022, wdpaImage
  ])
  .unmask(noData); 

var sampledGrid = finalStack.reduceRegions({
  collection: kalisuma_10km,
  reducer: ee.Reducer.mode().unweighted(),
  scale: 30,
  tileScale: 4
});

// spatial extraction
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
            var sampled = finalStack.reduceRegions({
              collection: pointsSubset,
              reducer: ee.Reducer.mode().unweighted(),
              scale: 30,
              tileScale: 4
              //geometries: false // speed boost: set to false if you have pixel_id
            });
            
            Export.table.toDrive({
              collection: sampled,
              folder: 'GEE_indo_pulp',
              description: 'kalisuma_landuse_1km_lon' + lon + '_lat' + lat,
              fileFormat: 'CSV',
              selectors: [
                'pixel_id',
                'pulp_2017',
                'pulp_2022',
                'palm_2017',
                'palm_2022',
                'forest_2017',
                'forest_2022',
                'peat',
                'op_conc',
                'hti_risk_2017',
                'hti_risk_2022',
                'wdpa'
              ]
            });
          }
        });
      });
    });
  });
});
  
print(finalStack);

// visual check
Map.centerObject(kalisuma_10km, 6);
Map.addLayer(palmStack.select("palm_2022").selfMask(), {palette: ['purple']}, 'Palm oil 2022');
Map.addLayer(palmStack.select("palm_2017").selfMask(), {palette: ['grey']}, 'Palm oil 2017');
Map.addLayer(pulpStack.select("pulp_2022").selfMask(), {palette: ['red']}, 'Pulp 2022');
Map.addLayer(pulpStack.select("pulp_2017").selfMask(), {palette: ['blue']}, 'Pulp 2017');
Map.addLayer(forest2022.selfMask(), {palette: ['darkgreen']}, 'Forest 2022');
Map.addLayer(idnProtected, {color: 'green'}, 'WDPA Protected Areas');

