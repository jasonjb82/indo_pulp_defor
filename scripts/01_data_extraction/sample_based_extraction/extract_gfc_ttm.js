// Extracting TTM modified classes  ------

// load data
var kab = ee.FeatureCollection("projects/trase/STORAGE/ID/BOUNDARIES/id_kabupaten"),
    suma_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SUMATERA_samples_hti"),
    balinusa_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/BALINUSA_samples_hti"),
    kali_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/KALIMANTAN_samples_hti"),
    malu_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/MALUKU_samples_hti"),
    papua_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/PAPUA_samples_hti"),
    sula_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SULAWESI_samples_hti"),
    gfc_ttm = ee.Image("users/jasonjb82/idn_pulp_defor/REGBRNIDNMYS_FC_2001to2022_TTM_20230523_Class1to6");

// define samples
var samples = kali_samples

Map.addLayer(gfc_ttm, {} ,"GFC TreeMap", false);

var functions = require('users/trasegis/tools:functions');

var proj = ee.Projection(
    'PROJCS["World_Sinusoidal",'+
    'GEOGCS["GCS_WGS_1984",'+
    '    DATUM["WGS_1984",'+
    '        SPHEROID["WGS_1984",6378137,298.257223563]],'+
    '    PRIMEM["Greenwich",0],'+
    '    UNIT["Degree",0.017453292519943295]],'+
    'PROJECTION["Sinusoidal"],'+
    'PARAMETER["False_Easting",0],'+
    'PARAMETER["False_Northing",0],'+
    'PARAMETER["Central_Meridian",0],'+
    'UNIT["Meter",1],'+
    'AUTHORITY["EPSG","54008"]]');

Map.setCenter(108.802, 0.956,5);
Map.addLayer(samples,({color: 'red', pointRadius: 0.5}), 'samples')

// reproject
var gfc_ttm_prj = gfc_ttm.reproject({crs: proj,scale: 30});


// select and rename band
var gfc_ttm_prj = gfc_ttm_prj.select(
    ['b1'], // old name
    ['gfc_ttm']  // new name
);

// extract jrc tmf annual change classes for all years
var values = samples.map(function(feature) {
  return feature.set(gfc_ttm_prj.reduceRegion({
    reducer: 'mode',
    geometry: feature.geometry(),
  }));
});

// drop .geo column (not needed if goal is tabular data)
var featuresOut = values.select(['.*'],null,false);

// get required columns
var col_names = featuresOut.first().propertyNames().getInfo() 
print(col_names);
var new_cols = col_names.splice(0,2).sort().reverse();
print(new_cols)

// table to googledrive export
Export.table.toDrive({
  collection: featuresOut,
  selectors: new_cols,
  description: 'kali_gfc_ttm',
  folder: 'GEE_indo_pulp', // set output layer on googledrive
  fileFormat: 'CSV'
}); 