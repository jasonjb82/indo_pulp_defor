// Extracting TreeMap landuse changes dataset values ------

// class values
// 1 - Forest
// 2 - Non Forest
// 3 - Industrial Timber Plantation

// load data
var image = ee.Image("users/trasegis/STORAGE/ID/LULC/gaveau_timber_2023"),
    papua_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/PAPUA_samples_hti"),
    kali_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/KALIMANTAN_samples_hti"),
    sula_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SULAWESI_samples_hti"),
    suma_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SUMATERA_samples_hti"),
    malu_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/MALUKU_samples_hti"),
    balinusa_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/BALINUSA_samples_hti");

// define samples
var samples = malu_samples

var image_byte = image.toInt().unmask();

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

// reproject
var gav_lulc = image_byte.reproject({crs: proj,scale: 30});

Map.setCenter(108.802, 0.956,5);
Map.addLayer(image_byte,({min:1,max:3}), 'gaveau')
Map.addLayer(samples,({color: 'red', pointRadius: 0.5}), 'samples')


// extract gaveau land use classes for all years
var values = samples.map(function(feature) {
  return feature.set(gav_lulc.reduceRegion({
    reducer: 'mode',
    geometry: feature.geometry(),
  }));
});
// drop .geo column (not needed if goal is tabular data)
var featuresOut = values.select(['.*'],null,false);

var col_names = featuresOut.first().propertyNames().getInfo() 
print(col_names);
var new_cols = col_names.splice(0,24).sort();
print(new_cols)

// table to googledrive export
Export.table.toDrive({
  collection: featuresOut,
  selectors: new_cols,
  description: 'malu_samples_gaveau_classes',
  folder: 'GEE_indo_pulp', // set output layer on googledrive
  fileFormat: 'CSV'
}); 