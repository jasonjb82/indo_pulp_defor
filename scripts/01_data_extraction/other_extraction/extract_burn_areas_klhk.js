/*

Extracting burned areas by grid points

Input: Annual burned areas from KLHK

*/

var ba = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/ba_2015_2022"),
    papua_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/PAPUA_samples_hti"),
    suma_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SUMATERA_samples_hti"),
    kali_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/KALIMANTAN_samples_hti"),
    sula_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SULAWESI_samples_hti"),
    balinusa_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/BALINUSA_samples_hti");

// define samples
var samples = kali_samples

// get columns
var burn_areas = ba.first().propertyNames().sort().slice(1,9)

// create image collection and rename columns and values
var col = ee.ImageCollection(burn_areas.map(function(property) {
  return ba.select([property])
    .remap([2015,2016,2017,2018,2019,2020,2021,2022], [1,1,1,1,1,1,1,1],property)
    .reduceToImage([property], ee.Reducer.first())
    .set('Year', ee.String(property).replace('yr_',''))
    .set('system:time_start', ee.Date('2000-01-01').millis());
 }));

// convert to bands and rename
var bands = col.toBands().regexpRename('(.*)_first', '$1');

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

// add a loop here later - but naah it is only 7 bands
var imgcol = ee.Image.cat(bands.select('0').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('1').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('2').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('3').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('4').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('5').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('6').toInt().unmask().reproject({crs: proj,scale: 30}),
                          bands.select('7').toInt().unmask().reproject({crs: proj,scale: 30}))
  .set('system:time_start', ee.Date('2000-01-01').millis());
var imgcol = ee.ImageCollection([imgcol]);


var functions = require('users/trasegis/tools:functions');

var params = {
  bands: [0, 1, 2, 3, 4, 5, 6, 7],
  bandsRename: ['2015', '2016', '2017', '2018', '2019','2020','2021','2022']
};


var chunk = 100000;
var collectionSize = 5316660

for (var i = 0; i<collectionSize;i=i+chunk){
  var subset = ee.FeatureCollection(suma_samples.toList(chunk, i));
  var ptsTopoStats = functions.zonalStats(imgcol, subset, params);
  Export.table.toDrive({
    collection: ptsTopoStats,
    folder: 'GEE_indo_pulp',
    selectors: ['sid','2015','2016','2017','2018','2019','2020','2021','2022'],
    description: 'kali_samples_burn_areas'+'_chunk_'+i,
    fileFormat: 'CSV'
  })
}
