/*
  
Assets description:
    gaveau_lulc: land use and cover - multi band raster, each band one year, classes described in the metadata
    industrial_plantations: wood pulp concessions 
    
Result:
    csv with the area of timber plantations for pulp (ha)

Notes:
    For indonesia we use sinusoidal projection
    
*/

// --- load data ----- //
var image = ee.Image("users/trasegis/STORAGE/ID/LULC/gaveau_timber_2023"),
    hti = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/IUPHHK_HTI_20230314_proj");

// --- load function --- //
var functions = require('users/trasegis/tools:functions');

var gaveau_lulc = gaveau_lulc.eq(3);

// --- rename bands --- //
var originalNames = gaveau_lulc.bandNames();
var newNames = originalNames.map(function(name) {
  return ee.String(name).replace('timberdeforestation_', 'pulp_');
});
gaveau_lulc = gaveau_lulc.rename(newNames);

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

gaveau_lulc = gaveau_lulc.reproject({crs: proj, scale: 30});
var gaveau_pulp_hti = functions.region_reducer(gaveau_lulc, hti);

// export the results as a CSV file
Export.table.toDrive({
    collection: gaveau_pulp_hti,
    description: 'pulp_annual_area_hti',
    folder: 'GEE_indo_pulp',
    fileFormat: 'CSV',
});