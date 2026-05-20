/*
  
Assets description: 

    
Result:
        - Annual deforestation in Indonesia from forest and non-forest areas

Notes:
        - Deforestation in the same year as reported by gaveau
    
*/

/* Import Functions */
var functions = require('users/trasegis/tools:functions');

/* import data */
var idn_adm = ee.FeatureCollection("users/jasonjb82/trase_idn/idn_adm_kabupaten"),
    gaveau = ee.Image("users/trasegis/STORAGE/ID/LULC/gaveau_timber_2023");

/* transforming the variables */
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


// Calculating using Gaveau only
var annual_def_f = ee.Image(0);
for (var yr= 2001; yr <= 2022; yr = yr + 1) {
  var step1 = gaveau.select('timberdeforestation_'+yr).eq(3) // to timber
                         .and(gaveau.select('timberdeforestation_'+(yr-1)).eq(1));  // from forest
  annual_def_f = annual_def_f.addBands(step1.rename('pulp_'+yr));
}

var annual_def_nf = ee.Image(0);
for (var yr= 2001; yr <= 2022; yr = yr + 1) {
  var step1 = gaveau.select('timberdeforestation_'+yr).eq(3) // to timber
                         .and(gaveau.select('timberdeforestation_'+(yr-1)).eq(2));  // from non-forest
  annual_def_nf = annual_def_nf.addBands(step1.rename('pulp_'+yr));
}

// export the results as a CSV file
functions.export_table(
     functions.region_reducer(annual_def_f.reproject({crs: proj,scale: 30}), idn_adm),
               'pulp_annual_defor_forest_indonesia',
               'GEE_outputs');


// export the results as a CSV file
functions.export_table(
     functions.region_reducer(annual_def_nf.reproject({crs: proj,scale: 30}), idn_adm),
               'pulp_annual_defor_non-forest_indonesia',
               'GEE_outputs');

