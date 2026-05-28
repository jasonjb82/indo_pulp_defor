/*
  
Assets description: 

Result:
        - Annual expansion of pulp on peatlands in Indonesia

Notes:
        - Expansion on forest & non-forest the same year as reported by MoA (2019)
    
*/

/* load layers */
var idn_adm = ee.FeatureCollection("users/jasonjb82/trase_idn/idn_adm_kabupaten"),
    gaveau = ee.Image("users/trasegis/STORAGE/ID/LULC/gaveau_timber_2023"),
    gav_class = ee.Image("users/jasonjb82/idn_pulp_defor/REGBRNIDNMYS_FC_2001to2022_TTM_20230523_Class1to6"),
    peat_img = ee.FeatureCollection("projects/trase/STORAGE/ID/LULC/PEAT_AREA/gambut_indonesia_2019");


/* import functions */
var functions = require('users/trasegis/tools:functions');

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

// get peat areas and create binary mask
var mask = ee.Image(0).byte().paint(peat_img,1)
var peat_img = ee.Image(1)
peat_img = peat_img.mask(mask)
Map.addLayer(peat_img, {palette:"green"}, "peat")

// Calculating areas
var annual_peat_exp = ee.Image(0);
for (var yr= 2001; yr <= 2022; yr = yr + 1) {
  var step1 = gaveau.select('timberdeforestation_'+yr).eq(3) // to timber
                         .and(gaveau.select('timberdeforestation_'+(yr-1)).lte(2).multiply(peat_img));  // from peat forest
  annual_peat_exp = annual_peat_exp.addBands(step1.rename('peat_expansion_'+yr));
}


// export the results as a CSV file
functions.export_table(
     functions.region_reducer(annual_peat_exp.reproject({crs: proj,scale: 30}), idn_adm),
               'idn_pulp_annual_expansion_peatland_moa',
               'GEE_outputs');
