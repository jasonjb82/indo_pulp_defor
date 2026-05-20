/*
  
Assets description: 

Result:
        - Annual expansion in Indonesia on peat forests and mineral soils

Notes:
        - Expansion on forest & non-forest the same year as reported by gaveau (on peat and mineral soils)
    
*/

/* import data */

var idn_adm = ee.FeatureCollection("users/jasonjb82/trase_idn/idn_adm_kabupaten"),
    gaveau = ee.Image("users/trasegis/STORAGE/ID/LULC/gaveau_timber_2023"),
    gav_class = ee.Image("users/jasonjb82/idn_pulp_defor/REGBRNIDNMYS_FC_2001to2022_TTM_20230523_Class1to6");

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

// soil types
var soil_img = gav_class.select(['b1']);
print(soil_img);

// remap values
var peat_img = ee.Image(0)
      .where(soil_img.eq(0).and(soil_img.eq(1)),0)
      .where(soil_img.gte(100).and(soil_img.lte(122)),0)
      .where(soil_img.gte(300).and(soil_img.lt(600)),0)
      .where(soil_img.gte(600).and(soil_img.lt(623)),1)

var mineral_img = ee.Image(0)
      .where(soil_img.eq(0).and(soil_img.eq(1)),1)
      .where(soil_img.gte(100).and(soil_img.lte(122)),1)
      .where(soil_img.gte(300).and(soil_img.lt(600)),1)
      .where(soil_img.gte(600).and(soil_img.lt(623)),0)
      
// Calculating areas
var annual_soil_exp = ee.Image(0);
for (var yr= 2001; yr <= 2022; yr = yr + 1) {
  var step1 = gaveau.select('timberdeforestation_'+yr).eq(3) // to timber
                         .and(gaveau.select('timberdeforestation_'+(yr-1)).eq(1).multiply(peat_img));  // from peat forest
  var step2 = gaveau.select('timberdeforestation_'+yr).eq(3) // to timber
                         .and(gaveau.select('timberdeforestation_'+(yr-1)).lte(2).multiply(mineral_img));
  annual_soil_exp = annual_soil_exp.addBands(step1.rename('peat_defor_'+yr));
  annual_soil_exp = annual_soil_exp.addBands(step2.rename('mineral_defor_'+yr));
}

// export the results as a CSV file
functions.export_table(
     functions.region_reducer(annual_soil_exp.reproject({crs: proj,scale: 30}), idn_adm),
               'idn_pulp_annual_expansion_peat_mineral_soils',
               'GEE_outputs');