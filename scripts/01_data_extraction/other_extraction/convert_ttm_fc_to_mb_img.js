/*
  Convert TreeMap pulp expansion maps (2001-2022) featureclass to multi-band image for
  data extraction and anayslsus
*/


var table = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/idn_itp_traj_2000_2022"),
    kab = ee.FeatureCollection("users/jasonjb82/trase_idn/idn_adm_kabupaten"),
    geometry = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[93.27358963812654, 6.760289738554781],
          [93.27358963812654, -12.717839826402724],
          [142.58023026312654, -12.717839826402724],
          [142.58023026312654, 6.760289738554781]]], null, false);

var full = ee.FeatureCollection(table);
var lu_traj = full.first().propertyNames().sort().slice(10, -21)
print(lu_traj)

var col = ee.ImageCollection(lu_traj.map(function(property) {
  return full.select([property])
    .remap(['Forest','Non Forest','ITP'], [1,2,3],property)
    .reduceToImage([property], ee.Reducer.first())
    .set('Year', ee.String(property).replace('F','timberdeforestation_'))
    
 }))

var bands = col.toBands().regexpRename('(.*)_first', '$1')

var bands_renamed = bands.select(['0', '1', '2', '3','4','5','6','7','8','9','10',
                                  '11','12','13','14','15','16','17','18','19','20','21','22'],
['timberdeforestation_2000','timberdeforestation_2001','timberdeforestation_2002',
'timberdeforestation_2003','timberdeforestation_2004','timberdeforestation_2005',
'timberdeforestation_2006','timberdeforestation_2007','timberdeforestation_2008',
'timberdeforestation_2009','timberdeforestation_2010','timberdeforestation_2011',
'timberdeforestation_2012','timberdeforestation_2013','timberdeforestation_2014',
'timberdeforestation_2015','timberdeforestation_2016','timberdeforestation_2017',
'timberdeforestation_2018','timberdeforestation_2019','timberdeforestation_2020',
'timberdeforestation_2021','timberdeforestation_2022'])

Map.addLayer(bands_renamed, {min:1,max:3}, 'col')

//var bands_clipped = bands_renamed.clip(idn_boundaries);  

Export.image.toAsset({
  image: bands_renamed,
  description: 'gaveau_itp_2000_2022',
  assetId: 'idn_pulp_defor/gaveau_itp_2000_2022',
  scale: 10,
  region: geometry,
  maxPixels: 1e13,
});
