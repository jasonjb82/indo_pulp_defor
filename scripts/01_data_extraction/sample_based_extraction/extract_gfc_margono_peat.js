// Extracting GFC, Margono primary forest and peat ------

// load data
var hansen = ee.Image("UMD/hansen/global_forest_change_2022_v1_10"),
    margono_ttm = ee.Image("users/jasonjb82/idn_pulp_defor/IDN_ForestMask_2000_MargonoTTM"),
    peat_2019 = ee.FeatureCollection("projects/trase/STORAGE/ID/LULC/PEAT_AREA/gambut_indonesia_2019"),
    kali_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/KALIMANTAN_samples_hti"),
    mal_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/MALUKU_samples_hti"),
    papua_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/PAPUA_samples_hti"),
    sula_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SULAWESI_samples_hti"),
    suma_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/SUMATERA_samples_hti"),
    balinusa_samples = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/BALINUSA_samples_hti");

var features = suma_samples.select('sid');

// extract relevant GFC layers
var forcover_img = hansen.select(['treecover2000']); // select 2000 forest cover
var lossyear_img = hansen.select(['lossyear']); // select loss year layer
var primary_img = margono_ttm.updateMask(margono_ttm.select('b1').eq(100));  // identify primary forest

// get peat areas and create binary mask
var mask = ee.Image(0).byte().paint(peat_2019,1)
var peat_2019 = ee.Image(1)
peat_2019 = peat_2019.mask(mask)
Map.addLayer(peat_2019, {palette:"green"}, "peat")

// select and rename peat band
var peat_2019_img = peat_2019.select(
    ['constant'], // old name
    ['b1']  // new name
);

// below extracts pixel values for each feature in a feature collection
var features = features.map(function(feature) {
    var forcover = forcover_img.reduceRegion({reducer: 'first',geometry: feature.geometry(),scale:30,tileScale:16}).get('treecover2000');
    var lossyear = lossyear_img.reduceRegion({reducer: 'first',geometry: feature.geometry(),scale:30,tileScale:16}).get('lossyear');
    var peat = peat_2019_img.reduceRegion({geometry: feature.geometry(),reducer: ee.Reducer.first(),scale:90,tileScale:16}).get('b1');
    var primary = primary_img.reduceRegion({geometry: feature.geometry(),reducer: ee.Reducer.first(),scale:30,tileScale:16}).get('b1');
    return feature.set({
        'forcover': forcover, 'lossyear': lossyear,
        'peat': peat, 'primary': primary
    })
})

// table to googledrive export
Export.table.toDrive({
  collection: features,
  selectors: ['sid','forcover','lossyear','peat','primary'],
  description : 'suma_samples_gfc_margono_peat',
  folder: 'GEE_indo_pulp', // set output layer on googledrive
  fileFormat: 'CSV'
}); 