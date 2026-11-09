// Tabulating forest area by concession type and island (SI Table SM12) ------
//
// Sums the area of every TreeMap forest class (gfc_ttm) inside each concession
// layer, by island. Each layer is painted to a mask first, so overlapping
// polygons within a layer are counted once. Overlaps between layers are not
// removed: each layer is tabulated on its own.
//
// gfc_ttm codes (as in 01_data_prep.R): 100 / 400 / 600 = dryland / mangrove /
// peat swamp forest with no loss by 2022; 1xx / 4xx / 6xx = forest lost in
// year 20xx. Remaining forest 2022 = codes 100, 400 and 600. Forest in 2000 =
// all 1xx, 4xx and 6xx codes. The "indonesia" layer (all islands) gives the
// national forest area in 2000 for comparison with SI Table SM1.
//
// Output: one CSV with columns layer, island_code, gfc_ttm, area_ha, where
// island_code = 1 BALINUSA, 2 KALIMANTAN, 3 MALUKU, 4 PAPUA, 5 SULAWESI,
// 6 SUMATERA, 7 JAWA.
//
// Concession layers: remote/01_data/01_in/wwi/wwi_concessions, uploaded
// as assets. Islands: remote/01_data/01_in/klhk/KLHK_Kelompok_Pulau_20210308.

// load data
var gfc_ttm = ee.Image("users/jasonjb82/idn_pulp_defor/REGBRNIDNMYS_FC_2001to2022_TTM_20230523_Class1to6").select('b1'),
    islands = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/id_islands_klhk"),
    kab = ee.FeatureCollection("projects/trase/STORAGE/ID/BOUNDARIES/id_kabupaten"),
    hti_pulp = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/IUPHHK_HT_Pulp_suppliers_all_2024"),
    ha_all = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/IUPHHK_Hutan_Alam_20220320_updated"),
    ha_without_hpt = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/IUPHHK_Hutan_Alam_20220320_updated_without_HPT"),
    arahan = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/Arahan_2022_updated_without_HL"),
    pbph_2022 = ee.FeatureCollection("users/jasonjb82/idn_pulp_defor/PBPH_29112022");

// island code image
var island_codes = ee.Dictionary({
  BALINUSA: 1, KALIMANTAN: 2, MALUKU: 3, PAPUA: 4, SULAWESI: 5, SUMATERA: 6, JAWA: 7
});
var island_img = islands
  .map(function(f) { return f.set('code', island_codes.get(f.get('pulau'), 9)); })
  .reduceToImage(['code'], ee.Reducer.first())
  .rename('island');

// layers to tabulate. The KLHK island groups extend over sea and neighbouring
// countries (the TreeMap map covers Malaysia and Brunei too), so the national
// total is masked by Indonesia's district (kabupaten) boundaries instead.
var layers = {
  indonesia: kab,
  hti_pulp_2024: hti_pulp,
  ha_all: ha_all,
  ha_without_hpt: ha_without_hpt,
  arahan_2022: arahan,
  pbph_29112022: pbph_2022
};

// group code = island * 1000 + gfc_ttm
var group_img = island_img.multiply(1000).add(gfc_ttm).toInt().rename('group');
var area_img = ee.Image.pixelArea().divide(10000).rename('area_ha');
var region = islands.geometry().bounds();

var tabulate = function(name) {
  var mask = ee.Image(0).byte().paint(layers[name], 1);
  var sums = area_img.addBands(group_img)
    .updateMask(mask.and(gfc_ttm.gt(0)))
    .reduceRegion({
      reducer: ee.Reducer.sum().group({groupField: 1, groupName: 'group'}),
      geometry: region,
      crs: gfc_ttm.projection(),
      scale: 30,
      maxPixels: 1e13,
      tileScale: 16
    });
  return ee.FeatureCollection(ee.List(sums.get('groups')).map(function(g) {
    g = ee.Dictionary(g);
    var group = ee.Number(g.get('group'));
    return ee.Feature(null, {
      layer: name,
      island_code: group.divide(1000).floor(),
      gfc_ttm: group.mod(1000),
      area_ha: g.get('sum')
    });
  }));
};

// one export per layer, so a failure in one does not lose the others
Object.keys(layers).forEach(function(name) {
  Export.table.toDrive({
    collection: tabulate(name),
    description: 'forest_by_concession_type_' + name,
    folder: 'GEE_indo_pulp', // set output layer on googledrive
    selectors: ['layer', 'island_code', 'gfc_ttm', 'area_ha'],
    fileFormat: 'CSV'
  });
});
