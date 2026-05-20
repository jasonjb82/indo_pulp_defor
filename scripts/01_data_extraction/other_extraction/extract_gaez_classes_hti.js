/**
 * Script to calculate GAEZ classes by pulp concessions
 * Variables: GAEZ dataset
 */
 
// Load data
var hti = ee.FeatureCollection("projects/trase/STORAGE/ID/BOUNDARIES/ID_pulpwood_concessions_3_1"),
    gaez = ee.Image("users/jasonjb82/idn_pulp_defor/aez_v9v2red_CRUTS32_Hist_8110_100_avg");

// extract gaez classes
var gaez_classes = gaez.select(['b1'],['class'])

// area calculation by gaez classes by hti concessions
var calculateClassArea = function(feature) {
    var areas = ee.Image.pixelArea().addBands(gaez_classes).reduceRegion({
      reducer: ee.Reducer.sum().group({
      groupField: 1,
      groupName: 'class',
    }),
    geometry: feature.geometry(),
    scale: 30,
    maxPixels: 1e50
    })
    var classAreas = ee.List(areas.get('groups'))
    var classAreaLists = classAreas.map(function(item) {
      var areaDict = ee.Dictionary(item)
      var classNumber = ee.Number(areaDict.get('class')).format()
      var area = ee.Number(areaDict.get('sum'))
      return ee.List([classNumber, area])
    })
    var result = ee.Dictionary(classAreaLists.flatten())
    // the result dictionary has area for all the classes
    // we add the hti_code to the dictionary and create a feature
    var hti_code = feature.get('ID')
    return ee.Feature(feature.geometry(), result.set('ID',hti_code))
}
var htiAreas = hti.map(calculateClassArea);

var classes = ee.List.sequence(1,33)
// as we need to use the list of output fields in the Export function
// we have to call .getInfo() to get the list values on the client-side
var outputFields = ee.List(['ID']).cat(classes).getInfo()

print(outputFields)
// export the results as a CSV file
Export.table.toDrive({
    collection: htiAreas,
    description: 'gaez_classes_by_conc',
    folder: 'GEE_outputs',
    fileFormat: 'CSV',
    selectors: outputFields
    })
