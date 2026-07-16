/**
 * POPULATION NEAR PROTECTED AREAS (WDPA MAY 2021)
 * Optimized: All_2020 Only + FastDistanceTransform
 */

// --- 1. PARAMETERS ---
var SOURCES = ['WP', 'GHSL'];
var BUFFER_METERS = 10000;
var POP_SCALE_M = 100;
var MAX_PIXELS = 1e13;
var TILE_SCALE = 16;
var EXPORT_FOLDER = 'PA_Pop_Final_All2020_Only';

var ISO3_LIST = [
  "AFG", "AGO", "BGD", "BEN", "BTN", "BOL", "BFA", "BDI", "CPV", "KHM"//,
//  "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "EGY", "SLV",
//  "ERI", "SWZ", "ETH", "GMB", "GHA", "GIN", "GNB", "HTI", "HND", "IND",
//  "IDN", "KEN", "KIR", "PRK", "KGZ", "LAO", "LSO", "LBR", "MDG", "MWI",
//  "MLI", "MRT", "FSM", "MDA", "MNG", "MAR", "MOZ", "MMR", "NPL", "NIC",
//  "NER", "NGA", "PAK", "PNG", "PHL", "RWA", "STP", "SEN", "SLE", "SLB",
//  "SOM", "SSD", "SDN", "SYR", "TJK", "TZA", "TLS", "TGO", "TUN", "UGA",
//  "UKR", "UZB", "VUT", "VNM", "118", "129", "YEM", "ZMB", "ZWE"
];

// --- 2. DATASETS ---
var ADM0 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM0');
var ADM1 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM1');
var WDPA_BASE = ee.FeatureCollection("WCMC/WDPA/202105/polygons")
  .filter(ee.Filter.inList('STATUS', ['Designated', 'Established', 'Inscribed']))
  .filter(ee.Filter.neq('DESIG_ENG', 'UNESCO-MAB Biosphere Reserve'))
  .filter(ee.Filter.neq('MARINE', '2'));

var landMask = ee.Image("ESA/WorldCover/v100/2020").select('Map').neq(80);

// Filter for All_2020 definition
var all2020Filter = ee.Filter.or(
  ee.Filter.and(ee.Filter.gt('STATUS_YR', 0), ee.Filter.lte('STATUS_YR', 2020)),
  ee.Filter.eq('STATUS_YR', 0)
);

// --- 3. HELPER FUNCTIONS ---
function getPopImage(source, year) {
  var img = (source === 'WP')
    ? ee.ImageCollection('WorldPop/GP/100m/pop').filter(ee.Filter.eq('year', year)).select('population').mosaic()
    : ee.Image('JRC/GHSL/P2023A/GHS_POP/' + year).select('population_count');
  return img.updateMask(landMask);
}

function processRegion(feature, admLevel, iso, source) {
  var geom = feature.geometry();
  var meta = {iso3: iso, adm_level: admLevel, adm_name: feature.get('shapeName'), adm_id: feature.get('shapeID'), source: source};
  
  // 1. Filter WDPA for this region only
  var scenarioFc = WDPA_BASE
    .filter(ee.Filter.eq('ISO3', iso))
    .filterBounds(geom.buffer(BUFFER_METERS * 2)) // Buffer slightly more to catch edge cases
    .filter(all2020Filter);

  // 2. Define Masks
  var sMask = ee.Image().byte().paint(scenarioFc.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III'])), 1).unmask(0).gt(0);
  var nsMask = ee.Image().byte().paint(scenarioFc.filter(ee.Filter.inList('IUCN_CAT', ['IV','V','VI'])), 1).unmask(0).gt(0);
  var unkMask = ee.Image().byte().paint(scenarioFc.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III','IV','V','VI']).not()), 1).unmask(0).gt(0);

  var masks = {strict: sMask, nonStrict: nsMask, unknownCat: unkMask};
  
  // 3. Prepare processing
  var popImg = getPopImage(source, 2020);
  var areaImg = ee.Image.pixelArea().divide(1e6).updateMask(landMask);
  
  var cats = ['strict', 'nonStrict', 'unknownCat'];
  var imagesToStack = [popImg.rename('p_tot')];
  var claimed = ee.Image(0).byte().unmask(0);

  // 4. Calculate stats per category with hierarchy enforcement
  cats.forEach(function(c) {
    var m = masks[c];
    
    // Fast Distance Transform (The optimization)
    // 110 pixels * 100m = 11km, which safely covers the 10km buffer requirement
    var m10_raw = m.not().fastDistanceTransform(110)
      .multiply(POP_SCALE_M)
      .lte(BUFFER_METERS)
      .and(landMask);

    // Apply hierarchy: Exclude areas already claimed by higher-tier categories
    var m_exclusive = m.and(claimed.not());
    claimed = claimed.or(m_exclusive);

    var m10_exclusive = m10_raw.and(claimed.not());
    claimed = claimed.or(m10_exclusive);

    imagesToStack.push(popImg.updateMask(m_exclusive).rename('p_' + c));
    imagesToStack.push(popImg.updateMask(m10_exclusive).rename('p_' + c + '10'));
    imagesToStack.push(areaImg.updateMask(m_exclusive).rename('a_' + c));
    imagesToStack.push(areaImg.updateMask(m10_exclusive).rename('a_' + c + '10'));
  });

  var stats = ee.Image.cat(imagesToStack).reduceRegion({
    reducer: ee.Reducer.sum(), geometry: geom, scale: POP_SCALE_M, maxPixels: MAX_PIXELS, tileScale: TILE_SCALE
  });

  return ee.Feature(null, ee.Dictionary(meta).combine({
    scenario: 'All_2020',
    pop_total: stats.get('p_tot'), 
    pop_strict: stats.get('p_strict'), pop_strict10: stats.get('p_strict10'),
    pop_nonstrict: stats.get('p_nonStrict'), pop_nonstrict10: stats.get('p_nonStrict10'),
    pop_unknowncat: stats.get('p_unknownCat'), pop_unknowncat10: stats.get('p_unknownCat10'),
    area_strict: stats.get('a_strict'), area_strict10: stats.get('a_strict10'),
    area_nonstrict: stats.get('a_nonStrict'), area_nonstrict10: stats.get('a_nonStrict10'),
    area_unknowncat: stats.get('a_unknownCat'), area_unknowncat10: stats.get('a_unknownCat10')
  }));
}

// --- 4. EXECUTION ---
SOURCES.forEach(function(source) {
  ISO3_LIST.forEach(function(iso) {
    var regions = ADM1.filter(ee.Filter.eq('shapeGroup', iso));
    var finalRegions = ee.FeatureCollection(ee.Algorithms.If(regions.size().gt(0), regions, ADM0.filter(ee.Filter.eq('shapeGroup', iso))));
    
    var results = finalRegions.map(function(feat) {
      var lvl = ee.Number(ee.Algorithms.If(regions.size().gt(0), 1, 0));
      return processRegion(feat, lvl, iso, source);
    });

    Export.table.toDrive({
      collection: results, description: 'PA_Pop_' + iso + '_' + source + '_All2020_Only',
      folder: EXPORT_FOLDER, fileFormat: 'CSV'
    });
  });
});