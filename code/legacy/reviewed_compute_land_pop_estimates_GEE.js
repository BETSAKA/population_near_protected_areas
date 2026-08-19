/**
 * POPULATION NEAR PROTECTED AREAS (WDPA MAY 2021)
 * ADM1 Segmentation + Absolute Hierarchy (Strict > Non-Strict > Unknown)
 */

// --- 1. PARAMETERS ---
var SOURCES = ['WP', 'GHSL'];
var BUFFER_METERS = 10000;
var POP_SCALE_M = 100;
var MAX_PIXELS = 1e13;
var TILE_SCALE = 16;
var EXPORT_FOLDER = 'PA_Pop_Final_Absolute';

var ISO3_LIST = [
  "AFG", "AGO", "BGD", "BEN", "BTN", "BOL", "BFA", "BDI", "CPV", "KHM",
  "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "EGY", "SLV",
  "ERI", "SWZ", "ETH", "GMB", "GHA", "GIN", "GNB", "HTI", "HND", "IND",
  "IDN", "KEN", "KIR", "PRK", "KGZ", "LAO", "LSO", "LBR", "MDG", "MWI",
  "MLI", "MRT", "FSM", "MDA", "MNG", "MAR", "MOZ", "MMR", "NPL", "NIC",
  "NER", "NGA", "PAK", "PNG", "PHL", "RWA", "STP", "SEN", "SLE", "SLB",
  "SOM", "SSD", "SDN", "SYR", "TJK", "TZA", "TLS", "TGO", "TUN", "UGA",
  "UKR", "UZB", "VUT", "VNM", "118", "129", "YEM", "ZMB", "ZWE", "118", 
  "129"
];

// --- 2. DATASETS ---
var ADM0 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM0');
var ADM1 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM1');
var WDPA_BASE = ee.FeatureCollection("WCMC/WDPA/202105/polygons")
  .filter(ee.Filter.inList('STATUS', ['Designated', 'Established', 'Inscribed']))
  .filter(ee.Filter.neq('DESIG_ENG', 'UNESCO-MAB Biosphere Reserve'))
  .filter(ee.Filter.neq('MARINE', '2'));

var landMask = ee.Image("ESA/WorldCover/v100/2020").select('Map').neq(80);

// --- 3. HELPER FUNCTIONS ---

function getPopImage(source, year) {
  var img = (source === 'WP') 
    ? ee.ImageCollection('WorldPop/GP/100m/pop').filter(ee.Filter.eq('year', year)).select('population').mosaic()
    : ee.Image('JRC/GHSL/P2023A/GHS_POP/' + year).select('population_count');
  return img.updateMask(landMask);
}

function getIucnMasks(fc) {
  var sMask = ee.Image().byte().paint(fc.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III'])), 1).unmask(0).gt(0);
  var nsMask = ee.Image().byte().paint(fc.filter(ee.Filter.inList('IUCN_CAT', ['IV','V','VI'])), 1).unmask(0).gt(0);
  var unkMask = ee.Image().byte().paint(fc.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III','IV','V','VI']).not()), 1).unmask(0).gt(0);
  
  var strictFinal = sMask;
  var nonStrictFinal = nsMask.and(strictFinal.not());
  var unknownFinal = unkMask.and(strictFinal.not()).and(nonStrictFinal.not());
  
  return {
    strict: strictFinal,
    nonStrict: nonStrictFinal,
    unknownCat: unknownFinal,
    counts: {
      strict: fc.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III'])).size(),
      nonStrict: fc.filter(ee.Filter.inList('IUCN_CAT', ['IV','V','VI'])).size(),
      unknownCat: fc.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III','IV','V','VI']).not()).size()
    }
  };
}

function processRegion(feature, admLevel, iso, source) {
  var geom = feature.geometry();
  var rows = [];
  var meta = {iso3: iso, adm_level: admLevel, adm_name: feature.get('shapeName'), adm_id: feature.get('shapeID'), source: source};

  var scenarios = [
    {id: 'Confirmed_2000', popYear: 2000, filter: ee.Filter.and(ee.Filter.gt('STATUS_YR', 0), ee.Filter.lte('STATUS_YR', 2000))},
    {id: 'Confirmed_2020', popYear: 2020, filter: ee.Filter.and(ee.Filter.gt('STATUS_YR', 0), ee.Filter.lte('STATUS_YR', 2020))},
    {id: 'Unknown_Year',   popYear: 2020, filter: ee.Filter.eq('STATUS_YR', 0)}
  ];

  scenarios.forEach(function(s) {
    var searchGeom = geom.simplify(1000).buffer(BUFFER_METERS, 1000);
    var scenarioFc = WDPA_BASE.filterBounds(searchGeom).filter(s.filter);
    var masks = getIucnMasks(scenarioFc);
    var kernel = ee.Kernel.euclidean(BUFFER_METERS, 'meters', false);
    var popImg = getPopImage(source, s.popYear);
    var areaImg = ee.Image.pixelArea().divide(1e6).updateMask(landMask);

    var cats = ['strict', 'nonStrict', 'unknownCat'];
    var imagesToStack = [popImg.rename('p_tot')];
    var claimed = ee.Image(0).byte().unmask(0);

    cats.forEach(function(c) {
      var m = masks[c];
      var m10_raw = m.focal_max({kernel: kernel}).gt(0);
      
      var m_exclusive = m.and(claimed.not());
      claimed = claimed.or(m_exclusive);

      var m10_exclusive = m10_raw.and(claimed.not());
      claimed = claimed.or(m10_exclusive);

      imagesToStack.push(popImg.updateMask(m_exclusive.updateMask(landMask)).rename('p_' + c));
      imagesToStack.push(popImg.updateMask(m10_exclusive.updateMask(landMask)).rename('p_' + c + '10'));
      imagesToStack.push(areaImg.updateMask(m_exclusive.updateMask(landMask)).rename('a_' + c));
      imagesToStack.push(areaImg.updateMask(m10_exclusive.updateMask(landMask)).rename('a_' + c + '10'));
    });

    var stats = ee.Image.cat(imagesToStack).reduceRegion({
      reducer: ee.Reducer.sum(), geometry: geom, scale: POP_SCALE_M, maxPixels: MAX_PIXELS, tileScale: TILE_SCALE
    });

    rows.push(ee.Feature(null, ee.Dictionary(meta).combine({
      scenario: s.id, pop_year: s.popYear,
      count_strict: masks.counts.strict, count_nonstrict: masks.counts.nonStrict, count_unknowncat: masks.counts.unknownCat,
      pop_total: stats.get('p_tot'), pop_strict: stats.get('p_strict'), pop_strict10: stats.get('p_strict10'),
      pop_nonstrict: stats.get('p_nonStrict'), pop_nonstrict10: stats.get('p_nonStrict10'),
      pop_unknowncat: stats.get('p_unknownCat'), pop_unknowncat10: stats.get('p_unknownCat10'),
      area_strict: stats.get('a_strict'), area_strict10: stats.get('a_strict10'),
      area_nonstrict: stats.get('a_nonStrict'), area_nonstrict10: stats.get('a_nonStrict10'),
      area_unknowncat: stats.get('a_unknownCat'), area_unknowncat10: stats.get('a_unknownCat10')
    })));
  });
  return ee.FeatureCollection(rows);
}

// --- 4. EXECUTION ---
SOURCES.forEach(function(source) {
  ISO3_LIST.forEach(function(iso) {
    var regions = ADM1.filter(ee.Filter.eq('shapeGroup', iso));
    var finalRegions = ee.FeatureCollection(ee.Algorithms.If(regions.size().gt(0), regions, ADM0.filter(ee.Filter.eq('shapeGroup', iso))));
    var results = finalRegions.map(function(feat) {
      var lvl = ee.Number(ee.Algorithms.If(regions.size().gt(0), 1, 0));
      return processRegion(feat, lvl, iso, source);
    }).flatten();
    Export.table.toDrive({
      collection: results, description: 'PA_Pop_' + iso + '_' + source,
      folder: EXPORT_FOLDER, fileFormat: 'CSV'
    });
  });
});