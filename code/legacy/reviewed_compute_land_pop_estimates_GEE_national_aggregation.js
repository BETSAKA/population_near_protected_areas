/**
 * NATIONAL TOTALS: PA AREA, COUNT, AND POPULATION
 * Run at ADM0 Level with Absolute Hierarchy
 */

var ISO3_LIST = ["AFG", "AGO", "BGD", "BEN", "BTN", "BOL", "BFA", "BDI", "CPV", "KHM", 
                 "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "EGY", "SLV", 
                 "ERI", "SWZ", "ETH", "GMB", "GHA", "GIN", "GNB", "HTI", "HND", "IND", 
                 "IDN", "KEN", "KIR", "PRK", "KGZ", "LAO", "LSO", "LBR", "MDG", "MWI", 
                 "MLI", "MRT", "FSM", "MDA", "MNG", "MAR", "MOZ", "MMR", "NPL", "NIC", 
                 "NER", "NGA", "PAK", "PNG", "PHL", "RWA", "STP", "SEN", "SLE", "SLB", 
                 "SOM", "SSD", "SDN", "SYR", "TJK", "TZA", "TLS", "TGO", "TUN", "UGA", 
                 "UKR", "UZB", "VUT", "VNM", "YEM", "ZMB", "ZWE", "118", "129"];

var ADM0 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM0');
var WDPA = ee.FeatureCollection("WCMC/WDPA/202105/polygons")
  .filter(ee.Filter.inList('STATUS', ['Designated', 'Established', 'Inscribed']))
  .filter(ee.Filter.neq('DESIG_ENG', 'UNESCO-MAB Biosphere Reserve'))
  .filter(ee.Filter.neq('MARINE', '2'));

var landMask = ee.Image("ESA/WorldCover/v100/2020").select('Map').neq(80);

// --- 1. POPULATION SOURCES ---
var getPop = function(source, year) {
  var img = (source === 'WP') 
    ? ee.ImageCollection('WorldPop/GP/100m/pop').filter(ee.Filter.eq('year', year)).select('population').mosaic()
    : ee.Image('JRC/GHSL/P2023A/GHS_POP/' + year).select('population_count');
  return img.updateMask(landMask);
};

// --- 2. PROCESSING FUNCTION ---
var processNational = function(iso) {
  var country = ADM0.filter(ee.Filter.eq('shapeGroup', iso));
  var geom = country.geometry();
  
  // Filter PAs for this country
  var countryPAs = WDPA.filterBounds(geom).filter(ee.Filter.eq('ISO3', iso));
  
  var strictFC = countryPAs.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III']));
  var nonStrictFC = countryPAs.filter(ee.Filter.inList('IUCN_CAT', ['IV','V','VI']));
  var unknownFC = countryPAs.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III','IV','V','VI']).not());

  // Create Hierarchical Masks
  var sMask = ee.Image().byte().paint(strictFC, 1).unmask(0).gt(0);
  var nsMask = ee.Image().byte().paint(nonStrictFC, 1).unmask(0).gt(0).and(sMask.not());
  var unkMask = ee.Image().byte().paint(unknownFC, 1).unmask(0).gt(0).and(sMask.not()).and(nsMask.not());
  var totalPAMask = sMask.or(nsMask).or(unkMask);

  var areaImg = ee.Image.pixelArea().divide(1e6).updateMask(landMask);
  
  // Population Images
  var wp00 = getPop('WP', 2000);
  var wp20 = getPop('WP', 2020);
  var gh00 = getPop('GHSL', 2000);
  var gh20 = getPop('GHSL', 2020);

  var stats = ee.Image.cat([
    areaImg.updateMask(sMask).rename('a_strict'),
    areaImg.updateMask(nsMask).rename('a_nonstrict'),
    areaImg.updateMask(unkMask).rename('a_unknown'),
    areaImg.updateMask(totalPAMask).rename('a_total'),
    wp00.rename('pop_wp_00'),
    wp20.rename('pop_wp_20'),
    gh00.rename('pop_gh_00'),
    gh20.rename('pop_gh_20')
  ]).reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: geom,
    scale: 250, // Increased scale for ADM0 to ensure stability
    maxPixels: 1e13
  });

  return ee.Feature(null, {
    'iso3': iso,
    'count_strict': strictFC.size(),
    'count_nonstrict': nonStrictFC.size(),
    'count_unknown': unknownFC.size(),
    'count_total': countryPAs.size(),
    'area_strict': stats.get('a_strict'),
    'area_nonstrict': stats.get('a_nonstrict'),
    'area_unknown': stats.get('a_unknown'),
    'area_total_pa': stats.get('a_total'),
    'nat_pop_wp_00': stats.get('pop_wp_00'),
    'nat_pop_wp_20': stats.get('pop_wp_20'),
    'nat_pop_gh_00': stats.get('pop_gh_00'),
    'nat_pop_gh_20': stats.get('pop_gh_20')
  });
};

// --- 3. EXECUTION ---
var nationalResults = ee.FeatureCollection(ISO3_LIST.map(processNational));

Export.table.toDrive({
  collection: nationalResults,
  description: 'National_PA_Totals_Refactored',
  fileFormat: 'CSV'
});