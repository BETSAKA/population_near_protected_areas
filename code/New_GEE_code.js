// POPULATION NEAR PROTECTED AREAS (2000 & 2020)
// STRICT / NON-STRICT / ALL PAs - WORLDPOP + GHSL

// PARAMETERS ---------------------------------------------------------------
var YEARS = [2000, 2020];
var BUFFER_METERS = 10000;
var AREA_THRESHOLD_KM2 = 1e6;
var POP_SCALE_M = 100;
var MAX_PIXELS = 1e13;
var TILE_SCALE = 4;

var EXPORT_DESCRIPTION = 'Population_PAs_Strict_Nonstrict_All_WP_GHSL_2000_2020';
var EXPORT_FOLDER = 'GEE_PA_Population_Analysis';


// COUNTRY LIST -------------------------------------------------------------
var ISO3_LIST = [
  'AFG','AGO','BGD','BEN','BTN','BOL','BFA','BDI','CPV','KHM','CMR','CAF','TCD','COM','COD','COG',
  'CIV','DJI','EGY','SLV','ERI','SWZ','ETH','GMB','GHA','GIN','GNB','HTI','HND','IND','IDN','KEN',
  'KIR','PRK','KGZ','LAO','LSO','LBR','MDG','MWI','MLI','MRT','FSM','MDA','MNG','MAR','MOZ','MMR',
  'NPL','NIC','NER','NGA','PAK','PNG','PHL','RWA','STP','SEN','SLE','SLB','SOM','SSD','SDN','SYR',
  'TJK','TZA','TLS','TGO','TUN','UGA','UKR','UZB','VUT','VNM','PSE','YEM','ZMB','ZWE'
];



// OPTIONAL: if some countries fail to process at first run --------------------

// FORCE ADM1 countries (override AREA_THRESHOLD_KM2 logic)
// recommended for countries that did not succeed at first run
var forceAdm1 = [
  'AFG','BGD','BEN','BFA','CMR','CAF','COG',
  'GIN',
  'LAO','MDG','MWI','MOZ', 'MMR',
  'NPL','NGA','PAK','PNG','PHL','STP','SSD',
  'TZA','UKR','UZB','VNM','PSE','YEM','ZMB'
  ];

// FORCE separate export for countries with complex geometries
// Case for Philippines, likely because of its archipelago nature
var splitAdm1Export = ['PHL'];

// EDIT:  List of countries already processed
// Leave empty if first run. If some countries failed,
// indicate here the ones that succeeded and do not need to be processed again. 
// e.g. var skipAlreadyProcessed = ['KEN', 'GHA', 'ZWE'];
var skipAlreadyProcessed = [
  'AGO','BTN','BOL','BDI','CPV','KHM','TCD','COM','COD', 'AFG','BGD','BEN','BFA','CMR','CAF','COG',
  'CIV','DJI','EGY','SLV','ERI','SWZ','ETH','GMB','GHA','GNB','HTI','HND','IND','IDN','KEN', 'GIN',
  'KIR','PRK','KGZ','LSO','LBR','MWI','MLI','MRT','FSM','MDA','MNG','MAR','LAO','MDG','MOZ', 'MMR',
  'NIC','NER','RWA','SEN','SLE','SLB','SOM','SDN','SYR', 'NPL','NGA','PAK','PNG','STP','SSD', 
  'TJK','TLS','TGO','TUN','UGA','VUT','ZWE',  'TZA','UKR','UZB','VNM','PSE','YEM','ZMB'
  ];  // fill as needed


// OUTPUT COLUMNS -----------------------------------------------------------
var EXPORT_COLUMNS = [
  'iso3', 'adm_level', 'country_name', 'adm1_name',
  // Strict
  'pa_count_strict_2000', 'pa_area_strict_2000_km2', 'pa_area10km_strict_2000_km2',
  'pop2000_in_pa_strict_WP', 'pop2000_in_pa10_strict_WP',
  'pop2000_in_pa_strict_GHSL', 'pop2000_in_pa10_strict_GHSL',
  'pa_count_strict_2020', 'pa_area_strict_2020_km2', 'pa_area10km_strict_2020_km2',
  'pop2020_in_pa_strict_WP', 'pop2020_in_pa10_strict_WP',
  'pop2020_in_pa_strict_GHSL', 'pop2020_in_pa10_strict_GHSL',
  // Non-strict
  'pa_count_non_2000', 'pa_area_non_2000_km2', 'pa_area10km_non_2000_km2',
  'pop2000_in_pa_non_WP', 'pop2000_in_pa10_non_WP',
  'pop2000_in_pa_non_GHSL', 'pop2000_in_pa10_non_GHSL',
  'pa_count_non_2020', 'pa_area_non_2020_km2', 'pa_area10km_non_2020_km2',
  'pop2020_in_pa_non_WP', 'pop2020_in_pa10_non_WP',
  'pop2020_in_pa_non_GHSL', 'pop2020_in_pa10_non_GHSL',
  // All (union of strict + non after overlap removal)
  'pa_count_all_2000', 'pa_area_all_2000_km2', 'pa_area10km_all_2000_km2',
  'pop2000_in_pa_all_WP', 'pop2000_in_pa10_all_WP',
  'pop2000_in_pa_all_GHSL', 'pop2000_in_pa10_all_GHSL',
  'pa_count_all_2020', 'pa_area_all_2020_km2', 'pa_area10km_all_2020_km2',
  'pop2020_in_pa_all_WP', 'pop2020_in_pa10_all_WP',
  'pop2020_in_pa_all_GHSL', 'pop2020_in_pa10_all_GHSL',
  // Totals
  'pop2000_total_WP', 'pop2020_total_WP',
  'pop2000_total_GHSL', 'pop2020_total_GHSL',
  'country_area_km2', 'adm_area_km2'
];

// DATASETS -----------------------------------------------------------------
var ADM0 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM0');
var ADM1 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM1');

// FIX #2: use shapeGroup -----------------------------------------------------
var countries = ADM0.filter(ee.Filter.inList('shapeGroup', ISO3_LIST));

// WDPA FILTERING (with marine exclusion) ----------------------------------
// NB: exclude purely marine polygons (MARINE == '2')
var WDPA = ee.FeatureCollection('WCMC/WDPA/current/polygons')
  .filter(ee.Filter.inList('STATUS', ['Designated', 'Established', 'Inscribed']))
  .filter(ee.Filter.neq('DESIG_ENG', 'UNESCO-MAB Biosphere Reserve'))
  .filter(ee.Filter.neq('MARINE', '2'));

var WDPA_STRICT = WDPA.filter(ee.Filter.inList('IUCN_CAT', ['Ia','Ib','II','III']));
var WDPA_NON    = WDPA.filter(ee.Filter.inList('IUCN_CAT', ['IV','V','VI']));

// YEAR-SPECIFIC WDPA -------------------------------------------------------
function wdpaByYear(fc, year){ return fc.filter(ee.Filter.lte('STATUS_YR', year)); }
var WDPA_SETS = ee.Dictionary({
  'strict_2000': wdpaByYear(WDPA_STRICT, 2000),
  'strict_2020': wdpaByYear(WDPA_STRICT, 2020),
  'non_2000':    wdpaByYear(WDPA_NON,    2000),
  'non_2020':    wdpaByYear(WDPA_NON,    2020)
  // (ALL will be formed from strict ∪ non after overlap handling)
});

// POPULATION DATA (WorldPop + GHSL) ---------------------------------------
var WP = ee.ImageCollection('WorldPop/GP/100m/pop');
var POP_WP = ee.Dictionary({
  '2000': WP.filter(ee.Filter.eq('year', 2000)).select('population').mosaic(),
  '2020': WP.filter(ee.Filter.eq('year', 2020)).select('population').mosaic()
});
var POP_GHSL = ee.Dictionary({
  '2000': ee.Image('JRC/GHSL/P2023A/GHS_POP/2000').select('population_count'),
  '2020': ee.Image('JRC/GHSL/P2023A/GHS_POP/2020').select('population_count')
});

// LAND MASK ---------------------------------------------------------------
// use 2020 WorldPop as a land mask: population > 0 => land
var landMask = ee.Image(POP_WP.get('2020')).gt(0)
  .or(ee.Image(POP_GHSL.get('2020')).gt(0))
  .unmask(0);

// HELPERS ------------------------------------------------------------------
function safeNumber(x, fallback){ return ee.Number(ee.Algorithms.If(x, x, fallback)); }

function calcPopulation(popImage, region, maskOrNull){
  // always restrict to land
  var img = popImage.updateMask(landMask);

  // then apply PA / buffer mask if provided
  if (maskOrNull) {
    img = img.updateMask(maskOrNull);
  }

  var dict = img.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: region,
    scale: POP_SCALE_M,
    maxPixels: MAX_PIXELS,
    tileScale: TILE_SCALE,
    bestEffort: true
  });

  var key = img.bandNames().get(0);
  return safeNumber(dict.get(key), 0).round();
}

function calcAreaKm2(maskImg, region){
  var dict = ee.Image.pixelArea().updateMask(maskImg).reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: region,
    scale: POP_SCALE_M,
    maxPixels: MAX_PIXELS,
    tileScale: TILE_SCALE,
    bestEffort: true
  });
  return safeNumber(dict.get('area'), 0).divide(1e6);
}

function makePaMask(paFc, region){
  var count = ee.Number(paFc.size());
  return ee.Image(ee.Algorithms.If(
    count.gt(0),
    paFc.reduceToImage({properties:['WDPAID'], reducer: ee.Reducer.count()}).gt(0).clip(region),
    ee.Image(0).clip(region)
  ));
}

function makeBufferMask(paMask, region){
  var kernel = ee.Kernel.euclidean(BUFFER_METERS, 'meters', false);
  return ee.Image(paMask).focal_max({kernel: kernel}).clip(region);
}

// DISTINCT WDPA COUNT (for ALL = union) -----------------------------------
function countDistinctWdpaid(fc){
  // Reduce to a list of WDPAID and count distinct
  var ids = ee.FeatureCollection(fc).aggregate_array('WDPAID');
  return ee.Number(ids.distinct().size());
}

// CORE REGION PROCESSOR ----------------------------------------------------
function processRegion(regionFeat, admLevel){
  var geom = regionFeat.geometry();

  // Use shapeGroup for iso3 -----------------------------------------
  var iso = ee.String(regionFeat.get('shapeGroup'));

  // Correct naming for ADM0 vs ADM1 -------------------------------
  var featName = ee.String(regionFeat.get('shapeName')); // name of the processed feature
  var countryName = (admLevel === 0)
    ? featName
    : countries.filter(ee.Filter.eq('shapeGroup', ee.String(regionFeat.get('shapeGroup'))))
               .first().get('shapeName');
  var adm1Name = (admLevel === 1) ? featName : null;

  var admAreaKm2 = geom.area().divide(1e6);
  var countryGeom = countries.filter(ee.Filter.eq('shapeGroup', iso)).geometry();
  var countryAreaKm2 = countryGeom.area().divide(1e6);

  // Build strict/non masks by year ----------------------------------------
  var paStrict2000 = makePaMask(ee.FeatureCollection(WDPA_SETS.get('strict_2000')).filterBounds(geom), geom);
  var paStrict2020 = makePaMask(ee.FeatureCollection(WDPA_SETS.get('strict_2020')).filterBounds(geom), geom);
  var paNon2000    = makePaMask(ee.FeatureCollection(WDPA_SETS.get('non_2000')).filterBounds(geom), geom);
  var paNon2020    = makePaMask(ee.FeatureCollection(WDPA_SETS.get('non_2020')).filterBounds(geom), geom);

  // Overlap removal for non-strict ----------------------------------------
  paNon2000 = paNon2000.and(paStrict2000.not()).clip(geom);
  paNon2020 = paNon2020.and(paStrict2020.not()).clip(geom);

  // FIX #3: define ALL as union of strict ∪ (non minus strict) ------------
  var paAll2000 = paStrict2000.or(paNon2000).clip(geom);
  var paAll2020 = paStrict2020.or(paNon2020).clip(geom);

  // 10km union masks (PA ∪ buffer) ----------------------------------------
  var paStrict10km2000 = paStrict2000.or(makeBufferMask(paStrict2000, geom));
  var paStrict10km2020 = paStrict2020.or(makeBufferMask(paStrict2020, geom));
  var paNon10km2000    = paNon2000.or(makeBufferMask(paNon2000, geom));
  var paNon10km2020    = paNon2020.or(makeBufferMask(paNon2020, geom));
  var paAll10km2000    = paAll2000.or(makeBufferMask(paAll2000, geom));
  var paAll10km2020    = paAll2020.or(makeBufferMask(paAll2020, geom));

  // PA COUNTS --------------------------------------------------------------
  var strict2000_fc = ee.FeatureCollection(WDPA_SETS.get('strict_2000')).filterBounds(geom);
  var strict2020_fc = ee.FeatureCollection(WDPA_SETS.get('strict_2020')).filterBounds(geom);
  var non2000_fc    = ee.FeatureCollection(WDPA_SETS.get('non_2000')).filterBounds(geom);
  var non2020_fc    = ee.FeatureCollection(WDPA_SETS.get('non_2020')).filterBounds(geom);

  var pa_count_strict_2000 = strict2000_fc.size();
  var pa_count_strict_2020 = strict2020_fc.size();
  var pa_count_non_2000    = non2000_fc.size();
  var pa_count_non_2020    = non2020_fc.size();

  // ALL counts as distinct WDPAID across strict ∪ non ---------------------
  var all2000_fc = strict2000_fc.merge(non2000_fc);
  var all2020_fc = strict2020_fc.merge(non2020_fc);
  var pa_count_all_2000 = countDistinctWdpaid(all2000_fc);
  var pa_count_all_2020 = countDistinctWdpaid(all2020_fc);

  // AREAS (km²) ------------------------------------------------------------
  var pa_area_strict_2000_km2 = calcAreaKm2(paStrict2000, geom);
  var pa_area_strict_2020_km2 = calcAreaKm2(paStrict2020, geom);
  var pa_area10km_strict_2000_km2 = calcAreaKm2(paStrict10km2000, geom);
  var pa_area10km_strict_2020_km2 = calcAreaKm2(paStrict10km2020, geom);

  var pa_area_non_2000_km2 = calcAreaKm2(paNon2000, geom);
  var pa_area_non_2020_km2 = calcAreaKm2(paNon2020, geom);
  var pa_area10km_non_2000_km2 = calcAreaKm2(paNon10km2000, geom);
  var pa_area10km_non_2020_km2 = calcAreaKm2(paNon10km2020, geom);

  var pa_area_all_2000_km2 = calcAreaKm2(paAll2000, geom);
  var pa_area_all_2020_km2 = calcAreaKm2(paAll2020, geom);
  var pa_area10km_all_2000_km2 = calcAreaKm2(paAll10km2000, geom);
  var pa_area10km_all_2020_km2 = calcAreaKm2(paAll10km2020, geom);

  // POPULATIONS ------------------------------------------------------------
  var wp2000 = ee.Image(POP_WP.get('2000'));
  var wp2020 = ee.Image(POP_WP.get('2020'));
  var gh2000 = ee.Image(POP_GHSL.get('2000'));
  var gh2020 = ee.Image(POP_GHSL.get('2020'));

  var pop2000_in_pa_strict_WP = calcPopulation(wp2000, geom, paStrict2000);
  var pop2000_in_pa10_strict_WP = calcPopulation(wp2000, geom, paStrict10km2000);
  var pop2020_in_pa_strict_WP = calcPopulation(wp2020, geom, paStrict2020);
  var pop2020_in_pa10_strict_WP = calcPopulation(wp2020, geom, paStrict10km2020);

  var pop2000_in_pa_non_WP = calcPopulation(wp2000, geom, paNon2000);
  var pop2000_in_pa10_non_WP = calcPopulation(wp2000, geom, paNon10km2000);
  var pop2020_in_pa_non_WP = calcPopulation(wp2020, geom, paNon2020);
  var pop2020_in_pa10_non_WP = calcPopulation(wp2020, geom, paNon10km2020);

  var pop2000_in_pa_all_WP = calcPopulation(wp2000, geom, paAll2000);
  var pop2000_in_pa10_all_WP = calcPopulation(wp2000, geom, paAll10km2000);
  var pop2020_in_pa_all_WP = calcPopulation(wp2020, geom, paAll2020);
  var pop2020_in_pa10_all_WP = calcPopulation(wp2020, geom, paAll10km2020);

  var pop2000_in_pa_strict_GHSL = calcPopulation(gh2000, geom, paStrict2000);
  var pop2000_in_pa10_strict_GHSL = calcPopulation(gh2000, geom, paStrict10km2000);
  var pop2020_in_pa_strict_GHSL = calcPopulation(gh2020, geom, paStrict2020);
  var pop2020_in_pa10_strict_GHSL = calcPopulation(gh2020, geom, paStrict10km2020);

  var pop2000_in_pa_non_GHSL = calcPopulation(gh2000, geom, paNon2000);
  var pop2000_in_pa10_non_GHSL = calcPopulation(gh2000, geom, paNon10km2000);
  var pop2020_in_pa_non_GHSL = calcPopulation(gh2020, geom, paNon2020);
  var pop2020_in_pa10_non_GHSL = calcPopulation(gh2020, geom, paNon10km2020);

  var pop2000_in_pa_all_GHSL = calcPopulation(gh2000, geom, paAll2000);
  var pop2000_in_pa10_all_GHSL = calcPopulation(gh2000, geom, paAll10km2000);
  var pop2020_in_pa_all_GHSL = calcPopulation(gh2020, geom, paAll2020);
  var pop2020_in_pa10_all_GHSL = calcPopulation(gh2020, geom, paAll10km2020);

  var pop2000_total_WP = calcPopulation(wp2000, geom, null);
  var pop2020_total_WP = calcPopulation(wp2020, geom, null);
  var pop2000_total_GHSL = calcPopulation(gh2000, geom, null);
  var pop2020_total_GHSL = calcPopulation(gh2020, geom, null);

  return ee.Feature(null, {
    iso3: ee.String(regionFeat.get('shapeGroup')),
    adm_level: admLevel,             
    country_name: countryName,      
    adm1_name: adm1Name,           

    pa_count_strict_2000: pa_count_strict_2000,
    pa_area_strict_2000_km2: pa_area_strict_2000_km2,
    pa_area10km_strict_2000_km2: pa_area10km_strict_2000_km2,
    pop2000_in_pa_strict_WP: pop2000_in_pa_strict_WP,
    pop2000_in_pa10_strict_WP: pop2000_in_pa10_strict_WP,
    pop2000_in_pa_strict_GHSL: pop2000_in_pa_strict_GHSL,
    pop2000_in_pa10_strict_GHSL: pop2000_in_pa10_strict_GHSL,

    pa_count_strict_2020: pa_count_strict_2020,
    pa_area_strict_2020_km2: pa_area_strict_2020_km2,
    pa_area10km_strict_2020_km2: pa_area10km_strict_2020_km2,
    pop2020_in_pa_strict_WP: pop2020_in_pa_strict_WP,
    pop2020_in_pa10_strict_WP: pop2020_in_pa10_strict_WP,
    pop2020_in_pa_strict_GHSL: pop2020_in_pa_strict_GHSL,
    pop2020_in_pa10_strict_GHSL: pop2020_in_pa10_strict_GHSL,

    pa_count_non_2000: pa_count_non_2000,
    pa_area_non_2000_km2: pa_area_non_2000_km2,
    pa_area10km_non_2000_km2: pa_area10km_non_2000_km2,
    pop2000_in_pa_non_WP: pop2000_in_pa_non_WP,
    pop2000_in_pa10_non_WP: pop2000_in_pa10_non_WP,
    pop2000_in_pa_non_GHSL: pop2000_in_pa_non_GHSL,
    pop2000_in_pa10_non_GHSL: pop2000_in_pa10_non_GHSL,

    pa_count_non_2020: pa_count_non_2020,
    pa_area_non_2020_km2: pa_area_non_2020_km2,
    pa_area10km_non_2020_km2: pa_area10km_non_2020_km2,
    pop2020_in_pa_non_WP: pop2020_in_pa_non_WP,
    pop2020_in_pa10_non_WP: pop2020_in_pa10_non_WP,
    pop2020_in_pa_non_GHSL: pop2020_in_pa_non_GHSL,
    pop2020_in_pa10_non_GHSL: pop2020_in_pa10_non_GHSL,

    pa_count_all_2000: pa_count_all_2000,
    pa_area_all_2000_km2: pa_area_all_2000_km2,
    pa_area10km_all_2000_km2: pa_area10km_all_2000_km2,
    pop2000_in_pa_all_WP: pop2000_in_pa_all_WP,
    pop2000_in_pa10_all_WP: pop2000_in_pa10_all_WP,
    pop2000_in_pa_all_GHSL: pop2000_in_pa_all_GHSL,
    pop2000_in_pa10_all_GHSL: pop2000_in_pa10_all_GHSL,

    pa_count_all_2020: pa_count_all_2020,
    pa_area_all_2020_km2: pa_area_all_2020_km2,
    pa_area10km_all_2020_km2: pa_area10km_all_2020_km2,
    pop2020_in_pa_all_WP: pop2020_in_pa_all_WP,
    pop2020_in_pa10_all_WP: pop2020_in_pa10_all_WP,
    pop2020_in_pa_all_GHSL: pop2020_in_pa_all_GHSL,
    pop2020_in_pa10_all_GHSL: pop2020_in_pa10_all_GHSL,

    pop2000_total_WP: pop2000_total_WP,
    pop2020_total_WP: pop2020_total_WP,
    pop2000_total_GHSL: pop2000_total_GHSL,
    pop2020_total_GHSL: pop2020_total_GHSL,

    country_area_km2: countryAreaKm2,
    adm_area_km2: admAreaKm2
  });
}

// PIPELINE ----------------------------------------------------------------
var countriesWithArea = countries
  .filter(ee.Filter.notNull(['shapeGroup']))           // safety
  .filter(ee.Filter.inList('shapeGroup', ISO3_LIST))   // ensure consistent
  .map(function(f){
    var a   = f.geometry().area().divide(1e6);
    var iso = f.get('shapeGroup');

    var force   = ee.List(forceAdm1).contains(iso);
    var autoBig = a.gt(AREA_THRESHOLD_KM2);

    var bigFlag = ee.Algorithms.If(force, 1, autoBig);  // 1 if forced, else boolean

    return f.set({
      countryArea_km2: a,
      bigFlag: bigFlag
    });
  })
  // remove countries already processed
  .filter(ee.Filter.inList('shapeGroup', skipAlreadyProcessed).not());
var big   = countriesWithArea.filter(ee.Filter.eq('bigFlag', 1));
var small = countriesWithArea.filter(ee.Filter.eq('bigFlag', 0));

print('To process at ADM0:', small.size());
print('To process at ADM1:', big.size());

var resultsSmall = small.map(function(f){ return processRegion(f, 0); });
var resultsBig = big.map(function(ctry){
  var iso = ctry.get('shapeGroup');
  var sub = ADM1
    .filter(ee.Filter.eq('shapeGroup', iso))
    .filterBounds(ctry.geometry());
  return sub.map(function(adm1){ return processRegion(adm1, 1); });
}).flatten();

var allResults = resultsSmall.merge(resultsBig);
// print('Preview:', allResults.limit(5)); // Fails: 'Too many concurrent aggregations.' Frontend issue afai

// SPLIT EXPORT PER COUNTRY -----------------------------------------------

countriesWithArea.evaluate(function(countryList) {
  countryList.features.forEach(function(f) {

    var iso   = f.properties.shapeGroup;
    var isBig = f.properties.bigFlag === 1;

    print('Submitting export for:', iso);

    // IF THE COUNTRY IS IN splitAdm1Export, export one task per ADM1
    if (splitAdm1Export.indexOf(iso) !== -1) {

      ADM1.filter(ee.Filter.eq('shapeGroup', iso))
        .evaluate(function(adm1List) {

          adm1List.features.forEach(function(a) {

            var adm1Name = a.properties.shapeName;
            var adm1Id = adm1Name.replace(/[^\w]/g, '_');  // safe filename

            // one-feature FC for this ADM1
            var fc = ee.FeatureCollection([ee.Feature(a)]);

            var resultsAdm1 = fc.map(function(feat) {
              return processRegion(feat, 1);  // ADM1-level processing
            });

            Export.table.toDrive({
              collection: resultsAdm1,
              description: 'PA_Pop_' + iso + '_ADM1_' + adm1Id + '_WP_GHSL',
              folder: EXPORT_FOLDER,
              fileNamePrefix: 'PA_Pop_' + iso + '_ADM1_' + adm1Id,
              fileFormat: 'CSV',
              selectors: EXPORT_COLUMNS
            });
          });
        });

      // skip standard country export
      return;
    }


    // STANDARD EXPORT: one task per country (ADM0 or ADM1)
    var subset = isBig
      ? ADM1.filter(ee.Filter.eq('shapeGroup', iso))
      : ee.FeatureCollection(countriesWithArea)
          .filter(ee.Filter.eq('shapeGroup', iso));

    var results = subset.map(function(feat){
      return processRegion(feat, isBig ? 1 : 0);
    });

    Export.table.toDrive({
      collection: results,
      description: 'PA_Pop_' + iso + '_WP_GHSL',
      folder: EXPORT_FOLDER,
      fileNamePrefix: 'PA_Pop_' + iso,
      fileFormat: 'CSV',
      selectors: EXPORT_COLUMNS
    });

  });
});