/**
 * Export the effective WDPA candidate polygon set used by
 * reviewed_compute_land_pop_estimates_GEE_all2020_fix.js.
 *
 * Purpose:
 * - reproduce the exact PA candidate-selection logic of the reviewed GEE fix
 *   script as a one-file-per-ISO polygon export
 * - support R-side auditing of the WDPA subset that the reviewed GEE fix could
 *   actually use in computation
 *
 * Important:
 * - this exports PA polygons only; it does not export buffers
 * - the 10 km buffer is used only internally to mirror the reviewed GEE search
 *   logic around each ADM region
 * - this script does not split by scenario; it exports the union of all WDPA
 *   polygons that could enter any scenario for the focal ISO
 * - this mirrors the reviewed fix script's strict ISO3 == iso filter, so
 *   pseudo-ISO codes like 118 and 129 will remain empty if the WDPA collection
 *   does not use those ISO3 values directly
 */

var EXPORT_FOLDER = 'WDPA_202105_effective_candidate_set_reviewed_all2020_fix';
var EXPORT_FORMAT = 'GeoJSON';
var BUFFER_METERS = 10000;

var ISO3_LIST = [
  'AFG', 'AGO', 'BGD', 'BEN', 'BTN', 'BOL', 'BFA', 'BDI', 'CPV', 'KHM',
  'CMR', 'CAF', 'TCD', 'COM', 'COD', 'COG', 'CIV', 'DJI', 'EGY', 'SLV',
  'ERI', 'SWZ', 'ETH', 'GMB', 'GHA', 'GIN', 'GNB', 'HTI', 'HND', 'IND',
  'IDN', 'KEN', 'KIR', 'PRK', 'KGZ', 'LAO', 'LSO', 'LBR', 'MDG', 'MWI',
  'MLI', 'MRT', 'FSM', 'MDA', 'MNG', 'MAR', 'MOZ', 'MMR', 'NPL', 'NIC',
  'NER', 'NGA', 'PAK', 'PNG', 'PHL', 'RWA', 'STP', 'SEN', 'SLE', 'SLB',
  'SOM', 'SSD', 'SDN', 'SYR', 'TJK', 'TZA', 'TLS', 'TGO', 'TUN', 'UGA',
  'UKR', 'UZB', 'VUT', 'VNM', '118', '129', 'YEM', 'ZMB', 'ZWE'
];

var ADM0 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM0');
var ADM1 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM1');

var WDPA_BASE = ee.FeatureCollection('WCMC/WDPA/202105/polygons')
  .filter(ee.Filter.inList('STATUS', ['Designated', 'Established', 'Inscribed']))
  .filter(ee.Filter.neq('DESIG_ENG', 'UNESCO-MAB Biosphere Reserve'))
  .filter(ee.Filter.neq('MARINE', '2'));

function getRegionsForIso(iso) {
  var adm1 = ADM1.filter(ee.Filter.eq('shapeGroup', iso));
  return ee.FeatureCollection(ee.Algorithms.If(
    adm1.size().gt(0),
    adm1,
    ADM0.filter(ee.Filter.eq('shapeGroup', iso))
  ));
}

function effectiveCandidateSetForIso(iso) {
  var regions = getRegionsForIso(iso);

  var collected = regions.map(function(feature) {
    var geom = feature.geometry();
    var searchGeom = geom.simplify(1000).buffer(BUFFER_METERS, 1000);

    return WDPA_BASE
      .filter(ee.Filter.eq('ISO3', iso))
      .filterBounds(searchGeom)
      .map(function(wdpaFeature) {
        return wdpaFeature
          .set('export_iso3', iso)
          .set('matched_region_id', feature.get('shapeID'))
          .set('matched_region_name', feature.get('shapeName'));
      });
  }).flatten();

  var distinctIds = ee.List(collected.aggregate_array('WDPAID')).distinct();

  return ee.FeatureCollection(distinctIds.map(function(wdpaid) {
    wdpaid = ee.Number(wdpaid);
    return ee.Feature(collected.filter(ee.Filter.eq('WDPAID', wdpaid)).first());
  }));
}

ISO3_LIST.forEach(function(iso) {
  var fc = effectiveCandidateSetForIso(iso);

  print(iso + ' export count', fc.size());

  Export.table.toDrive({
    collection: fc,
    description: 'WDPA_202105_' + iso,
    folder: EXPORT_FOLDER,
    fileFormat: EXPORT_FORMAT
  });
});