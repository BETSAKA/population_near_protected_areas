/**
 * Export WDPA May 2021 PA polygons for R-side reproduction.
 *
 * Purpose:
 * - export the PA source polygons only
 * - keep all buffering, clipping, and STATUS_YR scenario logic in R
 */

var EXPORT_FOLDER = 'WDPA_202105_effective_extent_reviewed_all2020_fix';
var EXPORT_FORMAT = 'GeoJSON';

var ISO3_LIST = [
  'AFG', 'AGO', 'BGD', 'BEN', 'BTN', 'BOL', 'BFA', 'BDI', 'CPV', 'KHM',
  'CMR', 'CAF', 'TCD', 'COM', 'COD', 'COG', 'CIV', 'DJI', 'EGY', 'SLV',
  'ERI', 'SWZ', 'ETH', 'GMB', 'GHA', 'GIN', 'GNB', 'HTI', 'HND', 'IND',
  'IDN', 'KEN', 'KIR', 'PRK', 'KGZ', 'LAO', 'LSO', 'LBR', 'MDG', 'MWI',
  'MLI', 'MRT', 'FSM', 'MDA', 'MNG', 'MAR', 'MOZ', 'MMR', 'NPL', 'NIC',
  'NER', 'NGA', 'PAK', 'PNG', 'PHL', 'RWA', 'STP', 'SEN', 'SLE', 'SLB',
  'SOM', 'SSD', 'SDN', 'SYR', 'TJK', 'TZA', 'TLS', 'TGO', 'TUN', 'UGA',
  'UKR', 'UZB', 'VUT', 'VNM', 'PSE', 'YEM', 'ZMB', 'ZWE'
];

var SPECIAL_PSE_SPLITS = {
  '118': 'Gaza',
  '129': 'West Bank'
};

var ADM0 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM0');
var ADM1 = ee.FeatureCollection('WM/geoLab/geoBoundaries/600/ADM1');

var WDPA_BASE = ee.FeatureCollection('WCMC/WDPA/202105/polygons')
  .filter(ee.Filter.inList('STATUS', ['Designated', 'Established', 'Inscribed']))
  .filter(ee.Filter.neq('DESIG_ENG', 'UNESCO-MAB Biosphere Reserve'))
  .filter(ee.Filter.neq('MARINE', '2'));

function standardCountrySlice(iso) {
  return WDPA_BASE
    .filter(ee.Filter.or(
      ee.Filter.eq('ISO3', iso),
      ee.Filter.eq('PARENT_ISO', iso)
    ))
    .map(function(feature) {
      return feature
        .set('export_iso3', iso)
        .set('export_type', 'country');
    });
}

function pseSplitSlice(splitIso) {
  var splitName = SPECIAL_PSE_SPLITS[splitIso];
  var splitRegion = ADM1
    .filter(ee.Filter.eq('shapeGroup', 'PSE'))
    .filter(ee.Filter.eq('shapeName', splitName));

  return WDPA_BASE
    .filter(ee.Filter.or(
      ee.Filter.eq('ISO3', 'PSE'),
      ee.Filter.eq('PARENT_ISO', 'PSE')
    ))
    .filterBounds(splitRegion.geometry())
    .map(function(feature) {
      return feature
        .set('export_iso3', splitIso)
        .set('export_parent_iso3', 'PSE')
        .set('export_subunit', splitName)
        .set('export_type', 'pse_split');
    });
}

function paSliceForIso(iso) {
  if (SPECIAL_PSE_SPLITS[iso]) {
    return pseSplitSlice(iso);
  }
  return standardCountrySlice(iso);
}

ISO3_LIST.forEach(function(iso) {
  var fc = paSliceForIso(iso);

  print(iso + ' export count', fc.size());

  Export.table.toDrive({
    collection: fc,
    description: 'WDPA_202105_' + iso,
    folder: EXPORT_FOLDER,
    fileFormat: EXPORT_FORMAT
  });
});