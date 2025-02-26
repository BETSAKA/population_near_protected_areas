// low and low middle income countries according to World Bank in 2020, source:
// https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
var isoList = [ 
  "AFG", "AGO", "BGD", "BEN", "BTN", "BOL", "BFA", "BDI", "CPV", "KHM",
  "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "EGY", "SLV",
  "ERI", "SWZ", "ETH", "GMB", "GHA", "GIN", "GNB", "HTI", "HND", "IND",
  "IDN", "KEN", "KIR", "PRK", "KGZ", "LAO", "LSO", "LBR", "MDG", "MWI",
  "MLI", "MRT", "FSM", "MDA", "MNG", "MAR", "MOZ", "MMR", "NPL", "NIC",
  "NER", "NGA", "PAK", "PNG", "PHL", "RWA", "STP", "SEN", "SLE", "SLB",
  "SOM", "SSD", "SDN", "SYR", "TJK", "TZA", "TLS", "TGO", "TUN", "UGA",
  "UKR", "UZB", "VUT", "VNM", "PSE", "YEM", "ZMB", "ZWE"];

// Define an area threshold (in km²) for ADM1 subdivision
var areaThreshold = 1e6; // i.e., 1,000,000 km²

/**** STEP 1: LOAD COUNTRY BOUNDARIES (ADM0) FROM GEOBOUNDARIES ****/
var countries = ee.FeatureCollection("WM/geoLab/geoBoundaries/600/ADM0")
  .filter(ee.Filter.inList("shapeGroup", isoList));

// Add an "area_km2" property for each country
countries = countries.map(function(feat) {
  var a = feat.geometry().area().divide(1e6); // area in km²
  return feat.set("countryArea_km2", a);
});

// Split countries into big vs. small
var bigCountries = countries.filter(ee.Filter.gt("countryArea_km2", areaThreshold));
var smallCountries = countries.filter(ee.Filter.lte("countryArea_km2", areaThreshold));

print("Big countries count:", bigCountries.size());
print("Small countries count:", smallCountries.size());

/**** STEP 2: LOAD & FILTER WDPA DATA (BASE FILTERS) ****/
// We'll do the main status/designation filters first.
var wdpaBase = ee.FeatureCollection("WCMC/WDPA/current/polygons")
  .filter(ee.Filter.inList("STATUS", ["Designated", "Inscribed", "Established"]))
  .filter(ee.Filter.neq("DESIG_ENG", "UNESCO-MAB Biosphere Reserve"));

/**** Additional filters by STATUS_YR ****/
// We create two separate collections for year-based analysis:
var wdpa2000 = wdpaBase.filter(ee.Filter.lte("STATUS_YR", 2000));
var wdpa2020 = wdpaBase.filter(ee.Filter.lte("STATUS_YR", 2020));

/**** STEP 3: LOAD WORLDPOP IMAGES FOR 2000 & 2020 AND MOSAIC ****/
var pop2000 = ee.ImageCollection("WorldPop/GP/100m/pop")
  .filter(ee.Filter.eq("year", 2000))
  .select("population")
  .mosaic();

var pop2020 = ee.ImageCollection("WorldPop/GP/100m/pop")
  .filter(ee.Filter.eq("year", 2020))
  .select("population")
  .mosaic();

/**** STEP 4: HELPER FUNCTIONS ****/
function getPopulation(image, mask, region) {
  var maskedPop = image.updateMask(mask);
  var popDict = maskedPop.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: region,
    scale: 100,
    maxPixels: 1e13,
    tileScale: 4
  });
  return ee.Number(popDict.get("population"));
}

function getArea(mask, region) {
  var areaImg = ee.Image.pixelArea().divide(1e6).updateMask(mask);
  var areaDict = areaImg.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: region,
    scale: 100,
    maxPixels: 1e13,
    tileScale: 4
  });
  return ee.Number(areaDict.get("area"));
}

function getPopulationNoMask(image, region) {
  var popDict = image.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: region,
    scale: 100,
    maxPixels: 1e13,
    tileScale: 4
  });
  return ee.Number(popDict.get("population"));
}

/**** STEP 5A: PROCESS ONE COUNTRY AT ADM0 LEVEL ****/
function processCountry(countryFeat) {
  var iso = countryFeat.getString("shapeGroup");
  var countryGeom = countryFeat.geometry();

  // For 2000: WDPA polygons with STATUS_YR <= 2000
  var wdpa2000InCtry = wdpa2000.filterBounds(countryGeom);
  // Rasterize
  var paMask2000 = wdpa2000InCtry
    .reduceToImage({
      properties: ["WDPAID"], 
      reducer: ee.Reducer.count()
    })
    .gt(0)
    .clip(countryGeom);

  // For 2020: WDPA polygons with STATUS_YR <= 2020
  var wdpa2020InCtry = wdpa2020.filterBounds(countryGeom);
  var paMask2020 = wdpa2020InCtry
    .reduceToImage({
      properties: ["WDPAID"], 
      reducer: ee.Reducer.count()
    })
    .gt(0)
    .clip(countryGeom);

  // Count of WDPA features (not pixel-based) for each year
  var paCount2000 = wdpa2000InCtry.size();
  var paCount2020 = wdpa2020InCtry.size();

  // 10 km buffer for each year
  var paMaskBuffered2000 = paMask2000.focal_max({
    kernel: ee.Kernel.euclidean(10000, "meters")
  }).clip(countryGeom);

  var paMaskBuffered2020 = paMask2020.focal_max({
    kernel: ee.Kernel.euclidean(10000, "meters")
  }).clip(countryGeom);

  // Compute area for each year
  var paArea2000 = getArea(paMask2000, countryGeom);
  var paArea2020 = getArea(paMask2020, countryGeom);
  var paBufferArea2000 = getArea(paMaskBuffered2000, countryGeom);
  var paBufferArea2020 = getArea(paMaskBuffered2020, countryGeom);

  var countryArea = countryFeat.getNumber("countryArea_km2");

  // Population for year 2000 inside 2000 PAs
  var pop2000inPA = getPopulation(pop2000, paMask2000, countryGeom);
  var pop2000inPA10 = getPopulation(pop2000, paMaskBuffered2000, countryGeom);
  // Population for year 2020 inside 2020 PAs
  var pop2020inPA = getPopulation(pop2020, paMask2020, countryGeom);
  var pop2020inPA10 = getPopulation(pop2020, paMaskBuffered2020, countryGeom);

  // Total population in the country for each year
  var pop2000Total = getPopulationNoMask(pop2000, countryGeom);
  var pop2020Total = getPopulationNoMask(pop2020, countryGeom);

  // Return a feature with all relevant properties
  return ee.Feature(null, {
    shapeGroup: iso,
    adm_level: 0,
    adm_name: countryFeat.getString("shapeName"),

    pa_count_2000: paCount2000,
    pa_area_2000_km2: paArea2000,
    pa_area10km_2000_km2: paBufferArea2000,
    pop2000_in_pa: pop2000inPA,
    pop2000_in_pa10: pop2000inPA10,
    pop2000_total: pop2000Total,

    pa_count_2020: paCount2020,
    pa_area_2020_km2: paArea2020,
    pa_area10km_2020_km2: paBufferArea2020,
    pop2020_in_pa: pop2020inPA,
    pop2020_in_pa10: pop2020inPA10,
    pop2020_total: pop2020Total,

    country_area_km2: countryArea
  });
}

/**** STEP 5B: PROCESS ONE COUNTRY AT ADM1 LEVEL (SAME LOGIC) ****/
var adm1 = ee.FeatureCollection("WM/geoLab/geoBoundaries/600/ADM1");

function processCountryByADM1(countryFeat) {
  var iso = countryFeat.getString("shapeGroup");
  var countryGeom = countryFeat.geometry();
  var countryArea = countryFeat.getNumber("countryArea_km2");
  var countryName = countryFeat.getString("shapeName");

  // Filter ADM1 to match shapeGroup
  var adm1ForCountry = adm1.filter(ee.Filter.eq("shapeGroup", iso));

  var adm1Results = adm1ForCountry.map(function(adm1Feat) {
    var adm1Geom = adm1Feat.geometry();
    var adm1Name = adm1Feat.getString("shapeName"); 

    // WDPA <= 2000
    var wdpa2000InAdm1 = wdpa2000.filterBounds(adm1Geom);
    var paMask2000 = wdpa2000InAdm1
      .reduceToImage({
        properties: ["WDPAID"],
        reducer: ee.Reducer.count()
      })
      .gt(0)
      .clip(adm1Geom);
    var paCount2000 = wdpa2000InAdm1.size();
    var paMaskBuffered2000 = paMask2000.focal_max({
      kernel: ee.Kernel.euclidean(10000, "meters")
    }).clip(adm1Geom);

    // WDPA <= 2020
    var wdpa2020InAdm1 = wdpa2020.filterBounds(adm1Geom);
    var paMask2020 = wdpa2020InAdm1
      .reduceToImage({
        properties: ["WDPAID"],
        reducer: ee.Reducer.count()
      })
      .gt(0)
      .clip(adm1Geom);
    var paCount2020 = wdpa2020InAdm1.size();
    var paMaskBuffered2020 = paMask2020.focal_max({
      kernel: ee.Kernel.euclidean(10000, "meters")
    }).clip(adm1Geom);

    // Areas
    var paArea2000 = getArea(paMask2000, adm1Geom);
    var paArea2020 = getArea(paMask2020, adm1Geom);
    var paBufferArea2000 = getArea(paMaskBuffered2000, adm1Geom);
    var paBufferArea2020 = getArea(paMaskBuffered2020, adm1Geom);

    var adm1Area = adm1Geom.area().divide(1e6);

    // Population 2000 in 2000 PAs
    var pop2000inPA = getPopulation(pop2000, paMask2000, adm1Geom);
    var pop2000inPA10 = getPopulation(pop2000, paMaskBuffered2000, adm1Geom);
    var pop2000Total = getPopulationNoMask(pop2000, adm1Geom);

    // Population 2020 in 2020 PAs
    var pop2020inPA = getPopulation(pop2020, paMask2020, adm1Geom);
    var pop2020inPA10 = getPopulation(pop2020, paMaskBuffered2020, adm1Geom);
    var pop2020Total = getPopulationNoMask(pop2020, adm1Geom);

    return ee.Feature(null, {
      shapeGroup: iso,
      adm_level: 1,
      country_name: countryName,
      adm1_name: adm1Name,

      pa_count_2000: paCount2000,
      pa_area_2000_km2: paArea2000,
      pa_area10km_2000_km2: paBufferArea2000,
      pop2000_in_pa: pop2000inPA,
      pop2000_in_pa10: pop2000inPA10,
      pop2000_total: pop2000Total,

      pa_count_2020: paCount2020,
      pa_area_2020_km2: paArea2020,
      pa_area10km_2020_km2: paBufferArea2020,
      pop2020_in_pa: pop2020inPA,
      pop2020_in_pa10: pop2020inPA10,
      pop2020_total: pop2020Total,

      country_area_km2: countryArea,
      adm1_area_km2: adm1Area
    });
  });

  return adm1Results;
}

/**** STEP 6: MAP OVER BIG & SMALL COUNTRIES, COMBINE RESULTS ****/
var bigResults = bigCountries.map(processCountryByADM1).flatten();
var smallResults = smallCountries.map(processCountry);
var allResults = bigResults.merge(smallResults);

print("allResults:", allResults.limit(50));

// Export final table
Export.table.toDrive({
  collection: allResults,
  description: "Population_PAs_ADM0_ADM1_YearlyWDPA",
  fileFormat: "CSV"
});