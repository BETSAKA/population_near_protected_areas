# R reproduction plan for reviewed GEE workflow

## Scope

This note documents the attempt to reproduce the reviewed GEE workflow in R.

- requested package branch initially checked: `fBedecarrats/mapme.biodiversity@add-ghsl-test2`
- original GEE workflow: `code/reviewed_compute_land_pop_estimates_GEE.js`
- target fixed workflow: inferred from the issue in the original script because `code/reviewed_compute_land_pop_estimates_GEE_all2020_fix.js` is not present in the repository or local git history

The workflow was first prototyped with `mapme.biodiversity`, then refactored to direct raster access because the package branch exposed the wrong effective raster products for faithful reproduction.

## Confirmed issue in the reviewed GEE script

The original reviewed GEE script enforces exclusivity only inside each scenario (`Confirmed_2000`, `Confirmed_2020`, `Unknown_Year`).

- Within a scenario, the `claimed` mask is updated category by category, so `strict`, `nonStrict`, `unknownCat`, and their 10 km belts are mutually exclusive.
- Across scenarios, the `claimed` mask is reset at the start of each scenario loop.
- The paper code in `code/reviewed_produce_table_and_figures.R` reconstructs the 2020 "all PAs" quantity by adding `Confirmed_2020` and `Unknown_Year` after the fact.
- That post hoc addition can double count pixels that are claimed in both scenarios.

The proposed R fix therefore computes a direct `All_2020_Fixed` scenario where the WDPA subset is:

- `STATUS_YR > 0 & STATUS_YR <= 2020`
- or `STATUS_YR == 0`

and a single `claimed` geometry is carried across all categories in that combined scenario.

## WDPA source choice

Two WDPA candidates were checked.

### Primary source kept

`data/WDPA_2020_05_GEE`

- per-country shapefiles named `WDPA_202105_<ISO>.shp`
- schema matches the GEE filters exactly: `ISO3`, `STATUS`, `STATUS_YR`, `DESIG_ENG`, `MARINE`, `IUCN_CAT`, `WDPAID`, `NAME`
- month matches the GEE collection path `WCMC/WDPA/202105/polygons`

### Secondary source rejected as primary input

`s3://fbedecarrats/WDPA/source_data/WDPA_2021.zip`

- accessible with configured AWS credentials
- archive contents are stamped `WDPA_Oct2021_*`
- therefore it is an October 2021 release, not the May 2021 release used in GEE
- overlapping countries differ materially from the May files, so it cannot be treated as equivalent input

This October 2021 archive is only used as a fallback for countries absent from the local May export when no better local source exists.

## Boundary source choice

geoBoundaries are downloaded from the public `gbOpen` API / GitHub release URLs.

- standard countries use `shapeGroup == ISO3`
- Palestine is handled through `PSE` ADM1 boundaries and split into:
	- `118` = Gaza
	- `129` = West Bank

## Implemented R scripts

- `code/replicate_gee_mapme_common.R`
	- shared helpers
	- WDPA loading
	- geoBoundaries loading
	- ESA WorldCover 2020 land mask loading from public S3 tiles
	- direct population raster access through local cache, S3, or public HTTP sources
	- category hierarchy and exclusive buffer logic

- `code/replicate_gee_mapme_original.R`
	- reproduces the reviewed scenario structure:
		- `Confirmed_2000`
		- `Confirmed_2020`
		- `Unknown_Year`

- `code/replicate_gee_mapme_all2020_fix.R`
	- computes a direct `All_2020_Fixed` scenario with a single exclusivity pass

- `code/replicate_gee_mapme_compare.R`
	- reconstructs the original all-2020 quantity by summing `Confirmed_2020 + Unknown_Year`
	- compares that reconstructed value with the direct fixed scenario

## Direct raster source strategy

The current helper no longer depends on `mapme.biodiversity` for raster access.

- WorldPop is read from the public country-level 100 m files:
	- `https://data.worldpop.org/GIS/Population/Global_2000_2020/<year>/<ISO>/<iso>_ppp_<year>.tif`
- GHSL is read from the public 3 arc-second archives:
	- `https://jeodpp.jrc.ec.europa.eu/.../GHS_POP_E<year>_GLOBE_R2023A_4326_3ss_V1_0.zip`
- both resource families are staged to:
	- `s3://projet-betsaka/diffusion/population_pas/`

The staging script is:

- `code/stage_population_resources_to_s3.sh`

## Important reproducibility limitation

Even after moving off `mapme.biodiversity`, the public raster sources still do not match the effective raster sources used in the reviewed GEE workflow closely enough for exact reproduction.

- direct public WorldPop 100 m country rasters improve source fidelity relative to the package branch.
- direct public GHSL 3 arc-second rasters improve source fidelity relative to the package branch.
- however, the reviewed GEE script still uses Earth Engine assets and Earth Engine resampling / masking behavior at `scale = 100`.

In a narrow validation on Afghanistan / Kandahar / `Confirmed_2000`:

- reviewed GHSL `pop_total`: about `896,521`
- R reproduction GHSL `pop_total` with direct public `3ss` raster: about `774,682`
- reviewed WorldPop `pop_total`: about `710,140`
- R reproduction WorldPop `pop_total` with direct public `100m` raster: about `820,328`

That means the masking logic can be tested in R and the direct-source route is preferable to the package branch, but exact consistency with the repository's reviewed outputs is still not achievable from public files alone because the underlying Earth Engine inputs or processing behavior differ.

## Execution examples

Run a small subset after staging resources:

```bash
code/stage_population_resources_to_s3.sh
Rscript code/replicate_gee_mapme_original.R AFG,UKR,MDA,118,129
Rscript code/replicate_gee_mapme_all2020_fix.R AFG,UKR,MDA,118,129
Rscript code/replicate_gee_mapme_compare.R \
	data/Output_R_mapme_reviewed_original \
	data/Output_R_mapme_all2020_fix \
	data/tests/mapme_original_vs_fix_comparison.csv
```

Run the full country list:

```bash
Rscript code/replicate_gee_mapme_original.R
Rscript code/replicate_gee_mapme_all2020_fix.R
Rscript code/replicate_gee_mapme_compare.R
```

## Current status

- The bug in the reviewed GEE logic is confirmed.
- The direct all-2020 fix is implemented in R.
- The R workflow is reproducible from direct public resources plus local WDPA inputs.
- Exact replication of the reviewed population totals remains blocked by differences between public raster products and the effective Earth Engine inputs.
