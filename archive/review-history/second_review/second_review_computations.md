You are helping revise an R-based replication package for an ERL paper on population near protected areas in LMICs. The reviewer asked for additional computations to clarify whether the larger population near non-strict protected areas is due to there being more/larger non-strict PAs, or to different settlement patterns around PA types.

Work in the existing project repository. Inspect the current R scripts, especially the script that produces manuscript tables and figures, and reuse the existing data-processing conventions. Do not rewrite the whole pipeline. Add a clean, reproducible section that computes the missing reviewer-requested quantities and saves them as CSV, RDS, and a formatted table if the project already uses gt.

Context and definitions to preserve:

* The sample is the 75 low- and lower-middle-income countries used in the revised manuscript, excluding India, South Sudan, and Timor-Leste.
* Use GHSL as the main population source.
* Use the May 2021 WDPA snapshot, with the same filters as the manuscript: retain STATUS equal to Designated, Established, or Inscribed; exclude UNESCO-MAB Biosphere Reserves; exclude purely marine PAs.
* PA categories must follow the manuscript:

  * strict = IUCN Ia, Ib, II, III
  * non-strict = IUCN IV, V, VI
  * unknown = Not Reported, Not Applicable, Not Assigned, NA, or otherwise missing/unclassified IUCN category
* “Inside or within 10 km” means population inside PAs plus population in the 10 km buffer ring.
* For the 2020 cross-sectional table, use all PAs known to exist in the May 2021 WDPA release, including PAs with missing designation year. If the existing GEE output already has a scenario for “all 2020 including missing year,” use that. If it separates Confirmed_2020 and Unknown_Year, verify whether those masks were generated jointly and exclusively before summing. Do not naively add independently generated 10 km buffers if that would double count overlapping pixels.
* For 2000 comparisons, use only PAs with recorded designation years, as in the manuscript.

Main computation to add:

Create a category-level table for all 2020 PAs, GHSL, 75 LMICs excluding India, with one row each for strict, non-strict, unknown category, and all PAs. Include:

* n_pa: number of protected areas by category. Count distinct WDPAID if available; otherwise use the most stable unique PA identifier in the WDPA attributes. Report in a code comment which identifier was used.
* pa_area_km2: total terrestrial PA area by category, using the same area logic as the population analysis.
* pa_area_share_pct: PA area by category divided by total land area of the 75-country sample.
* pop_inside_million: population inside PAs by category.
* pop_inside_pct: population inside PAs by category divided by total GHSL population of the 75-country sample.
* pop_buffer10_million: population in the 10 km buffer ring, excluding population inside PAs.
* pop_inside_or_10km_million: pop_inside_million + pop_buffer10_million.
* pop_inside_or_10km_pct: population inside or within 10 km divided by total GHSL population of the 75-country sample.
* people_inside_or_10km_per_km2_pa: pop_inside_or_10km divided by pa_area_km2.
* people_buffer10_per_km2_pa: pop_buffer10 divided by pa_area_km2.
* optional, if buffer area is available or easy to compute consistently: buffer_area_km2 and pop_density_buffer10 = pop_buffer10 / buffer_area_km2.

Use clear labels suitable for the manuscript:

* Strict PAs (IUCN Ia-III)
* Non-strict PAs (IUCN IV-VI)
* Unknown IUCN category
* All PAs

Validation checks:

* The all-PAs row should reproduce the manuscript’s 2020 GHSL order of magnitude: about 60.7 million people inside PAs and about 902.1 million inside or within 10 km, corresponding to about 2.5% and 36.6% of the combined population.
* The strict, non-strict, and unknown rows should sum exactly or near-exactly to the all-PAs row if the category masks are exclusive. If not, diagnose and explain whether the difference comes from overlaps, non-exclusive masks, or scenario construction.
* No population share should exceed 100%. If any national or aggregate percentage exceeds 100%, stop and trace the source before saving final outputs.
* Check that pa_area_km2 is positive for all non-empty categories.
* Check whether non-strict PAs have more people near them mainly because pa_area_km2 is larger, n_pa is larger, or because people_inside_or_10km_per_km2_pa is higher.

Also revise the existing manuscript Table 1 outputs by adding inside-only category rows. Produce a compact table with these rows:

* Inside PAs only, all categories
* Inside strict PAs only
* Inside non-strict PAs only
* Inside unknown-category PAs only
* Inside PAs or within 10 km, all categories
* Inside or within 10 km of strict PAs
* Inside or within 10 km of non-strict PAs
* Inside or within 10 km of unknown-category PAs

Columns:

* Exposure definition
* Population, millions
* Percent of 75-country GHSL population

Also compute and save the abstract numbers needed for the revision:

* total GHSL population of the 75-country sample in 2020
* total GHSL population of the 75-country sample in 2000, restricted consistently to the comparison denominator used in the manuscript
* 2020 population inside PAs and percent
* 2020 population inside or within 10 km and percent
* 2000 population inside PAs and percent, using only PAs with recorded designation years
* 2000 population inside or within 10 km and percent, using only PAs with recorded designation years
* India’s share of the LMIC population before exclusion, if the data needed are already in the repository; otherwise leave a clear TODO and do not hard-code it without checking the source.

Expected output files:

* results/reviewer_pa_category_standardization.csv
* results/reviewer_pa_category_standardization.rds
* results/reviewer_table1_extended.csv
* results/reviewer_abstract_numbers.csv
* results/reviewer_checks.txt
* if gt is used in the project, also save reviewer_pa_category_standardization.docx and reviewer_table1_extended.docx

Coding style:

* Use tidyverse.
* Keep the added code in a clearly marked section, for example “Reviewer 3: PA category standardization.”
* Avoid changing existing outputs unless necessary.
* Make the code robust to variable names by checking available columns and failing with an informative error if a required variable is missing.
* Add short comments explaining each denominator and whether the table uses all 2020 PAs or only PAs with recorded designation years.
* At the end, print a concise interpretation: whether the higher population near non-strict PAs remains higher after standardizing by PA area, and how much of the raw difference appears attributable to PA number/area versus nearby population intensity.
