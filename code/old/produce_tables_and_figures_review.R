# Load required libraries
library(tidyverse)
library(readxl)
library(gt)

# Parameters
options(scipen = 999)
folder <- "data/GEE_PA_pop_2025-12-21"
expected_country_number <- 76
discarded_countries <- c("SSD", "TLS") # countries that did not exist in 2000

# Load the data -----------------------------------------------------------

# World bank country list by income classification
wb_country_list <- read_excel(
  "data/OGHIST.xlsx",
  sheet = "Country_cat",
  n_max = 219
)

# GEE outputs
paths <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

pa_pop_raw <- map_dfr(paths, \(p) {
  file <- basename(p)
  tibble(
    file = file,
    ISO3_file = str_match(file, "^PA_Pop_([A-Z]{3})_")[, 2],
    pop_source = str_match(file, "_(WP|GHSL)\\.csv$")[, 2]
  ) |>
    bind_cols(readr::read_csv(p, show_col_types = FALSE))
}) |>
  mutate(
    pop_source = factor(pop_source, levels = c("GHSL", "WP")),
    ISO3 = coalesce(ISO3_file, iso3, shapeGroup)
  )

country_list_gee <- c(²
  "BEN",
  "BDI",
  "DJI",
  "AFG",
  "AGO",
  "BGD",
  "BTN",
  "BOL",
  "BFA",
  "CPV",
  "KHM",
  "CMR",
  "CAF",
  "TCD",
  "COM",
  "COD",
  "COG",
  "CIV",
  "EGY",
  "SLV",
  "ERI",
  "SWZ",
  "ETH",
  "GMB",
  "GHA",
  "GIN",
  "GNB",
  "HTI",
  "HND",
  "IND",
  "IDN",
  "KEN",
  "KIR",
  "PRK",
  "KGZ",
  "LAO",
  "LSO",
  "LBR",
  "MDG",
  "MWI",
  "MLI",
  "MRT",
  "FSM",
  "MDA",
  "MNG",
  "MAR",
  "MOZ",
  "MMR",
  "NPL",
  "NIC",
  "NER",
  "NGA",
  "PAK",
  "PNG",
  "PHL",
  "RWA",
  "STP",
  "SEN",
  "SLE",
  "SLB",
  "SOM",
  "SSD",
  "SDN",
  "SYR",
  "TJK",
  "TZA",
  "TLS",
  "TGO",
  "TUN",
  "UGA",
  "UKR",
  "UZB",
  "VUT",
  "VNM",
  "PSE",
  "YEM",
  "ZMB",
  "ZWE"
) %>%
  unique() %>%
  setdiff(discarded_countries)


# Check country completeness ----------------------------------------------

# Retain "Low" and "Low-Medium" in 2020
llm_2020 <- wb_country_list |>
  filter(FY20 %in% c("L", "LM")) |>
  filter(!ISO3 %in% discarded_countries)

iso3_llm <- llm_2020 %>%
  distinct(ISO3) %>%
  pull(ISO3) %>%
  sort()

# In GEE list but not LLM 2020
gee_not_llm <- setdiff(country_list_gee, iso3_llm)
if (length(gee_not_llm) > 0) {
  print(gee_not_llm)
  stop("Some countries included listed in GEE were not in World Bank list")
}

# In LLM 2020 but not GEE list
llm_not_gee <- setdiff(iso3_llm, country_list_gee)
if (length(llm_not_gee) > 0) {
  print(llm_not_gee)
  stop("Some countries included in World Bank list were not listed in GEE")
}

# Ensure that we have no duplicates
dup_check <- pa_pop_raw %>%
  count(ISO3, pop_source, adm_level) %>%
  filter(n > 1)

# Check if duplicates and stop in that case
if (nrow(dup_check) > 0) {
  print(dup_check)
  stop(
    "Duplicate ADM units detected: more than one row per (ISO3, pop_source, adm_level)."
  )
}

keys <- pa_pop_raw %>%
  distinct(ISO3, pop_source, adm_level)

# Number of countries (unique ISO3)
n_countries <- keys %>%
  distinct(ISO3) %>%
  nrow()

# Check that all expected ISO3 are present (and only those, if you want)
iso3_in_data <- pa_pop_raw %>%
  distinct(ISO3) %>%
  pull(ISO3) %>%
  sort()

iso3_expected <- sort(unique(country_list_gee))

missing_iso3 <- setdiff(iso3_expected, iso3_in_data)
extra_iso3 <- setdiff(iso3_in_data, iso3_expected)

if (length(missing_iso3) > 0) {
  cat("Missing ISO3 (in country_list_gee but not in data):\n")
  print(missing_iso3)
  stop("Some expected countries are missing from the loaded CSVs.")
} else {
  cat("OK: all countries in country_list_gee are present in the data.\n")
}

# Check that all countries have both sources
source_coverage <- keys %>%
  distinct(ISO3, pop_source) %>%
  count(ISO3, name = "n_sources") %>%
  mutate(has_both_sources = n_sources == 2)

if (any(!source_coverage$has_both_sources)) {
  cat("Countries with missing source(s):\n")
  source_coverage %>%
    filter(!has_both_sources) %>%
    arrange(ISO3) %>%
    print(n = Inf)
  cat("\n")
}


# Consolidate -------------------------------------------------------------

# List additive fields
sum_vars <- names(pa_pop_raw) %>%
  keep(~ str_detect(.x, "^(a_|p_)")) %>% # keep a_ and p_ only for this table
  union("pop_total_2020")

pa_pop_country <- pa_pop_raw %>%
  group_by(ISO3, pop_source) %>%
  summarise(
    across(all_of(sum_vars), ~ sum(.x, na.rm = TRUE)),
    country_area_km2 = sum(adm_area, na.rm = TRUE),
    country = first(shapeName),
    .groups = "drop"
  )

# Build long (ISO3 x source) with pct columns
pa_pop_2020_long <- pa_pop_country %>%
  transmute(
    ISO3,
    country,
    pop_source,
    country_area_km2,
    pop_total_2020,

    land_st_km2 = a_C2020_st,
    land_all_km2 = a_C2020_st + a_C2020_ns + a_C2020_uk,

    pop_in_st = p_C2020_st,
    pop_in_all = p_C2020_st + p_C2020_ns + p_C2020_uk,

    pop_10_st = p_C2020_st10,
    pop_10_all = p_C2020_st10 + p_C2020_ns10 + p_C2020_uk10
  ) %>%
  mutate(
    land_st_pct = land_st_km2 / country_area_km2 * 100,
    land_all_pct = land_all_km2 / country_area_km2 * 100,
    pop_in_st_pct = pop_in_st / pop_total_2020 * 100,
    pop_in_all_pct = pop_in_all / pop_total_2020 * 100,
    pop_10_st_pct = pop_10_st / pop_total_2020 * 100,
    pop_10_all_pct = pop_10_all / pop_total_2020 * 100
  )

# Total area once (avoid double counting)
country_area_total <- pa_pop_country %>%
  filter(pop_source == "GHSL") %>%
  summarise(country_area_km2 = sum(country_area_km2, na.rm = TRUE)) %>%
  pull(country_area_km2)

# Total rows by source (recompute pct from totals)
pa_pop_total_long <- pa_pop_2020_long %>%
  group_by(pop_source) %>%
  summarise(
    ISO3 = "Total",
    country = "Total",
    country_area_km2 = country_area_total,
    pop_total_2020 = sum(pop_total_2020, na.rm = TRUE),

    land_st_km2 = sum(land_st_km2, na.rm = TRUE),
    land_all_km2 = sum(land_all_km2, na.rm = TRUE),
    pop_in_st = sum(pop_in_st, na.rm = TRUE),
    pop_in_all = sum(pop_in_all, na.rm = TRUE),
    pop_10_st = sum(pop_10_st, na.rm = TRUE),
    pop_10_all = sum(pop_10_all, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    land_st_pct = land_st_km2 / country_area_km2 * 100,
    land_all_pct = land_all_km2 / country_area_km2 * 100,
    pop_in_st_pct = pop_in_st / pop_total_2020 * 100,
    pop_in_all_pct = pop_in_all / pop_total_2020 * 100,
    pop_10_st_pct = pop_10_st / pop_total_2020 * 100,
    pop_10_all_pct = pop_10_all / pop_total_2020 * 100
  ) %>%
  select(names(pa_pop_2020_long))

pa_pop_2020_long2 <- bind_rows(pa_pop_total_long, pa_pop_2020_long)

# Wide: keep land % once (it is identical across sources given same WDPA+area)
pa_pop_2020_wide <- pa_pop_2020_long2 %>%
  select(
    ISO3,
    country,
    country_area_km2,
    land_st_pct,
    land_all_pct,
    pop_source,
    pop_in_st_pct,
    pop_in_all_pct,
    pop_10_st_pct,
    pop_10_all_pct
  ) %>%
  pivot_wider(
    id_cols = c(ISO3, country, country_area_km2, land_st_pct, land_all_pct),
    names_from = pop_source,
    values_from = c(
      pop_in_st_pct,
      pop_in_all_pct,
      pop_10_st_pct,
      pop_10_all_pct
    )
  )

table_2020 <- pa_pop_2020_wide %>%
  # put Total first, then alphabetical
  arrange(desc(ISO3 == "Total"), country) %>%
  select(
    country,
    land_st_pct,
    land_all_pct,
    pop_in_st_pct_GHSL,
    pop_in_all_pct_GHSL,
    pop_10_st_pct_GHSL,
    pop_10_all_pct_GHSL,
    pop_in_st_pct_WP,
    pop_in_all_pct_WP,
    pop_10_st_pct_WP,
    pop_10_all_pct_WP
  ) %>%
  gt() %>%
  tab_header(
    title = "Table 1: PA Coverage and Population Proximity (2020)",
    subtitle = "Percentage of land and population within and near protected areas"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  cols_label(
    country = "Country",
    land_st_pct = "Strict",
    land_all_pct = "All",
    pop_in_st_pct_GHSL = "Strict",
    pop_in_all_pct_GHSL = "All",
    pop_10_st_pct_GHSL = "Strict",
    pop_10_all_pct_GHSL = "All",
    pop_in_st_pct_WP = "Strict",
    pop_in_all_pct_WP = "All",
    pop_10_st_pct_WP = "Strict",
    pop_10_all_pct_WP = "All"
  ) %>%
  # Level 2: within vs 10km
  tab_spanner(
    label = "within PAs",
    columns = c(pop_in_st_pct_GHSL, pop_in_all_pct_GHSL),
    id = "ghsl_within"
  ) %>%
  tab_spanner(
    label = "10km of PAs",
    columns = c(pop_10_st_pct_GHSL, pop_10_all_pct_GHSL),
    id = "ghsl_10km"
  ) %>%
  tab_spanner(
    label = "within PAs",
    columns = c(pop_in_st_pct_WP, pop_in_all_pct_WP),
    id = "wp_within"
  ) %>%
  tab_spanner(
    label = "10km of PAs",
    columns = c(pop_10_st_pct_WP, pop_10_all_pct_WP),
    id = "wp_10km"
  ) %>%
  tab_spanner(
    label = "within PAs",
    columns = c(land_st_pct, land_all_pct),
    id = "land_within"
  ) %>%
  # Level 1: source blocks
  tab_spanner(
    label = "% land within (WDPA)",
    spanners = "land_within"
  ) %>%
  tab_spanner(
    label = "% population (GHSL)",
    spanners = c("ghsl_within", "ghsl_10km")
  ) %>%
  tab_spanner(
    label = "% population (WorldPop)",
    spanners = c("wp_within", "wp_10km")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(2),
    column_labels.padding = px(2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = country == "Total")
  ) %>%
  # Thin vertical lines
  tab_style(
    style = cell_borders(sides = "right", weight = px(0.5)),
    locations = cells_body(columns = everything())
  ) %>%
  # Thick separators after Country, Land block, GHSL block
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = cells_body(
      columns = c(
        country,
        land_all_pct,
        pop_10_all_pct_GHSL
      )
    )
  )
table_2020
