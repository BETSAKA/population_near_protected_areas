# Load librairies --------------------------------------------------------

library(tidyverse)
library(fs)

# Load data --------------------------------------------------------------

data_dir <- "data/GEE_PA_Refactored_2026"
csv_files <- dir_ls(data_dir, glob = "*.csv")
combined_data <- map_df(
  csv_files,
  ~ {
    source_name <- str_extract(path_file(.x), "(?<=_)(GHSL|WP)(?=\\.csv)")
    read_csv(
      .x,
      col_types = cols(iso3 = col_character(), .default = col_guess()),
      show_col_types = FALSE
    ) |>
      mutate(source = source_name, .after = iso3)
  }
)

# Consolidate data -------------------------------------------------------

consolidated_data <- combined_data |>
  mutate(
    iso3 = case_when(
      iso3 %in% c("118", "129") ~ "PSE",
      TRUE ~ iso3
    )
  ) |>
  summarize(
    adm_level = first(adm_level),
    across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
    .by = c(iso3, source, scenario, pop_year)
  )


# Compare with previous data ---------------------------------------------

# loads pa_pop_worldpop, pa_pop_ghsl, pa_pop_total, pa_pop_total_no_india
# from older file
load("results/pa_pop_total.rds")

bdi_new <- consolidated_data |>
  filter(iso3 == "BDI")
bdi_old_wp <- pa_pop_worldpop |>
  filter(ISO3 == "BDI")
bdi_old_ghsl <- pa_pop_ghsl |>
  filter(ISO3 == "BDI")

# Load comparison data -------------------------------------------------------

library(arrow)
library(geoarrow)
library(sf)
library(tmap)

wdpa_2021_bdi <- open_dataset("data/WDPA_2021.parquet") |>
  filter(ISO3 == "BDI") |>
  collect() |>
  mutate(
    geometry = st_as_sfc(geometry),
    across(where(is.character), ~ iconv(., to = "UTF-8", sub = ""))
  ) |>
  st_make_valid()

wdpa_2021_bdi |>
  st_union() |>
  st_area() |>
  units::set_units("km^2")

tmap_mode("view")
tm_shape(wdpa_2021_bdi) +
  tm_polygons(col = "blue", alpha = 0.5, border.alpha = 0.8) +
  tm_shape(wdpa_2025_bdi) +
  tm_polygons(col = "red", alpha = 0.5, border.alpha = 0.8)

library(wdpar)

# Chargement des données WDPA
wdpa_2025_bdi <- wdpa_fetch("BDI", wait = TRUE)

bdi_new %>%
  select(starts_with("area")) %>%
  head()
