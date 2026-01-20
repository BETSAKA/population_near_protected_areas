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

# Load previous data processing function
source("code/produce_table_and_figures.R")

# Reshape consolidated_data to match old structure
comparison_data <- consolidated_data |>
  filter(scenario %in% c("Confirmed_2000", "Confirmed_2020")) |>
  mutate(
    year = str_extract(scenario, "\\d{4}"),
    scenario_type = str_extract(scenario, "^[^_]+")
  ) |>
  select(-scenario_type, -adm_level) |>
  pivot_wider(
    names_from = c(year, source),
    values_from = c(starts_with("pop"), starts_with("pa_area")),
    names_sep = "_"
  )

# Join with old data for comparison
# Old data structure: pa_pop_worldpop and pa_pop_ghsl
old_combined <- full_join(
  pa_pop_worldpop |>
    select(ISO3, starts_with("pop2000"), starts_with("pop2020")),
  pa_pop_ghsl |> select(ISO3, starts_with("pop2000"), starts_with("pop2020")),
  by = "ISO3",
  suffix = c("_old_wp", "_old_ghsl")
)

# Compare values
data_comparison <- comparison_data |>
  left_join(old_combined, by = c("iso3" = "ISO3"))
