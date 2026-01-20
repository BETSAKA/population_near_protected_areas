# Load libraries ----------------------------------------------------------

library(tidyverse)
library(gt)
library(readxl)
library(writexl)
library(gtExtras)
library(ggplot2)
library(ggrepel)
library(scales)
library(cowplot)
library(stddiff)

# Paths ------------------------------------------------------------------

consolidated_path <- "data/pa_pop_consolidated.csv"
naidoo_path <- "data/Naidoo2019-TableS1.csv"
wb_path <- "data/OGHIST.xlsx"

# Load and prepare data ---------------------------------------------------

wb_country_list <- read_excel(wb_path, sheet = "Country_cat", n_max = 219)

llm_2020 <- wb_country_list |>
  filter(FY20 %in% c("L", "LM")) |>
  filter(ISO3 != "TLS", ISO3 != "SSD")

# Load consolidated dataset (adm level) -----------------------------------

new_joint_files_all <- list.files(
  "data/GEE_PA_Population_Analysis",
  pattern = "\\.csv$",
  full.names = TRUE
)

raw <- new_joint_files_all |>
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

# Expected key columns in raw (your names)
key_cols <- c("iso3", "country_name", "country_area_km2")
missing_key <- setdiff(key_cols, names(raw))
if (length(missing_key) > 0) {
  stop(paste0("Missing required key columns:\n", paste(missing_key, collapse = "\n")))
}

# Aggregate to country level ----------------------------------------------

sum_cols <- names(raw)[vapply(raw, is.numeric, logical(1))]
sum_cols <- setdiff(sum_cols, c("adm_level", "adm_area_km2", "country_area_km2"))

pa_pop <- raw |>
  filter(iso3 %in% llm_2020$ISO3) |>
  group_by(iso3, country_name) |>
  summarise(
    country_area_km2 = first(country_area_km2),
    across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  rename(
    ISO3 = iso3,
    country = country_name
  ) |>
  arrange(country)

# Expected key columns in raw
key_cols <- c("iso3", "country_name", "country_area_km2")
missing_key <- setdiff(key_cols, names(raw))
if (length(missing_key) > 0) {
  stop(paste0("Missing required key columns:\n", paste(missing_key, collapse = "\n")))
}

# Aggregate to country level (sum counts and areas across admin units) -----

# Columns to sum: all numeric except adm_level and adm_area_km2 (not needed for country totals)
sum_cols <- names(raw)[vapply(raw, is.numeric, logical(1))]
sum_cols <- setdiff(sum_cols, c("adm_level", "adm_area_km2", "country_area_km2"))

pa_pop <- raw |>
  group_by(iso3, country_name) |>
  summarise(
    country_area_km2 = first(country_area_km2),
    across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  rename(
    ISO3 = iso3,
    country = country_name
  ) |>
  filter(ISO3 %in% llm_2020$ISO3) |>
  arrange(country)

# Sanity check: required base columns for percent computations ------------
required_base <- c(
  "ISO3", "country", "country_area_km2",
  "pop2000_total_WP", "pop2020_total_WP",
  "pop2000_total_GHSL", "pop2020_total_GHSL"
)

missing_base <- setdiff(required_base, names(pa_pop))
if (length(missing_base) > 0) {
  stop(paste0("Missing required base columns after aggregation:\n", paste(missing_base, collapse = "\n")))
}

# Compute percentage columns explicitly -----------------------------------

types <- c("strict", "non", "all")
years <- c("2000", "2020")
sources <- c("WP", "GHSL")

# Land shares
for (t in types) {
  for (y in years) {
    a_col <- paste0("pa_area_", t, "_", y, "_km2")
    b_col <- paste0("pa_area10km_", t, "_", y, "_km2")
    
    if (!a_col %in% names(pa_pop)) stop(paste0("Missing column: ", a_col))
    if (!b_col %in% names(pa_pop)) stop(paste0("Missing column: ", b_col))
    
    pa_pop[[paste0(a_col, "_pct")]] <- pa_pop[[a_col]] / pa_pop$country_area_km2 * 100
    pa_pop[[paste0(b_col, "_pct")]] <- pa_pop[[b_col]] / pa_pop$country_area_km2 * 100
  }
}

# Population shares
for (t in types) {
  for (y in years) {
    for (s in sources) {
      tot_col <- paste0("pop", y, "_total_", s)
      
      inpa_col <- paste0("pop", y, "_in_pa_", t, "_", s)
      inpa10_col <- paste0("pop", y, "_in_pa10_", t, "_", s)
      
      if (!tot_col %in% names(pa_pop)) stop(paste0("Missing column: ", tot_col))
      if (!inpa_col %in% names(pa_pop)) stop(paste0("Missing column: ", inpa_col))
      if (!inpa10_col %in% names(pa_pop)) stop(paste0("Missing column: ", inpa10_col))
      
      pa_pop[[paste0(inpa_col, "_pct")]] <- pa_pop[[inpa_col]] / pa_pop[[tot_col]] * 100
      pa_pop[[paste0(inpa10_col, "_pct")]] <- pa_pop[[inpa10_col]] / pa_pop[[tot_col]] * 100
    }
  }
}

# Legacy objects for unchanged outputs (all PAs) --------------------------

pa_pop_worldpop <- pa_pop |>
  transmute(
    ISO3, country,
    country_area_km2,
    pop2000_total = pop2000_total_WP,
    pop2020_total = pop2020_total_WP,
    pa_area_2000_km2 = pa_area_all_2000_km2,
    pa_area_2020_km2 = pa_area_all_2020_km2,
    pa_area10km_2000_km2 = pa_area10km_all_2000_km2,
    pa_area10km_2020_km2 = pa_area10km_all_2020_km2,
    pa_area_2000_km2_pct = pa_area_all_2000_km2_pct,
    pa_area_2020_km2_pct = pa_area_all_2020_km2_pct,
    pa_area10km_2000_km2_pct = pa_area10km_all_2000_km2_pct,
    pa_area10km_2020_km2_pct = pa_area10km_all_2020_km2_pct,
    pop2000_in_pa_worldpop = pop2000_in_pa_all_WP,
    pop2020_in_pa_worldpop = pop2020_in_pa_all_WP,
    pop2000_in_pa10_worldpop = pop2000_in_pa10_all_WP,
    pop2020_in_pa10_worldpop = pop2020_in_pa10_all_WP,
    pop2000_in_pa_worldpop_pct = pop2000_in_pa_all_WP_pct,
    pop2020_in_pa_worldpop_pct = pop2020_in_pa_all_WP_pct,
    pop2000_in_pa10_worldpop_pct = pop2000_in_pa10_all_WP_pct,
    pop2020_in_pa10_worldpop_pct = pop2020_in_pa10_all_WP_pct
  )

pa_pop_ghsl <- pa_pop |>
  transmute(
    ISO3, country,
    country_area_km2,
    pop2000_total = pop2000_total_GHSL,
    pop2020_total = pop2020_total_GHSL,
    pa_area_2000_km2 = pa_area_all_2000_km2,
    pa_area_2020_km2 = pa_area_all_2020_km2,
    pa_area10km_2000_km2 = pa_area10km_all_2000_km2,
    pa_area10km_2020_km2 = pa_area10km_all_2020_km2,
    pa_area_2000_km2_pct = pa_area_all_2000_km2_pct,
    pa_area_2020_km2_pct = pa_area_all_2020_km2_pct,
    pa_area10km_2000_km2_pct = pa_area10km_all_2000_km2_pct,
    pa_area10km_2020_km2_pct = pa_area10km_all_2020_km2_pct,
    pop2000_in_pa_ghsl = pop2000_in_pa_all_GHSL,
    pop2020_in_pa_ghsl = pop2020_in_pa_all_GHSL,
    pop2000_in_pa10_ghsl = pop2000_in_pa10_all_GHSL,
    pop2020_in_pa10_ghsl = pop2020_in_pa10_all_GHSL,
    pop2000_in_pa_ghsl_pct = pop2000_in_pa_all_GHSL_pct,
    pop2020_in_pa_ghsl_pct = pop2020_in_pa_all_GHSL_pct,
    pop2000_in_pa10_ghsl_pct = pop2000_in_pa10_all_GHSL_pct,
    pop2020_in_pa10_ghsl_pct = pop2020_in_pa10_all_GHSL_pct
  )

pa_pop_combined <- pa_pop |>
  transmute(
    ISO3, country,
    country_area_km2,
    pa_area_2000_km2 = pa_area_all_2000_km2,
    pa_area_2020_km2 = pa_area_all_2020_km2,
    pa_area10km_2000_km2 = pa_area10km_all_2000_km2,
    pa_area10km_2020_km2 = pa_area10km_all_2020_km2,
    pa_area_2000_km2_pct = pa_area_all_2000_km2_pct,
    pa_area_2020_km2_pct = pa_area_all_2020_km2_pct,
    pa_area10km_2000_km2_pct = pa_area10km_all_2000_km2_pct,
    pa_area10km_2020_km2_pct = pa_area10km_all_2020_km2_pct,
    pop2000_total_worldpop = pop2000_total_WP,
    pop2020_total_worldpop = pop2020_total_WP,
    pop2000_total_ghsl = pop2000_total_GHSL,
    pop2020_total_ghsl = pop2020_total_GHSL,
    pop2000_in_pa_worldpop = pop2000_in_pa_all_WP,
    pop2020_in_pa_worldpop = pop2020_in_pa_all_WP,
    pop2000_in_pa_ghsl = pop2000_in_pa_all_GHSL,
    pop2020_in_pa_ghsl = pop2020_in_pa_all_GHSL,
    pop2000_in_pa10_worldpop = pop2000_in_pa10_all_WP,
    pop2020_in_pa10_worldpop = pop2020_in_pa10_all_WP,
    pop2000_in_pa10_ghsl = pop2000_in_pa10_all_GHSL,
    pop2020_in_pa10_ghsl = pop2020_in_pa10_all_GHSL,
    pop2000_in_pa_worldpop_pct = pop2000_in_pa_all_WP_pct,
    pop2020_in_pa_worldpop_pct = pop2020_in_pa_all_WP_pct,
    pop2000_in_pa_ghsl_pct = pop2000_in_pa_all_GHSL_pct,
    pop2020_in_pa_ghsl_pct = pop2020_in_pa_all_GHSL_pct,
    pop2000_in_pa10_worldpop_pct = pop2000_in_pa10_all_WP_pct,
    pop2020_in_pa10_worldpop_pct = pop2020_in_pa10_all_WP_pct,
    pop2000_in_pa10_ghsl_pct = pop2000_in_pa10_all_GHSL_pct,
    pop2020_in_pa10_ghsl_pct = pop2020_in_pa10_all_GHSL_pct
  )

# Totals rows (all PAs, unchanged dependencies) ---------------------------

compute_totals <- function(data, label) {
  data |>
    summarise(
      country = label,
      country_area_km2 = sum(country_area_km2, na.rm = TRUE),
      pa_area_2000_km2 = sum(pa_area_2000_km2, na.rm = TRUE),
      pa_area_2020_km2 = sum(pa_area_2020_km2, na.rm = TRUE),
      pa_area10km_2000_km2 = sum(pa_area10km_2000_km2, na.rm = TRUE),
      pa_area10km_2020_km2 = sum(pa_area10km_2020_km2, na.rm = TRUE),
      pop2000_total_worldpop = sum(pop2000_total_worldpop, na.rm = TRUE),
      pop2020_total_worldpop = sum(pop2020_total_worldpop, na.rm = TRUE),
      pop2000_total_ghsl = sum(pop2000_total_ghsl, na.rm = TRUE),
      pop2020_total_ghsl = sum(pop2020_total_ghsl, na.rm = TRUE),
      pop2000_in_pa_worldpop = sum(pop2000_in_pa_worldpop, na.rm = TRUE),
      pop2020_in_pa_worldpop = sum(pop2020_in_pa_worldpop, na.rm = TRUE),
      pop2000_in_pa_ghsl = sum(pop2000_in_pa_ghsl, na.rm = TRUE),
      pop2020_in_pa_ghsl = sum(pop2020_in_pa_ghsl, na.rm = TRUE),
      pop2000_in_pa10_worldpop = sum(pop2000_in_pa10_worldpop, na.rm = TRUE),
      pop2020_in_pa10_worldpop = sum(pop2020_in_pa10_worldpop, na.rm = TRUE),
      pop2000_in_pa10_ghsl = sum(pop2000_in_pa10_ghsl, na.rm = TRUE),
      pop2020_in_pa10_ghsl = sum(pop2020_in_pa10_ghsl, na.rm = TRUE)
    ) |>
    mutate(
      pa_area_2000_km2_pct = pa_area_2000_km2 / country_area_km2 * 100,
      pa_area_2020_km2_pct = pa_area_2020_km2 / country_area_km2 * 100,
      pa_area10km_2000_km2_pct = pa_area10km_2000_km2 / country_area_km2 * 100,
      pa_area10km_2020_km2_pct = pa_area10km_2020_km2 / country_area_km2 * 100,
      pop2000_in_pa_worldpop_pct = pop2000_in_pa_worldpop / pop2000_total_worldpop * 100,
      pop2020_in_pa_worldpop_pct = pop2020_in_pa_worldpop / pop2020_total_worldpop * 100,
      pop2000_in_pa_ghsl_pct = pop2000_in_pa_ghsl / pop2000_total_ghsl * 100,
      pop2020_in_pa_ghsl_pct = pop2020_in_pa_ghsl / pop2020_total_ghsl * 100,
      pop2000_in_pa10_worldpop_pct = pop2000_in_pa10_worldpop / pop2000_total_worldpop * 100,
      pop2020_in_pa10_worldpop_pct = pop2020_in_pa10_worldpop / pop2020_total_worldpop * 100,
      pop2000_in_pa10_ghsl_pct = pop2000_in_pa10_ghsl / pop2000_total_ghsl * 100,
      pop2020_in_pa10_ghsl_pct = pop2020_in_pa10_ghsl / pop2020_total_ghsl * 100
    )
}

pa_pop_total <- compute_totals(pa_pop_combined, "Total")
pa_pop_total_no_india <- pa_pop_combined |>
  filter(ISO3 != "IND") |>
  compute_totals("Total without India")

# Totals by type for new Table S3 and Figure 5 ----------------------------

compute_totals_type <- function(data, label, pa_type = c("strict", "non", "all")) {
  pa_type <- match.arg(pa_type)
  
  data |>
    summarise(
      country = label,
      pa_type = pa_type,
      country_area_km2 = sum(country_area_km2, na.rm = TRUE),
      pop2000_total_GHSL = sum(pop2000_total_GHSL, na.rm = TRUE),
      pop2020_total_GHSL = sum(pop2020_total_GHSL, na.rm = TRUE),
      pa_area_2000_km2 = sum(.data[[paste0("pa_area_", pa_type, "_2000_km2")]], na.rm = TRUE),
      pa_area_2020_km2 = sum(.data[[paste0("pa_area_", pa_type, "_2020_km2")]], na.rm = TRUE),
      pop2000_in_pa10_GHSL = sum(.data[[paste0("pop2000_in_pa10_", pa_type, "_GHSL")]], na.rm = TRUE),
      pop2020_in_pa10_GHSL = sum(.data[[paste0("pop2020_in_pa10_", pa_type, "_GHSL")]], na.rm = TRUE)
    ) |>
    mutate(
      pa_area_2000_km2_pct = pa_area_2000_km2 / country_area_km2 * 100,
      pa_area_2020_km2_pct = pa_area_2020_km2 / country_area_km2 * 100,
      pop2000_in_pa10_GHSL_pct = pop2000_in_pa10_GHSL / pop2000_total_GHSL * 100,
      pop2020_in_pa10_GHSL_pct = pop2020_in_pa10_GHSL / pop2020_total_GHSL * 100
    )
}

totals_type_total <- bind_rows(
  compute_totals_type(pa_pop, "Total", "strict"),
  compute_totals_type(pa_pop, "Total", "non"),
  compute_totals_type(pa_pop, "Total", "all")
)

# Save intermediary result for manuscript citation ------------------------

save(pa_pop_worldpop, pa_pop_ghsl, pa_pop_total, pa_pop_total_no_india,
     file = "results/pa_pop_total.rds")

# Totals by type for Table S1, S3, Figure 5 -------------------------------

totals_type_total <- bind_rows(
  compute_totals_type(pa_pop, "Total", "strict"),
  compute_totals_type(pa_pop, "Total", "non"),
  compute_totals_type(pa_pop, "Total", "all")
)

totals_type_total_no_india <- bind_rows(
  compute_totals_type(filter(pa_pop, ISO3 != "IND"), "Total without India", "strict"),
  compute_totals_type(filter(pa_pop, ISO3 != "IND"), "Total without India", "non"),
  compute_totals_type(filter(pa_pop, ISO3 != "IND"), "Total without India", "all")
)

# Produce Table S1 (updated) ----------------------------------------------

country_rows <- bind_rows(
  pa_pop |>
    transmute(
      country,
      pa_type = "strict",
      pa_area_2000_km2_pct = pa_area_strict_2000_km2_pct,
      pa_area_2020_km2_pct = pa_area_strict_2020_km2_pct,
      pop2000_in_pa10_GHSL_pct = pop2000_in_pa10_strict_GHSL_pct,
      pop2020_in_pa10_GHSL_pct = pop2020_in_pa10_strict_GHSL_pct
    ),
  pa_pop |>
    transmute(
      country,
      pa_type = "non",
      pa_area_2000_km2_pct = pa_area_non_2000_km2_pct,
      pa_area_2020_km2_pct = pa_area_non_2020_km2_pct,
      pop2000_in_pa10_GHSL_pct = pop2000_in_pa10_non_GHSL_pct,
      pop2020_in_pa10_GHSL_pct = pop2020_in_pa10_non_GHSL_pct
    ),
  pa_pop |>
    transmute(
      country,
      pa_type = "all",
      pa_area_2000_km2_pct = pa_area_all_2000_km2_pct,
      pa_area_2020_km2_pct = pa_area_all_2020_km2_pct,
      pop2000_in_pa10_GHSL_pct = pop2000_in_pa10_all_GHSL_pct,
      pop2020_in_pa10_GHSL_pct = pop2020_in_pa10_all_GHSL_pct
    )
)

table_s1_df <- bind_rows(
  totals_type_total |>
    select(country, pa_type, pa_area_2000_km2_pct, pa_area_2020_km2_pct,
           pop2000_in_pa10_GHSL_pct, pop2020_in_pa10_GHSL_pct),
  totals_type_total_no_india |>
    select(country, pa_type, pa_area_2000_km2_pct, pa_area_2020_km2_pct,
           pop2000_in_pa10_GHSL_pct, pop2020_in_pa10_GHSL_pct),
  country_rows
) |>
  mutate(
    pa_type = factor(pa_type, levels = c("strict", "non", "all"),
                     labels = c("Strict", "Non strict", "All"))
  ) |>
  arrange(
    match(country, c("Total", "Total without India")),
    country,
    pa_type
  )

table_s1 <- table_s1_df |>
  gt(groupname_col = "pa_type") |>
  tab_header(
    title = "Table S1: PA coverage and population within 10km by PA type (2000 and 2020)",
    subtitle = "Strict and non strict 10km buffer populations may overlap and are not additive."
  ) |>
  fmt_number(columns = where(is.numeric), decimals = 1) |>
  cols_label(
    country = "Country",
    pa_area_2000_km2_pct = "2000",
    pa_area_2020_km2_pct = "2020",
    pop2000_in_pa10_GHSL_pct = "2000",
    pop2020_in_pa10_GHSL_pct = "2020"
  ) |>
  tab_spanner(
    label = "% land within PAs",
    columns = c(pa_area_2000_km2_pct, pa_area_2020_km2_pct)
  ) |>
  tab_spanner(
    label = "% population within 10km of PAs (GHSL)",
    columns = c(pop2000_in_pa10_GHSL_pct, pop2020_in_pa10_GHSL_pct)
  ) |>
  cols_align(align = "center", columns = everything()) |>
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(0),
    column_labels.padding = px(0)
  ) |>
  tab_style(
    style = cell_borders(sides = "right", weight = px(0.5)),
    locations = cells_body(columns = everything())
  ) |>
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = cells_body(columns = c(country))
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = country %in% c("Total", "Total without India"))
  )

gtsave(table_s1, "results/table_s1.html")
gtsave(table_s1, "results/table_s1.tex")
gtsave(table_s1, "results/table_s1.docx")
write_rds(table_s1, "results/table_s1.rds")

# Produce Table 1 (unchanged logic) --------------------------------------

compute_smd <- function(x1, x2) {
  mean_diff <- abs(mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE))
  pooled_sd <- sqrt((var(x1, na.rm = TRUE) + var(x2, na.rm = TRUE)) / 2)
  mean_diff / pooled_sd
}

smd_results <- tibble(
  Metric = c("Population within PAs", "Population within 10km of PAs"),
  `2000` = c(
    compute_smd(pa_pop_combined$pop2000_in_pa_worldpop, pa_pop_combined$pop2000_in_pa_ghsl),
    compute_smd(pa_pop_combined$pop2000_in_pa10_worldpop, pa_pop_combined$pop2000_in_pa10_ghsl)
  ),
  `2020` = c(
    compute_smd(pa_pop_combined$pop2020_in_pa_worldpop, pa_pop_combined$pop2020_in_pa_ghsl),
    compute_smd(pa_pop_combined$pop2020_in_pa10_worldpop, pa_pop_combined$pop2020_in_pa10_ghsl)
  )
)

table_1 <- smd_results |>
  gt() |>
  tab_header(
    title = "Table 1: Standard mean differences between GHSL and WorldPop estimates"
  ) |>
  fmt_number(columns = everything(), decimals = 2) |>
  cols_align(align = "center", columns = everything()) |>
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(2),
    column_labels.padding = px(2)
  ) |>
  cols_label(Metric = "") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = everything()),
      cells_body(rows = everything(), columns = Metric)
    )
  )

gtsave(table_1, "results/table_1.html")
gtsave(table_1, "results/table_1.tex")
gtsave(table_1, "results/table_1.docx")
write_rds(table_1, "results/table_1.rds")

# Produce Table S2 (unchanged logic, adapted only for WP and GHSL suffixes) -

pa_pop_diff <- pa_pop_combined |>
  select(
    ISO3, country,
    pop2000_in_pa_worldpop_pct, pop2020_in_pa_worldpop_pct,
    pop2000_in_pa10_worldpop_pct, pop2020_in_pa10_worldpop_pct,
    pop2000_in_pa_ghsl_pct, pop2020_in_pa_ghsl_pct,
    pop2000_in_pa10_ghsl_pct, pop2020_in_pa10_ghsl_pct
  )

# Rebuild with consistent long format from pa_pop (pct columns exist there)
pa_pop_diff <- pa_pop |>
  select(
    ISO3, country,
    pop2000_in_pa_all_WP_pct, pop2020_in_pa_all_WP_pct,
    pop2000_in_pa10_all_WP_pct, pop2020_in_pa10_all_WP_pct,
    pop2000_in_pa_all_GHSL_pct, pop2020_in_pa_all_GHSL_pct,
    pop2000_in_pa10_all_GHSL_pct, pop2020_in_pa10_all_GHSL_pct
  ) |>
  pivot_longer(
    cols = -c(ISO3, country),
    names_to = c("year", "metric", "source"),
    names_pattern = "pop(\\d{4})_in_(pa10|pa)_all_(WP|GHSL)_pct"
  ) |>
  mutate(year = as.integer(year)) |>
  pivot_wider(names_from = source, values_from = value) |>
  rename(worldpop = WP, ghsl = GHSL) |>
  mutate(
    abs_diff = abs(worldpop - ghsl),
    diff_perc = abs_diff / ghsl
  )

pa_area_diff <- pa_pop |>
  select(ISO3, pa_area_all_2000_km2_pct, pa_area_all_2020_km2_pct) |>
  pivot_longer(
    cols = -ISO3,
    names_to = "year",
    names_pattern = "pa_area_all_(\\d{4})_km2_pct",
    values_to = "pa_area_pct"
  ) |>
  mutate(year = as.numeric(year))

pa_pop_diff <- pa_pop_diff |>
  left_join(pa_area_diff, by = c("ISO3", "year"))

major_discrepancies <- pa_pop_diff |>
  filter(abs_diff > 5 & diff_perc > 0.1) |>
  arrange(desc(abs_diff))

table_s2 <- major_discrepancies |>
  mutate(
    worldpop = worldpop / 100,
    ghsl = ghsl / 100,
    metric = case_when(
      metric == "pa" ~ "Within PAs",
      metric == "pa10" ~ "Within 10km of PAs"
    )
  ) |>
  select(country, year, metric, worldpop, ghsl, abs_diff, diff_perc) |>
  gt() |>
  tab_header(
    title = md("Table S2: Largest differences between GHSL and WorldPop estimates"),
    subtitle = md("*Absolute difference of more than 5 percentage points and relative difference of more than 10%*")
  ) |>
  cols_label(
    country = "Country",
    year = "Year",
    metric = "Area",
    worldpop = md("**WorldPop**<br>**Estimate**"),
    ghsl = md("**GHSL**<br>**Estimate**"),
    abs_diff = md("**Absolute**<br>**Difference**"),
    diff_perc = md("**Relative**<br>**Difference**")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) |>
  fmt_percent(
    columns = c(worldpop, ghsl, diff_perc),
    decimals = 1
  ) |>
  fmt_number(
    columns = abs_diff,
    decimals = 1,
    suffixing = FALSE
  ) |>
  text_transform(
    locations = cells_body(columns = abs_diff),
    fn = function(x) paste0(x, " pp")
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(4),
    column_labels.padding = px(6)
  )

gtsave(table_s2, "results/table_s2.html")
gtsave(table_s2, "results/table_s2.tex")
gtsave(table_s2, "results/table_s2.docx")
write_rds(table_s2, "results/table_s2.rds")

# Produce Figure 1 (unchanged logic) -------------------------------------

figure_1 <- ggplot(pa_pop_combined, aes(x = reorder(country, pop2020_in_pa10_ghsl))) +
  geom_point(aes(y = pop2000_in_pa10_ghsl), color = "gray", size = 3) +
  geom_point(aes(y = pop2020_in_pa10_ghsl), color = "steelblue", size = 3) +
  geom_segment(
    aes(
      y = pop2000_in_pa10_ghsl,
      yend = pop2020_in_pa10_ghsl,
      xend = country,
      color = pop2020_in_pa10_ghsl > pop2000_in_pa10_ghsl
    ),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c("red", "steelblue"),
                     labels = c("Decrease", "Increase")) +
  coord_flip() +
  labs(
    title = "Figure 1: Change in % population near PAs (GHSL 2000 to 2020)",
    x = NULL,
    y = "Population within 10km of PAs (count)",
    color = "Trend"
  ) +
  theme_minimal()

ggsave("results/figure_1.png", plot = figure_1, width = 8, height = 9, units = "in", dpi = 300)

# Produce Figure 2 (updated, two panels strict and non strict) ------------

baseline_threshold_pct <- 1

fig2_df <- bind_rows(
  pa_pop |>
    transmute(
      ISO3, country,
      pa_type = "Strict",
      pop2020_total = pop2020_total_WP,
      var_land = (pa_area_strict_2020_km2_pct - pa_area_strict_2000_km2_pct) / pa_area_strict_2000_km2_pct,
      var_pop = (pop2020_in_pa10_strict_WP_pct - pop2000_in_pa10_strict_WP_pct) / pop2000_in_pa10_strict_WP_pct,
      baseline_land = pa_area_strict_2000_km2_pct
    ),
  pa_pop |>
    transmute(
      ISO3, country,
      pa_type = "Non strict",
      pop2020_total = pop2020_total_WP,
      var_land = (pa_area_non_2020_km2_pct - pa_area_non_2000_km2_pct) / pa_area_non_2000_km2_pct,
      var_pop = (pop2020_in_pa10_non_WP_pct - pop2000_in_pa10_non_WP_pct) / pop2000_in_pa10_non_WP_pct,
      baseline_land = pa_area_non_2000_km2_pct
    )
) |>
  filter(is.finite(var_land), is.finite(var_pop)) |>
  filter(baseline_land > baseline_threshold_pct)

lim_all <- range(c(fig2_df$var_land, fig2_df$var_pop), finite = TRUE)

legend_line <- data.frame(
  x = c(lim_all[1], lim_all[2]),
  y = c(lim_all[1], lim_all[2]),
  type = "Equal PA and population growth"
)

figure_2 <- ggplot(fig2_df, aes(x = var_land, y = var_pop, label = ISO3)) +
  geom_point(aes(size = pop2020_total / 1e6), color = "blue", alpha = 0.7) +
  geom_text_repel(size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  geom_line(data = legend_line, aes(x = x, y = y, linetype = type),
            color = "green", inherit.aes = FALSE) +
  scale_linetype_manual(name = NULL, values = c("Equal PA and population growth" = "solid")) +
  scale_size_continuous(
    breaks = c(10, 100, 200),
    labels = c("10", "100", "200"),
    guide = guide_legend(title = "Total population in 2020 (millions)", title.position = "top")
  ) +
  scale_x_continuous(limits = lim_all) +
  scale_y_continuous(limits = lim_all) +
  facet_wrap(~pa_type, ncol = 1) +
  labs(
    title = "Figure 2: Growth in PA land share vs growth in population within 10km (WorldPop 2000 to 2020)",
    x = "Growth in land share covered by PAs",
    y = "Growth in population share within 10km of PAs",
    subtitle = paste0("Baseline filter: land share for the relevant type above ", baseline_threshold_pct, "% in 2000")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

save_plot("results/figure_2.png", plot = figure_2, base_width = 8, base_height = 9, unit = "in", dpi = 300)

# Produce Figure 3 (updated, two panels strict and non strict) ------------

fig3_df <- bind_rows(
  pa_pop |>
    transmute(
      ISO3, country,
      pa_type = "Strict",
      x = pa_area_strict_2020_km2_pct,
      y = pop2020_in_pa10_strict_WP_pct,
      pop2020_total = pop2020_total_WP
    ),
  pa_pop |>
    transmute(
      ISO3, country,
      pa_type = "Non strict",
      x = pa_area_non_2020_km2_pct,
      y = pop2020_in_pa10_non_WP_pct,
      pop2020_total = pop2020_total_WP
    )
)

max_x <- max(fig3_df$x, na.rm = TRUE)
max_y <- max(fig3_df$y, na.rm = TRUE)

figure_3 <- ggplot(fig3_df, aes(x = x, y = y, size = pop2020_total / 1e6, label = ISO3)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_text_repel(size = 3) +
  facet_wrap(~pa_type, ncol = 1) +
  scale_size_continuous(
    breaks = c(10, 100, 200),
    labels = c("10", "100", "200"),
    guide = guide_legend(title = "Total population in 2020 (millions)", title.position = "top")
  ) +
  scale_x_continuous(limits = c(0, max_x)) +
  scale_y_continuous(limits = c(0, max_y)) +
  labs(
    title = "Figure 3: PA land share vs population within 10km in 2020 (WorldPop)",
    x = "% of country covered by PAs (type specific)",
    y = "% of population within 10km of PAs (type specific)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

save_plot("results/figure_3.png", plot = figure_3, base_width = 8, base_height = 9, unit = "in", dpi = 300)

# Produce Figure 4 (unchanged logic) -------------------------------------

figure_4 <- pa_pop_diff |>
  filter(!is.na(worldpop) & !is.na(ghsl)) |>
  ggplot(aes(y = pa_area_pct)) +
  geom_segment(aes(x = ghsl, xend = worldpop, yend = pa_area_pct),
               color = "blue", linewidth = 1) +
  geom_point(aes(x = ghsl, color = "GHSL"), size = 2) +
  geom_point(aes(x = worldpop, color = "WorldPop"), size = 2) +
  facet_grid(
    rows = vars(metric),
    cols = vars(year),
    labeller = labeller(metric = c("pa" = "Within PAs", "pa10" = "Within 10km of PAs"))
  ) +
  scale_color_manual(
    name = "Population source",
    values = c("GHSL" = "black", "WorldPop" = "blue")
  ) +
  labs(
    title = "Figure 4: Comparison of population estimates from GHSL and WorldPop",
    x = "Population estimate (% of national population)",
    y = "PA coverage (% of national territory)"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot("results/figure_4.png", plot = figure_4, base_width = 8, base_height = 9, unit = "in", dpi = 300)

# Produce Figure 5 (new, grouped bars, non additive) ----------------------

fig5_totals <- totals_type_total |>
  filter(country == "Total") |>
  transmute(
    pa_type,
    pct_2000 = pop2000_in_pa10_GHSL_pct,
    pct_2020 = pop2020_in_pa10_GHSL_pct
  ) |>
  mutate(
    pa_type = factor(pa_type, levels = c("strict", "non", "all"),
                     labels = c("Strict", "Non strict", "All"))
  )

fig5_long <- bind_rows(
  fig5_totals |>
    transmute(pa_type, year = "2000", pct = pct_2000),
  fig5_totals |>
    transmute(pa_type, year = "2020", pct = pct_2020)
) |>
  mutate(year = factor(year, levels = c("2000", "2020")))

figure_5 <- ggplot(fig5_long, aes(x = year, y = pct, fill = pa_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Figure 5: Population within 10km of PAs by PA type (GHSL, 2000 and 2020)",
    x = NULL,
    y = "% of population within 10km",
    fill = "PA type",
    caption = "Strict and non strict 10km buffers may overlap. Bars are not additive and should not be interpreted as components of the All bar."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.caption = element_text(hjust = 0, size = 8)
  )

save_plot("results/figure_5.png", plot = figure_5, base_width = 8, base_height = 4.5, unit = "in", dpi = 300)

# Produce Table S3 (new, totals by type) ----------------------------------

table_s3_df <- totals_type_total |>
  filter(country == "Total") |>
  transmute(
    pa_type = factor(pa_type, levels = c("strict", "non", "all"),
                     labels = c("Strict", "Non strict", "All")),
    land_2000 = pa_area_2000_km2_pct,
    land_2020 = pa_area_2020_km2_pct,
    land_change_pp = land_2020 - land_2000,
    pop10_2000 = pop2000_in_pa10_GHSL_pct,
    pop10_2020 = pop2020_in_pa10_GHSL_pct,
    pop10_change_pp = pop10_2020 - pop10_2000
  )

table_s3 <- table_s3_df |>
  gt() |>
  tab_header(
    title = "Table S3: Aggregate totals by PA type (2000 and 2020)",
    subtitle = "Land protected (%) and population within 10km (%) using GHSL, with changes in percentage points"
  ) |>
  fmt_number(columns = where(is.numeric), decimals = 1) |>
  cols_label(
    pa_type = "PA type",
    land_2000 = "2000", land_2020 = "2020", land_change_pp = "Change (pp)",
    pop10_2000 = "2000", pop10_2020 = "2020", pop10_change_pp = "Change (pp)"
  ) |>
  tab_spanner(label = "Land protected (%)", columns = c(land_2000, land_2020, land_change_pp)) |>
  tab_spanner(label = "Population within 10km (%)", columns = c(pop10_2000, pop10_2020, pop10_change_pp)) |>
  cols_align(align = "center", columns = everything()) |>
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(4),
    column_labels.padding = px(6)
  )

gtsave(table_s3, "results/table_s3.html")
gtsave(table_s3, "results/table_s3.tex")
gtsave(table_s3, "results/table_s3.docx")
write_rds(table_s3, "results/table_s3.rds")

# Figures from Naidoo 2019 ------------------------------------------------

naidoo_tb <- read_csv(naidoo_path,
                      col_types = cols(`Survey Years` = col_character())) |>
  mutate(perc_households = (Households_10km_PA / Households_n) * 100)

perc_hh_10km_pa <- naidoo_tb |>
  summarise(mean_perc_hh = mean(perc_households, na.rm = TRUE))