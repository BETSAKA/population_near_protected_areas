suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

old_output_dir <- file.path(
  project_dir,
  "data",
  "reviewed_PA_Pop_GHSL_Worldpop"
)
new_output_dir <- file.path(project_dir, "data", "reviewed_PA_Pop_new")
new_national_dir <- file.path(new_output_dir, "national_totals")

report_dir <- file.path(project_dir, "second_revision")
report_path <- file.path(
  report_dir,
  "first_vs_second_revision_mapping_and_comparison.md"
)
country_csv_path <- file.path(
  report_dir,
  "first_vs_second_revision_country_core_comparison.csv"
)
global_csv_path <- file.path(
  report_dir,
  "first_vs_second_revision_global_core_comparison.csv"
)
national_csv_path <- file.path(
  report_dir,
  "first_vs_second_revision_national_totals_comparison.csv"
)

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

fmt_num <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "NA",
    format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

fmt_pct <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "NA",
    paste0(format(round(x, digits), scientific = FALSE, trim = TRUE), "%")
  )
}

md_table <- function(data) {
  if (nrow(data) == 0) {
    return("_No rows._")
  }

  header <- paste0("| ", paste(names(data), collapse = " | "), " |")
  separator <- paste0(
    "| ",
    paste(rep("---", ncol(data)), collapse = " | "),
    " |"
  )
  rows <- apply(data, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })

  c(header, separator, rows)
}

read_country_outputs <- function(directory) {
  files <- list.files(
    directory,
    pattern = "^PA_Pop_.*_(GHSL|WP)\\.csv$",
    full.names = TRUE
  )

  tibble(path = files) |>
    mutate(
      file_name = basename(path),
      iso3 = str_match(file_name, "^PA_Pop_(.*)_(GHSL|WP)\\.csv$")[, 2],
      source = str_match(file_name, "^PA_Pop_(.*)_(GHSL|WP)\\.csv$")[, 3],
      data = map(path, ~ read_csv(.x, show_col_types = FALSE))
    )
}

aggregate_country_outputs <- function(file_tbl) {
  metric_cols <- c(
    "pop_total",
    "pop_strict",
    "pop_strict10",
    "pop_nonstrict",
    "pop_nonstrict10",
    "pop_unknowncat",
    "pop_unknowncat10",
    "area_strict",
    "area_strict10",
    "area_nonstrict",
    "area_nonstrict10",
    "area_unknowncat",
    "area_unknowncat10"
  )

  count_cols <- c("count_strict", "count_nonstrict", "count_unknowncat")

  file_tbl |>
    transmute(iso3, source, data) |>
    mutate(
      data = map(data, \(df) {
        df |>
          mutate(
            iso3 = as.character(iso3),
            source = as.character(source),
            scenario = as.character(scenario),
            pop_year = as.integer(pop_year)
          ) |>
          summarize(
            across(all_of(metric_cols), ~ sum(.x, na.rm = TRUE)),
            across(all_of(count_cols), ~ max(.x, na.rm = TRUE)),
            n_regions = n(),
            .by = c(iso3, source, scenario, pop_year)
          )
      })
    ) |>
    pull(data) |>
    list_rbind() |>
    arrange(source, iso3, pop_year, scenario)
}

derive_all2020_legacy <- function(country_outputs) {
  additive_cols <- c(
    "pop_strict",
    "pop_strict10",
    "pop_nonstrict",
    "pop_nonstrict10",
    "pop_unknowncat",
    "pop_unknowncat10",
    "area_strict",
    "area_strict10",
    "area_nonstrict",
    "area_nonstrict10",
    "area_unknowncat",
    "area_unknowncat10",
    "count_strict",
    "count_nonstrict",
    "count_unknowncat"
  )

  confirmed <- country_outputs |>
    filter(scenario == "Confirmed_2020") |>
    select(iso3, source, pop_total, n_regions, all_of(additive_cols))

  unknown <- country_outputs |>
    filter(scenario == "Unknown_Year") |>
    select(iso3, source, all_of(additive_cols)) |>
    rename_with(~ paste0(.x, "_unknown"), all_of(additive_cols))

  all2020 <- confirmed |>
    left_join(unknown, by = c("iso3", "source"))

  for (col in additive_cols) {
    unknown_col <- paste0(col, "_unknown")
    all2020[[col]] <- all2020[[col]] +
      dplyr::coalesce(all2020[[unknown_col]], 0)
  }

  all2020 |>
    mutate(
      pop_year = 2020L,
      scenario = "All_2020",
      legacy_construction = "Confirmed_2020 + Unknown_Year"
    ) |>
    select(
      iso3,
      source,
      scenario,
      pop_year,
      pop_total,
      all_of(additive_cols),
      n_regions,
      legacy_construction
    )
}

add_core_metrics <- function(df) {
  df |>
    mutate(
      pop_inside_all = pop_strict + pop_nonstrict + pop_unknowncat,
      pop_inside_or_10km_all = pop_strict +
        pop_nonstrict +
        pop_unknowncat +
        pop_strict10 +
        pop_nonstrict10 +
        pop_unknowncat10,
      area_inside_all = area_strict + area_nonstrict + area_unknowncat,
      area_inside_or_10km_all = area_strict +
        area_nonstrict +
        area_unknowncat +
        area_strict10 +
        area_nonstrict10 +
        area_unknowncat10
    )
}

read_old_national_totals <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(iso3 = as.character(iso3))
}

read_new_national_totals <- function(directory) {
  files <- list.files(
    directory,
    pattern = "^National_PA_Totals_Refactored_.*\\.csv$",
    full.names = TRUE
  )

  map_dfr(files, ~ read_csv(.x, show_col_types = FALSE)) |>
    mutate(iso3 = as.character(iso3))
}

compare_core_metrics <- function(old_df, new_df, keys) {
  metric_cols <- c(
    "pop_total",
    "pop_inside_all",
    "pop_inside_or_10km_all",
    "area_inside_all",
    "area_inside_or_10km_all",
    "count_strict",
    "count_nonstrict",
    "count_unknowncat"
  )

  joined <- old_df |>
    rename_with(~ paste0("first_", .x), all_of(metric_cols)) |>
    full_join(
      new_df |>
        rename_with(~ paste0("second_", .x), all_of(metric_cols)),
      by = keys
    )

  metrics_long <- map_dfr(metric_cols, \(metric) {
    first_col <- paste0("first_", metric)
    second_col <- paste0("second_", metric)

    joined |>
      transmute(
        across(all_of(keys)),
        metric = metric,
        first_revision = .data[[first_col]],
        second_revision = .data[[second_col]],
        absolute_change = second_revision - first_revision,
        pct_change = if_else(
          first_revision == 0,
          NA_real_,
          100 * absolute_change / first_revision
        )
      )
  })

  joined_core <- joined |>
    transmute(
      across(all_of(keys)),
      first_pop_total,
      second_pop_total,
      pop_total_abs_change = second_pop_total - first_pop_total,
      pop_total_pct_change = if_else(
        first_pop_total == 0,
        NA_real_,
        100 * pop_total_abs_change / first_pop_total
      ),
      first_pop_inside_all,
      second_pop_inside_all,
      pop_inside_all_abs_change = second_pop_inside_all - first_pop_inside_all,
      pop_inside_all_pct_change = if_else(
        first_pop_inside_all == 0,
        NA_real_,
        100 * pop_inside_all_abs_change / first_pop_inside_all
      ),
      first_pop_inside_or_10km_all,
      second_pop_inside_or_10km_all,
      pop_inside_or_10km_all_abs_change = second_pop_inside_or_10km_all -
        first_pop_inside_or_10km_all,
      pop_inside_or_10km_all_pct_change = if_else(
        first_pop_inside_or_10km_all == 0,
        NA_real_,
        100 * pop_inside_or_10km_all_abs_change / first_pop_inside_or_10km_all
      ),
      first_area_inside_all,
      second_area_inside_all,
      area_inside_all_abs_change = second_area_inside_all -
        first_area_inside_all,
      area_inside_all_pct_change = if_else(
        first_area_inside_all == 0,
        NA_real_,
        100 * area_inside_all_abs_change / first_area_inside_all
      ),
      first_area_inside_or_10km_all,
      second_area_inside_or_10km_all,
      area_inside_or_10km_all_abs_change = second_area_inside_or_10km_all -
        first_area_inside_or_10km_all,
      area_inside_or_10km_all_pct_change = if_else(
        first_area_inside_or_10km_all == 0,
        NA_real_,
        100 * area_inside_or_10km_all_abs_change / first_area_inside_or_10km_all
      ),
      first_count_strict,
      second_count_strict,
      first_count_nonstrict,
      second_count_nonstrict,
      first_count_unknowncat,
      second_count_unknowncat
    )

  list(core = joined_core, long = metrics_long)
}

old_country_files <- read_country_outputs(old_output_dir)
new_country_files <- read_country_outputs(new_output_dir)

old_country_agg <- aggregate_country_outputs(old_country_files)
new_country_agg <- aggregate_country_outputs(new_country_files)

old_country_augmented <- bind_rows(
  old_country_agg,
  derive_all2020_legacy(old_country_agg)
) |>
  add_core_metrics()

new_country_augmented <- new_country_agg |>
  add_core_metrics()

old_overlap <- old_country_augmented |>
  semi_join(
    new_country_augmented,
    by = c("iso3", "source", "scenario", "pop_year")
  )

new_overlap <- new_country_augmented |>
  semi_join(
    old_country_augmented,
    by = c("iso3", "source", "scenario", "pop_year")
  )

country_comparison <- compare_core_metrics(
  old_overlap,
  new_overlap,
  keys = c("iso3", "source", "scenario", "pop_year")
)

global_old <- old_overlap |>
  summarize(
    across(
      c(
        pop_total,
        pop_inside_all,
        pop_inside_or_10km_all,
        area_inside_all,
        area_inside_or_10km_all,
        count_strict,
        count_nonstrict,
        count_unknowncat
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c("source", "scenario", "pop_year")
  )

global_new <- new_overlap |>
  summarize(
    across(
      c(
        pop_total,
        pop_inside_all,
        pop_inside_or_10km_all,
        area_inside_all,
        area_inside_or_10km_all,
        count_strict,
        count_nonstrict,
        count_unknowncat
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    .by = c("source", "scenario", "pop_year")
  )

global_comparison <- compare_core_metrics(
  global_old,
  global_new,
  keys = c("source", "scenario", "pop_year")
)

old_national <- read_old_national_totals(
  file.path(old_output_dir, "National_PA_Totals_Refactored.csv")
)
new_national <- read_new_national_totals(new_national_dir)

common_national_cols <- intersect(names(old_national), names(new_national))
common_national_cols <- setdiff(common_national_cols, c("system:index", ".geo"))
overlap_iso3 <- intersect(old_national$iso3, new_national$iso3)

national_comparison <- old_national |>
  filter(iso3 %in% overlap_iso3) |>
  select(all_of(common_national_cols)) |>
  rename_with(~ paste0("first_", .x), -iso3) |>
  full_join(
    new_national |>
      filter(iso3 %in% overlap_iso3) |>
      select(all_of(common_national_cols)) |>
      rename_with(~ paste0("second_", .x), -iso3),
    by = "iso3"
  ) |>
  mutate(
    area_total_pa_abs_change = second_area_total_pa - first_area_total_pa,
    area_total_pa_pct_change = if_else(
      first_area_total_pa == 0,
      NA_real_,
      100 * area_total_pa_abs_change / first_area_total_pa
    ),
    nat_pop_gh_20_abs_change = second_nat_pop_gh_20 - first_nat_pop_gh_20,
    nat_pop_gh_20_pct_change = if_else(
      first_nat_pop_gh_20 == 0,
      NA_real_,
      100 * nat_pop_gh_20_abs_change / first_nat_pop_gh_20
    ),
    nat_pop_wp_20_abs_change = second_nat_pop_wp_20 - first_nat_pop_wp_20,
    nat_pop_wp_20_pct_change = if_else(
      first_nat_pop_wp_20 == 0,
      NA_real_,
      100 * nat_pop_wp_20_abs_change / first_nat_pop_wp_20
    ),
    count_total_abs_change = second_count_total - first_count_total
  ) |>
  arrange(iso3)

ensure_dir(report_dir)

write_csv(country_comparison$core, country_csv_path, na = "")
write_csv(global_comparison$core, global_csv_path, na = "")
write_csv(national_comparison, national_csv_path, na = "")

old_country_pairs <- old_country_files |>
  distinct(iso3, source)
new_country_pairs <- new_country_files |>
  distinct(iso3, source)
overlap_pairs <- inner_join(
  old_country_pairs,
  new_country_pairs,
  by = c("iso3", "source")
)

overlap_countries <- overlap_pairs |>
  distinct(iso3) |>
  arrange(iso3) |>
  pull(iso3)

missing_new_countries <- old_country_pairs |>
  distinct(iso3) |>
  anti_join(
    new_country_pairs |>
      distinct(iso3),
    by = "iso3"
  ) |>
  arrange(iso3) |>
  pull(iso3)

global_summary_table <- global_comparison$core |>
  transmute(
    source,
    scenario,
    first_pop_inside_or_10km_all = fmt_num(first_pop_inside_or_10km_all, 0),
    second_pop_inside_or_10km_all = fmt_num(second_pop_inside_or_10km_all, 0),
    pop_inside_or_10km_all_abs_change = fmt_num(
      pop_inside_or_10km_all_abs_change,
      0
    ),
    pop_inside_or_10km_all_pct_change = fmt_pct(
      pop_inside_or_10km_all_pct_change,
      2
    ),
    first_pop_inside_all = fmt_num(first_pop_inside_all, 0),
    second_pop_inside_all = fmt_num(second_pop_inside_all, 0),
    pop_inside_all_abs_change = fmt_num(pop_inside_all_abs_change, 0),
    pop_inside_all_pct_change = fmt_pct(pop_inside_all_pct_change, 2)
  ) |>
  arrange(
    source,
    factor(
      scenario,
      levels = c("Confirmed_2000", "Confirmed_2020", "Unknown_Year", "All_2020")
    )
  )

country_focus_table <- country_comparison$core |>
  transmute(
    iso3,
    source,
    scenario,
    first_pop_inside_or_10km_all = fmt_num(first_pop_inside_or_10km_all, 0),
    second_pop_inside_or_10km_all = fmt_num(second_pop_inside_or_10km_all, 0),
    pop_inside_or_10km_all_abs_change = fmt_num(
      pop_inside_or_10km_all_abs_change,
      0
    ),
    pop_inside_or_10km_all_pct_change = fmt_pct(
      pop_inside_or_10km_all_pct_change,
      2
    ),
    first_pop_inside_all = fmt_num(first_pop_inside_all, 0),
    second_pop_inside_all = fmt_num(second_pop_inside_all, 0),
    pop_inside_all_abs_change = fmt_num(pop_inside_all_abs_change, 0),
    pop_inside_all_pct_change = fmt_pct(pop_inside_all_pct_change, 2)
  ) |>
  arrange(
    source,
    factor(
      scenario,
      levels = c("Confirmed_2000", "Confirmed_2020", "Unknown_Year", "All_2020")
    ),
    iso3
  )

national_focus_table <- national_comparison |>
  transmute(
    iso3,
    first_area_total_pa = fmt_num(first_area_total_pa, 1),
    second_area_total_pa = fmt_num(second_area_total_pa, 1),
    area_total_pa_abs_change = fmt_num(area_total_pa_abs_change, 1),
    area_total_pa_pct_change = fmt_pct(area_total_pa_pct_change, 2),
    first_nat_pop_gh_20 = fmt_num(first_nat_pop_gh_20, 0),
    second_nat_pop_gh_20 = fmt_num(second_nat_pop_gh_20, 0),
    nat_pop_gh_20_abs_change = fmt_num(nat_pop_gh_20_abs_change, 0),
    nat_pop_gh_20_pct_change = fmt_pct(nat_pop_gh_20_pct_change, 2),
    first_nat_pop_wp_20 = fmt_num(first_nat_pop_wp_20, 0),
    second_nat_pop_wp_20 = fmt_num(second_nat_pop_wp_20, 0),
    nat_pop_wp_20_abs_change = fmt_num(nat_pop_wp_20_abs_change, 0),
    nat_pop_wp_20_pct_change = fmt_pct(nat_pop_wp_20_pct_change, 2),
    first_count_total,
    second_count_total,
    count_total_abs_change
  )

top_all2020_changes <- country_comparison$core |>
  filter(scenario == "All_2020") |>
  arrange(desc(abs(pop_inside_or_10km_all_abs_change))) |>
  transmute(
    iso3,
    source,
    first_pop_inside_or_10km_all = fmt_num(first_pop_inside_or_10km_all, 0),
    second_pop_inside_or_10km_all = fmt_num(second_pop_inside_or_10km_all, 0),
    pop_inside_or_10km_all_abs_change = fmt_num(
      pop_inside_or_10km_all_abs_change,
      0
    ),
    pop_inside_or_10km_all_pct_change = fmt_pct(
      pop_inside_or_10km_all_pct_change,
      2
    )
  )

mapping_table <- tribble(
  ~first_revision_output                                                                                                                        , ~second_revision_output , ~relationship ,
  "data/reviewed_PA_Pop_GHSL_Worldpop/PA_Pop_{ISO3}_{SOURCE}.csv"                                                                               ,
  "data/reviewed_PA_Pop_new/PA_Pop_{ISO3}_{SOURCE}.csv"                                                                                         ,
  "Same ADM-level artifact and same core columns; second revision adds explicit All_2020 rows."                                                 ,
  "Legacy 2020 all-PAs used in reviewed_produce_table_and_figures.R as Confirmed_2020 + Unknown_Year"                                           ,
  "scenario == All_2020 in data/reviewed_PA_Pop_new/PA_Pop_{ISO3}_{SOURCE}.csv"                                                                 ,
  "New direct replacement for the old derived all-2020 quantity; avoids double counting from separate hierarchy resets."                        ,
  "data/reviewed_PA_Pop_GHSL_Worldpop/National_PA_Totals_Refactored.csv"                                                                        ,
  "data/reviewed_PA_Pop_new/national_totals/National_PA_Totals_Refactored_{ISO3}.csv"                                                           ,
  "Old file is one combined table; new outputs are one file per country and add *_confirmed2020 fields."                                        ,
  "nat_pop_gh_20 and nat_pop_wp_20 in National_PA_Totals_Refactored.csv"                                                                        ,
  "nat_pop_gh_20 and nat_pop_wp_20 in National_PA_Totals_Refactored_{ISO3}.csv"                                                                 ,
  "Same field names, but in the new script they are aligned to the explicit All_2020 scenario and no longer mixed with unrestricted PA counts."
) |>
  mutate(across(everything(), as.character))

key_computations <- c(
  "1. Filter the WDPA to designated, established, or inscribed terrestrial PAs, excluding UNESCO-MAB Biosphere Reserves and marine-only sites.",
  "2. Split PAs into strict, non-strict, and unknown IUCN categories, then impose the exclusive hierarchy strict > non-strict > unknown.",
  "3. Build scenario-specific PA masks for Confirmed_2000, Confirmed_2020, Unknown_Year, and in the second revision All_2020.",
  "4. Build 10 km buffer rings that exclude pixels already claimed by PA cores or higher-priority buffers, so each pixel is counted once per scenario.",
  "5. Intersect those masks with GHSL and WorldPop population rasters and sum population and land area by ADM unit, then aggregate to country totals.",
  "6. Derive manuscript indicators such as population inside PAs, population inside or within 10 km, PA area, and category-specific decompositions from the country aggregates."
)

report_lines <- c(
  "# First vs Second Revision Output Mapping and Comparison",
  "",
  paste0("Generated on: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  "",
  "## Scope",
  "",
  paste0(
    "This report compares the first-revision GEE outputs in `data/reviewed_PA_Pop_GHSL_Worldpop` ",
    "with the second-revision R outputs in `data/reviewed_PA_Pop_new`."
  ),
  "",
  paste0("Old ADM output files found: ", nrow(old_country_pairs), "."),
  paste0("New ADM output files found: ", nrow(new_country_pairs), "."),
  paste0(
    "Countries currently comparable: ",
    length(overlap_countries),
    " (",
    paste(overlap_countries, collapse = ", "),
    ")."
  ),
  paste0(
    "Countries present in the first revision but not yet in the current second-revision folder: ",
    length(missing_new_countries),
    "."
  ),
  "",
  "Because `data/reviewed_PA_Pop_new` currently contains only a subset of countries, all country and global comparisons below are restricted to the overlap available at run time. Re-running this script after more second-revision outputs are produced will expand the comparison automatically.",
  "",
  "## Key Computations Needed for the Analysis",
  "",
  key_computations,
  "",
  "## Output Mapping",
  "",
  md_table(mapping_table),
  "",
  "## Comparison Method",
  "",
  "1. ADM-level CSVs are aggregated to country x source x scenario by summing population and area fields and taking the scenario-level PA counts once per country.",
  "2. The first revision has no explicit `All_2020` rows, so its all-2020 quantity is reconstructed exactly as the first-revision R analysis did: `Confirmed_2020 + Unknown_Year`.",
  "3. The second revision is compared directly on its explicit `All_2020` rows, as well as on the three shared scenarios (`Confirmed_2000`, `Confirmed_2020`, `Unknown_Year`).",
  "4. Global comparisons are sums across the overlapping countries only.",
  "",
  "## Global Comparison",
  "",
  md_table(global_summary_table),
  "",
  "## Country-by-Country Core Comparison",
  "",
  md_table(country_focus_table),
  "",
  "## National Totals Comparison",
  "",
  md_table(national_focus_table),
  "",
  "## Largest All_2020 Country-Level Changes",
  "",
  md_table(top_all2020_changes),
  "",
  "## Files Written by This Script",
  "",
  paste0("- `", report_path, "`"),
  paste0("- `", country_csv_path, "`"),
  paste0("- `", global_csv_path, "`"),
  paste0("- `", national_csv_path, "`")
)

writeLines(report_lines, report_path)

list(
  report_path = report_path,
  country_csv_path = country_csv_path,
  global_csv_path = global_csv_path,
  national_csv_path = national_csv_path,
  overlap_countries = overlap_countries,
  missing_new_countries = missing_new_countries
)
