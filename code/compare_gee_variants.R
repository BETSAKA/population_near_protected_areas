suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

source("code/replicate_gee_mapme_compare.R")

ensure_unzip_dir <- function(zip_path, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  marker <- file.path(exdir, ".unzipped")
  if (!file.exists(marker)) {
    utils::unzip(zip_path, exdir = exdir)
    writeLines(as.character(Sys.time()), marker)
  }
  exdir
}

normalize_old_gee <- function(df, dataset_name, source_name, file_name) {
  iso_vec <- if ("shapeGroup" %in% names(df)) {
    as.character(df$shapeGroup)
  } else if ("iso3" %in% names(df)) {
    as.character(df$iso3)
  } else {
    rep(NA_character_, nrow(df))
  }

  adm_level_vec <- if ("adm_level" %in% names(df)) as.integer(df$adm_level) else rep(NA_integer_, nrow(df))
  adm_name_vec <- if ("adm_name" %in% names(df)) {
    as.character(df$adm_name)
  } else if ("adm1_name" %in% names(df)) {
    as.character(df$adm1_name)
  } else {
    rep(NA_character_, nrow(df))
  }

  pop_total_vec <- if ("pop2020_total" %in% names(df)) as.numeric(df$pop2020_total) else rep(NA_real_, nrow(df))
  pop_inside_vec <- if ("pop2020_in_pa" %in% names(df)) as.numeric(df$pop2020_in_pa) else rep(NA_real_, nrow(df))
  pop_near_vec <- if ("pop2020_in_pa10" %in% names(df)) as.numeric(df$pop2020_in_pa10) else rep(NA_real_, nrow(df))
  area_inside_vec <- if ("pa_area_2020_km2" %in% names(df)) as.numeric(df$pa_area_2020_km2) else rep(NA_real_, nrow(df))
  area_near_vec <- if ("pa_area10km_2020_km2" %in% names(df)) as.numeric(df$pa_area10km_2020_km2) else rep(NA_real_, nrow(df))
  pa_count_vec <- if ("pa_count_2020" %in% names(df)) as.numeric(df$pa_count_2020) else rep(NA_real_, nrow(df))

  df |>
    transmute(
      dataset = dataset_name,
      source = source_name,
      iso3 = iso_vec,
      adm_level = adm_level_vec,
      adm_name = adm_name_vec,
      adm_id = paste0(adm_level_vec, "::", adm_name_vec),
      scenario = "All_2020",
      pop_total = pop_total_vec,
      pop_inside = pop_inside_vec,
      pop_near = pop_near_vec,
      area_inside = area_inside_vec,
      area_near = area_near_vec,
      pa_count = pa_count_vec,
      file = file_name
    )
}

normalize_absolute_all2020 <- function(df, dataset_name, file_name) {
  df |>
    filter(scenario %in% c("All_2020", "All_2020_Fixed", "All_2020_Reconstructed")) |>
    transmute(
      dataset = dataset_name,
      source = as.character(source),
      iso3 = as.character(iso3),
      adm_level = as.integer(adm_level),
      adm_name = as.character(adm_name),
      adm_id = as.character(adm_id),
      scenario = as.character(scenario),
      pop_total = as.numeric(pop_total),
      pop_inside = as.numeric(pop_strict + pop_nonstrict + pop_unknowncat),
      pop_near = as.numeric(pop_strict + pop_nonstrict + pop_unknowncat + pop_strict10 + pop_nonstrict10 + pop_unknowncat10),
      area_inside = as.numeric(area_strict + area_nonstrict + area_unknowncat),
      area_near = as.numeric(area_strict + area_nonstrict + area_unknowncat + area_strict10 + area_nonstrict10 + area_unknowncat10),
      pa_count = as.numeric(count_strict + count_nonstrict + count_unknowncat),
      file = file_name
    )
}

normalize_light_all2020 <- function(df, dataset_name, file_name) {
  df |>
    transmute(
      dataset = dataset_name,
      source = as.character(source),
      iso3 = as.character(iso3),
      adm_level = as.integer(adm_level),
      adm_name = as.character(adm_name),
      adm_id = as.character(adm_id),
      scenario = as.character(scenario),
      pop_total = as.numeric(pop_total),
      pop_inside = as.numeric(pop_strict + pop_nonstrict + pop_unknowncat),
      pop_near = as.numeric(pop_strict + pop_nonstrict + pop_unknowncat + pop_strict10 + pop_nonstrict10 + pop_unknowncat10),
      area_inside = as.numeric(area_strict + area_nonstrict + area_unknowncat),
      area_near = as.numeric(area_strict + area_nonstrict + area_unknowncat + area_strict10 + area_nonstrict10 + area_unknowncat10),
      pa_count = NA_real_,
      file = file_name
    )
}

load_original_gee_ghsl <- function(dir_path) {
  files <- Sys.glob(file.path(dir_path, "pop_pa_*.csv"))
  bind_rows(lapply(files, function(path) {
    normalize_old_gee(read_csv(path, show_col_types = FALSE), "orig_gee_ghsl", "GHSL", basename(path))
  }))
}

load_original_gee_wp <- function(dir_path) {
  files <- Sys.glob(file.path(dir_path, "*.csv"))
  bind_rows(lapply(files, function(path) {
    df <- read_csv(path, show_col_types = FALSE)
    if (!all(c("pop2020_total", "pa_count_2020", "shapeGroup") %in% names(df))) {
      return(NULL)
    }
    normalize_old_gee(df, "orig_gee_wp", "WP", basename(path))
  }))
}

load_heavy_fix_gee <- function(zip_path, exdir) {
  ensure_unzip_dir(zip_path, exdir)
  files <- Sys.glob(file.path(exdir, "PA_Pop_Final_Absolute_All2020_Fix", "*.csv"))
  bind_rows(lapply(files, function(path) {
    normalize_absolute_all2020(read_csv(path, show_col_types = FALSE), "heavy_fix_gee", basename(path))
  }))
}

load_light_fix_gee <- function(zip_path, exdir) {
  ensure_unzip_dir(zip_path, exdir)
  files <- Sys.glob(file.path(exdir, "PA_Pop_Final_All2020_Only", "*.csv"))
  bind_rows(lapply(files, function(path) {
    normalize_light_all2020(read_csv(path, show_col_types = FALSE), "light_fix_gee", basename(path))
  }))
}

load_r_current_original <- function(dir_path) {
  files <- Sys.glob(file.path(dir_path, "PA_Pop_*_*.csv"))
  bind_rows(lapply(files, function(path) {
    df <- read_csv(path, show_col_types = FALSE)
    reconstructed <- reconstruct_original_all2020(df)
    normalize_absolute_all2020(reconstructed, "r_current_original", basename(path))
  }))
}

load_r_current_fix <- function(dir_path) {
  files <- Sys.glob(file.path(dir_path, "PA_Pop_*_*.csv"))
  bind_rows(lapply(files, function(path) {
    normalize_absolute_all2020(read_csv(path, show_col_types = FALSE), "r_current_fix", basename(path))
  }))
}

dedupe_rows <- function(df) {
  df |>
    group_by(dataset, source, iso3, adm_level, adm_name, adm_id, scenario) |>
    slice(1) |>
    ungroup()
}

pairwise_summary <- function(long_df) {
  metrics <- c("pop_total", "pop_inside", "pop_near", "area_inside", "area_near", "pa_count")

  wide <- long_df |>
    pivot_wider(
      id_cols = c(iso3, source, adm_level, adm_name),
      names_from = dataset,
      values_from = all_of(metrics),
      names_sep = "__"
    )

  datasets <- sort(unique(long_df$dataset))
  pairs <- combn(datasets, 2, simplify = FALSE)

  bind_rows(lapply(pairs, function(pair) {
    left <- pair[[1]]
    right <- pair[[2]]

    bind_rows(lapply(metrics, function(metric) {
      left_col <- paste0(metric, "__", left)
      right_col <- paste0(metric, "__", right)
      if (!(left_col %in% names(wide) && right_col %in% names(wide))) {
        return(NULL)
      }
      comp <- wide |>
        filter(!is.na(.data[[left_col]]), !is.na(.data[[right_col]]))
      if (nrow(comp) == 0) {
        return(NULL)
      }
      tibble(
        dataset_left = left,
        dataset_right = right,
        metric = metric,
        n_overlap = nrow(comp),
        max_abs_diff = max(abs(comp[[left_col]] - comp[[right_col]]), na.rm = TRUE),
        mean_abs_diff = mean(abs(comp[[left_col]] - comp[[right_col]]), na.rm = TRUE)
      )
    }))
  }))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  heavy_zip <- if (length(args) >= 1) args[[1]] else "data/tests/PA_Pop_Final_Absolute_All2020_Fix-20260629T070509Z-3-001.zip"
  light_zip <- if (length(args) >= 2) args[[2]] else "data/tests/PA_Pop_Final_All2020_Only-20260629T070457Z-3-001.zip"
  output_dir <- if (length(args) >= 3) args[[3]] else "data/tests/gee_variant_comparison"

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  heavy_exdir <- file.path(output_dir, "heavy_unzip")
  light_exdir <- file.path(output_dir, "light_unzip")

  datasets <- bind_rows(
    load_original_gee_ghsl("data/Output_GEE_GHSL"),
    load_original_gee_wp("data/Output_GEE_Worldpop"),
    load_heavy_fix_gee(heavy_zip, heavy_exdir),
    load_light_fix_gee(light_zip, light_exdir),
    load_r_current_original("data/Output_R_mapme_reviewed_original"),
    load_r_current_fix("data/Output_R_mapme_all2020_fix")
  ) |>
    dedupe_rows()

  overlap_keys <- datasets |>
    count(iso3, source, adm_level, adm_name, name = "n_datasets") |>
    filter(n_datasets >= 2)

  overlap_long <- datasets |>
    semi_join(overlap_keys, by = c("iso3", "source", "adm_level", "adm_name"))

  overlap_wide <- overlap_long |>
    select(dataset, source, iso3, adm_level, adm_name, adm_id, pop_total, pop_inside, pop_near, area_inside, area_near, pa_count) |>
    pivot_wider(
      id_cols = c(iso3, source, adm_level, adm_name),
      names_from = dataset,
      values_from = c(pop_total, pop_inside, pop_near, area_inside, area_near, pa_count),
      names_sep = "__"
    )

  summary_tbl <- pairwise_summary(overlap_long)

  write_csv(overlap_long, file.path(output_dir, "gee_variant_overlap_long.csv"), na = "")
  write_csv(overlap_wide, file.path(output_dir, "gee_variant_overlap_wide.csv"), na = "")
  write_csv(summary_tbl, file.path(output_dir, "gee_variant_pairwise_summary.csv"), na = "")

  cat("Overlap units:", nrow(overlap_keys), "\n")
  cat("Datasets present:\n")
  print(count(overlap_long, dataset, source, name = "n_rows") |> arrange(dataset, source))
  cat("\nPairwise summary:\n")
  print(summary_tbl |> arrange(metric, dataset_left, dataset_right))
}

main()