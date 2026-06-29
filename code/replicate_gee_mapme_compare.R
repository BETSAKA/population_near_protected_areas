suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})

reconstruct_original_all2020 <- function(df) {
  confirmed <- df |>
    filter(scenario == "Confirmed_2020") |>
    select(-scenario, -pop_year)

  unknown <- df |>
    filter(scenario == "Unknown_Year") |>
    select(-scenario, -pop_year) |>
    rename_with(~ paste0(.x, "_unk"), c(starts_with("count_"), starts_with("pop_"), starts_with("area_")))

  confirmed |>
    left_join(unknown, by = c("iso3", "adm_level", "adm_name", "adm_id", "source")) |>
    mutate(
      count_strict = count_strict + coalesce(count_strict_unk, 0),
      count_nonstrict = count_nonstrict + coalesce(count_nonstrict_unk, 0),
      count_unknowncat = count_unknowncat + coalesce(count_unknowncat_unk, 0),
      pop_total = pop_total,
      pop_strict = pop_strict + coalesce(pop_strict_unk, 0),
      pop_strict10 = pop_strict10 + coalesce(pop_strict10_unk, 0),
      pop_nonstrict = pop_nonstrict + coalesce(pop_nonstrict_unk, 0),
      pop_nonstrict10 = pop_nonstrict10 + coalesce(pop_nonstrict10_unk, 0),
      pop_unknowncat = pop_unknowncat + coalesce(pop_unknowncat_unk, 0),
      pop_unknowncat10 = pop_unknowncat10 + coalesce(pop_unknowncat10_unk, 0),
      area_strict = area_strict + coalesce(area_strict_unk, 0),
      area_strict10 = area_strict10 + coalesce(area_strict10_unk, 0),
      area_nonstrict = area_nonstrict + coalesce(area_nonstrict_unk, 0),
      area_nonstrict10 = area_nonstrict10 + coalesce(area_nonstrict10_unk, 0),
      area_unknowncat = area_unknowncat + coalesce(area_unknowncat_unk, 0),
      area_unknowncat10 = area_unknowncat10 + coalesce(area_unknowncat10_unk, 0),
      scenario = "All_2020_Reconstructed",
      pop_year = 2020
    ) |>
    select(any_of(names(df)))
}

compare_original_vs_fix <- function(original_dir, fix_dir, output_csv) {
  original_files <- Sys.glob(file.path(original_dir, "PA_Pop_*_*.csv"))

  rows <- purrr::map_dfr(original_files, function(path) {
    file_name <- basename(path)
    fix_path <- file.path(fix_dir, file_name)

    if (!file.exists(fix_path)) {
      return(NULL)
    }

    original <- readr::read_csv(path, show_col_types = FALSE)
    reconstructed <- reconstruct_original_all2020(original)
    fixed <- readr::read_csv(fix_path, show_col_types = FALSE)

    reconstructed |>
      left_join(
        fixed |>
          rename_with(~ paste0(.x, "_fix"), c(starts_with("count_"), starts_with("pop_"), starts_with("area_"))),
        by = c("iso3", "adm_level", "adm_name", "adm_id", "source")
      ) |>
      transmute(
        iso3,
        adm_level,
        adm_name,
        adm_id,
        source,
        pop_total,
        pop_inside_reconstructed = pop_strict + pop_nonstrict + pop_unknowncat,
        pop_inside_fixed = pop_strict_fix + pop_nonstrict_fix + pop_unknowncat_fix,
        pop_10km_reconstructed = pop_strict10 + pop_nonstrict10 + pop_unknowncat10,
        pop_10km_fixed = pop_strict10_fix + pop_nonstrict10_fix + pop_unknowncat10_fix,
        inside_diff = pop_inside_reconstructed - pop_inside_fixed,
        near_diff = pop_10km_reconstructed - pop_10km_fixed,
        share_inside_reconstructed = pop_inside_reconstructed / pop_total,
        share_inside_fixed = pop_inside_fixed / pop_total,
        share_10km_reconstructed = pop_10km_reconstructed / pop_total,
        share_10km_fixed = pop_10km_fixed / pop_total
      )
  })

  readr::write_csv(rows, output_csv, na = "")
  invisible(rows)
}

args <- commandArgs(trailingOnly = TRUE)

if (sys.nframe() == 0) {
  compare_original_vs_fix(
    original_dir = ifelse(length(args) >= 1, args[[1]], "data/Output_R_mapme_reviewed_original"),
    fix_dir = ifelse(length(args) >= 2, args[[2]], "data/Output_R_mapme_all2020_fix"),
    output_csv = ifelse(length(args) >= 3, args[[3]], "data/tests/mapme_original_vs_fix_comparison.csv")
  )
}