suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
})

copy_new_files <- function(from_dir, to_dir, pattern = "*.csv") {
  dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
  files <- Sys.glob(file.path(from_dir, pattern))
  copied <- purrr::map_lgl(files, function(path) {
    target <- file.path(to_dir, basename(path))
    if (file.exists(target)) {
      return(FALSE)
    }
    ok <- file.copy(path, target)
    if (!ok) {
      stop("Failed to copy ", path, " to ", target)
    }
    TRUE
  })
  tibble(file = basename(files), copied = copied)
}

merge_status_tables <- function(run_dirs, output_path) {
  status_files <- unlist(lapply(run_dirs, function(run_dir) Sys.glob(file.path(run_dir, "status", "*.csv"))), use.names = FALSE)
  if (length(status_files) == 0) {
    return(tibble())
  }
  spec <- cols(
    task_id = col_character(),
    iso3 = col_character(),
    source = col_character(),
    status = col_character(),
    message = col_character(),
    started_at = col_character(),
    finished_at = col_character(),
    elapsed_seconds = col_double(),
    original_output = col_character(),
    fix_output = col_character()
  )
  merged <- bind_rows(lapply(status_files, function(path) read_csv(path, col_types = spec, show_col_types = FALSE))) |>
    group_by(task_id) |>
    arrange(desc(finished_at), desc(started_at)) |>
    slice(1) |>
    ungroup()
  write_csv(merged, output_path, na = "")
  merged
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 5) {
    stop(
      paste(
        "Usage:",
        "Rscript code/merge_reproduction_shards.R",
        "<target_original_dir> <target_fix_dir> <target_status_csv>",
        "<original_dir_1,original_dir_2,...> <fix_dir_1,fix_dir_2,...> [run_dir_1,run_dir_2,...]"
      )
    )
  }

  target_original_dir <- args[[1]]
  target_fix_dir <- args[[2]]
  target_status_csv <- args[[3]]
  original_sources <- strsplit(args[[4]], ",", fixed = TRUE)[[1]]
  fix_sources <- strsplit(args[[5]], ",", fixed = TRUE)[[1]]
  run_dirs <- if (length(args) >= 6) strsplit(args[[6]], ",", fixed = TRUE)[[1]] else character()

  original_copy <- bind_rows(lapply(original_sources, copy_new_files, to_dir = target_original_dir))
  fix_copy <- bind_rows(lapply(fix_sources, copy_new_files, to_dir = target_fix_dir))
  status_tbl <- if (length(run_dirs) > 0) merge_status_tables(run_dirs, target_status_csv) else tibble()

  cat("Original files copied:", sum(original_copy$copied, na.rm = TRUE), "\n")
  cat("Fixed files copied:", sum(fix_copy$copied, na.rm = TRUE), "\n")
  cat("Merged status rows:", nrow(status_tbl), "\n")
}

main()