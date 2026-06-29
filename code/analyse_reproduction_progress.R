suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
})

read_status_dir <- function(run_dir) {
  status_files <- Sys.glob(file.path(run_dir, "status", "*.csv"))
  if (length(status_files) == 0) {
    stop("No status files found in ", file.path(run_dir, "status"))
  }
  status_spec <- cols(iso3 = col_character(), task_id = col_character(), source = col_character(), status = col_character(), message = col_character(), started_at = col_character(), finished_at = col_character(), elapsed_seconds = col_double(), original_output = col_character(), fix_output = col_character())
  bind_rows(lapply(status_files, function(path) read_csv(path, col_types = status_spec, show_col_types = FALSE)))
}

read_manifest <- function(run_dir) {
  read_tsv(
    file.path(run_dir, "manifests", "tasks.tsv"),
    col_names = c("iso3", "source"),
    show_col_types = FALSE
  ) |>
    mutate(task_id = paste(iso3, source, sep = "_"))
}

estimate_eta <- function(status_tbl, manifest_tbl, max_parallel = 2L) {
  success_tbl <- status_tbl |>
    filter(status == "success", !is.na(elapsed_seconds))

  source_means <- success_tbl |>
    group_by(source) |>
    summarise(mean_elapsed_seconds = mean(elapsed_seconds, na.rm = TRUE), .groups = "drop")

  remaining_tbl <- manifest_tbl |>
    left_join(status_tbl |> select(task_id, status, started_at), by = "task_id") |>
    mutate(task_class = case_when(
      status == "success" ~ "done",
      status %in% c("running_original", "running_fix", "original_done", "started") ~ "running",
      status == "failed" ~ "retry",
      TRUE ~ "pending"
    ))

  running_adjustment <- remaining_tbl |>
    filter(task_class == "running") |>
    mutate(started_at = as.POSIXct(started_at, tz = "UTC")) |>
    mutate(runtime_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))) |>
    left_join(source_means, by = "source") |>
    mutate(remaining_seconds = pmax(mean_elapsed_seconds - runtime_seconds, 0)) |>
    group_by(source) |>
    summarise(remaining_seconds_running = sum(remaining_seconds, na.rm = TRUE), .groups = "drop")

  pending_retry <- remaining_tbl |>
    filter(task_class %in% c("pending", "retry")) |>
    count(source, name = "n_tasks") |>
    left_join(source_means, by = "source") |>
    mutate(remaining_seconds_pending = n_tasks * mean_elapsed_seconds)

  all_sources <- tibble(source = sort(unique(manifest_tbl$source)))

  eta_tbl <- all_sources |>
    left_join(select(pending_retry, source, n_tasks, mean_elapsed_seconds, remaining_seconds_pending), by = "source") |>
    left_join(select(running_adjustment, source, remaining_seconds_running), by = "source") |>
    mutate(
      n_tasks = coalesce(n_tasks, 0L),
      mean_elapsed_seconds = coalesce(mean_elapsed_seconds, 0),
      remaining_seconds_pending = coalesce(remaining_seconds_pending, 0),
      remaining_seconds_running = coalesce(remaining_seconds_running, 0),
      eta_source_seconds = remaining_seconds_pending + remaining_seconds_running
    ) |>
    select(source, n_tasks, mean_elapsed_seconds, eta_source_seconds)

  total_eta_seconds <- sum(eta_tbl$eta_source_seconds, na.rm = TRUE) / max_parallel

  list(
    eta_by_source = eta_tbl,
    total_eta_seconds = total_eta_seconds,
    task_state = remaining_tbl
  )
}

analyse_original_vs_fix <- function(original_dir, fix_dir) {
  orig_files <- Sys.glob(file.path(original_dir, "PA_Pop_*_*.csv"))
  fix_files <- Sys.glob(file.path(fix_dir, "PA_Pop_*_*.csv"))
  common <- intersect(basename(orig_files), basename(fix_files))

  if (length(common) == 0) {
    return(tibble())
  }

  map_dfr(common, function(fn) {
    orig <- read_csv(file.path(original_dir, fn), show_col_types = FALSE)
    fix <- read_csv(file.path(fix_dir, fn), show_col_types = FALSE)

    rec <- orig |>
      filter(scenario %in% c("Confirmed_2020", "Unknown_Year")) |>
      group_by(iso3, adm_name, adm_level, adm_id, source) |>
      summarise(
        pop_total = first(pop_total[scenario == "Confirmed_2020"]),
        pop_inside_reconstructed = sum(pop_strict + pop_nonstrict + pop_unknowncat),
        pop_near_reconstructed = sum(pop_strict10 + pop_nonstrict10 + pop_unknowncat10),
        .groups = "drop"
      )

    fx <- fix |>
      mutate(
        pop_inside_fixed = pop_strict + pop_nonstrict + pop_unknowncat,
        pop_near_fixed = pop_strict10 + pop_nonstrict10 + pop_unknowncat10
      ) |>
      select(iso3, adm_name, adm_level, adm_id, source, pop_total_fix = pop_total, pop_inside_fixed, pop_near_fixed)

    left_join(rec, fx, by = c("iso3", "adm_name", "adm_level", "adm_id", "source")) |>
      mutate(
        file = fn,
        inside_diff = pop_inside_reconstructed - pop_inside_fixed,
        near_diff = pop_near_reconstructed - pop_near_fixed,
        share_inside_reconstructed = pop_inside_reconstructed / pop_total,
        share_inside_fixed = pop_inside_fixed / pop_total_fix,
        share_near_reconstructed = pop_near_reconstructed / pop_total,
        share_near_fixed = pop_near_fixed / pop_total_fix
      )
  })
}

analyse_reproduction_mismatch <- function(current_dir, reviewed_dir) {
  current_files <- Sys.glob(file.path(current_dir, "PA_Pop_*_*.csv"))
  if (length(current_files) == 0) {
    return(tibble())
  }

  map_dfr(current_files, function(path) {
    fn <- basename(path)
    ref_path <- file.path(reviewed_dir, fn)
    if (!file.exists(ref_path)) {
      return(NULL)
    }

    new <- read_csv(path, show_col_types = FALSE)
    ref <- read_csv(ref_path, show_col_types = FALSE, col_types = cols(iso3 = col_character(), .default = col_guess()))
    joined <- left_join(new, ref, by = c("adm_name", "scenario", "source"), suffix = c("_new", "_ref"))

    tibble(
      file = fn,
      source = unique(new$source)[1],
      max_pop_total_abs_diff = max(abs(joined$pop_total_new - joined$pop_total_ref), na.rm = TRUE),
      mean_pop_total_abs_diff = mean(abs(joined$pop_total_new - joined$pop_total_ref), na.rm = TRUE)
    )
  })
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("Usage: Rscript code/analyse_reproduction_progress.R <run_dir> [original_dir] [fix_dir] [reviewed_dir] [max_parallel]")
  }

  run_dir <- args[[1]]
  original_dir <- if (length(args) >= 2) args[[2]] else "data/Output_R_mapme_reviewed_original"
  fix_dir <- if (length(args) >= 3) args[[3]] else "data/Output_R_mapme_all2020_fix"
  reviewed_dir <- if (length(args) >= 4) args[[4]] else "data/reviewed_PA_Pop_GHSL_Worldpop"
  max_parallel <- if (length(args) >= 5) as.integer(args[[5]]) else 2L

  analysis_dir <- file.path(run_dir, "analysis")
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

  status_tbl <- read_status_dir(run_dir)
  manifest_tbl <- read_manifest(run_dir)
  eta_info <- estimate_eta(status_tbl, manifest_tbl, max_parallel = max_parallel)

  state_counts <- eta_info$task_state |>
    count(task_class, name = "n_tasks") |>
    arrange(desc(n_tasks))
  write_csv(state_counts, file.path(analysis_dir, "task_state_counts.csv"), na = "")

  eta_tbl <- eta_info$eta_by_source
  eta_summary <- tibble(
    total_tasks = nrow(manifest_tbl),
    status_files = nrow(status_tbl),
    success_tasks = sum(status_tbl$status == "success", na.rm = TRUE),
    failed_tasks = sum(status_tbl$status == "failed", na.rm = TRUE),
    running_tasks = sum(status_tbl$status %in% c("started", "original_done", "running_original", "running_fix"), na.rm = TRUE),
    max_parallel = max_parallel,
    eta_total_seconds = eta_info$total_eta_seconds,
    eta_total_hours = eta_info$total_eta_seconds / 3600
  )
  write_csv(eta_tbl, file.path(analysis_dir, "eta_by_source.csv"), na = "")
  write_csv(eta_summary, file.path(analysis_dir, "eta_summary.csv"), na = "")

  failed_tbl <- status_tbl |>
    filter(status == "failed") |>
    arrange(source, iso3)
  write_csv(failed_tbl, file.path(analysis_dir, "failed_tasks.csv"), na = "")

  original_vs_fix <- analyse_original_vs_fix(original_dir, fix_dir)
  write_csv(original_vs_fix, file.path(analysis_dir, "original_vs_fixed_completed.csv"), na = "")

  mismatch_tbl <- analyse_reproduction_mismatch(original_dir, reviewed_dir)
  write_csv(mismatch_tbl, file.path(analysis_dir, "reproduction_vs_reviewed_completed.csv"), na = "")

  cat("Run:", run_dir, "\n")
  print(eta_summary)
  cat("\nTask state counts\n")
  print(state_counts)
  cat("\nTop original-vs-fixed completed diffs\n")
  print(
    original_vs_fix |>
      arrange(desc(abs(near_diff))) |>
      select(file, iso3, adm_name, source, inside_diff, near_diff, share_near_reconstructed, share_near_fixed) |>
      head(15)
  )
  cat("\nTop reproduction-vs-reviewed completed mismatches\n")
  print(
    mismatch_tbl |>
      arrange(desc(max_pop_total_abs_diff)) |>
      head(15)
  )
}

main()