suppressPackageStartupMessages({
  library(readr)
  library(tibble)
})

source("code/replicate_gee_common.R")

# This runs one country and one source.
# It writes the original file, the fixed file, a status file, and a small profile file.

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop(
    paste(
      "Usage:",
      "Rscript code/run_reproduction_task.R",
      "<iso> <source>",
      "[run_dir] [original_dir] [fix_dir] [cache_dir] [wdpa_dir] [wdpa_oct_zip]",
      "[wdpa_oct_extract_dir] [use_land_mask=true|false]"
    )
  )
}

iso <- args[[1]]
source_name <- args[[2]]
run_dir <- if (length(args) >= 3) args[[3]] else file.path("results", "reproduction_runs", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
original_dir <- if (length(args) >= 4) args[[4]] else "data/Output_R_mapme_reviewed_original"
fix_dir <- if (length(args) >= 5) args[[5]] else "data/Output_R_mapme_all2020_fix"
cache_dir <- if (length(args) >= 6) args[[6]] else "data/cache_direct"
wdpa_dir <- if (length(args) >= 7) args[[7]] else "data/WDPA_2021_05_GEE"
wdpa_oct_zip <- if (length(args) >= 8) args[[8]] else "/tmp/WDPA_2021.zip"
wdpa_oct_extract_dir <- if (length(args) >= 9) args[[9]] else file.path(cache_dir, "wdpa_oct2021")
use_land_mask <- if (length(args) >= 10) tolower(args[[10]]) == "true" else TRUE

original_output <- file.path(original_dir, sprintf("PA_Pop_%s_%s.csv", iso, source_name))
fix_output <- file.path(fix_dir, sprintf("PA_Pop_%s_%s.csv", iso, source_name))

task_id <- sprintf("%s_%s", iso, source_name)
status_dir <- file.path(run_dir, "status")
meta_dir <- file.path(run_dir, "meta")
profile_dir <- file.path(run_dir, "profiles")

ensure_dir(status_dir)
ensure_dir(meta_dir)
ensure_dir(profile_dir)
ensure_dir(original_dir)
ensure_dir(fix_dir)

status_path <- file.path(status_dir, paste0(task_id, ".csv"))
session_path <- file.path(meta_dir, paste0(task_id, "_session.txt"))
profile_path <- file.path(profile_dir, paste0(task_id, ".csv"))

writeLines(capture.output(sessionInfo()), session_path)

dir_size_bytes <- function(path) {
  if (!dir.exists(path)) {
    return(0)
  }
  files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0) {
    return(0)
  }
  info <- file.info(files)
  sum(info$size[!is.na(info$size) & !info$isdir], na.rm = TRUE)
}

cache_size_before <- dir_size_bytes(cache_dir)

started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

write_status <- function(status, message = "", started_at, finished_at = NA_character_, elapsed_seconds = NA_real_) {
# This keeps a simple record of where the task stopped or finished.
  write_csv(
    tibble(
      task_id = task_id,
      iso3 = iso,
      source = source_name,
      status = status,
      message = message,
      started_at = started_at,
      finished_at = finished_at,
      elapsed_seconds = elapsed_seconds,
      original_output = original_output,
      fix_output = fix_output
    ),
    status_path,
    na = ""
  )
}

write_status("started", started_at = started_at)

time_taken <- system.time({
  result <- tryCatch(
    {
# This writes the original output first and the fixed output after that.
      boundary_info <- load_boundary_units(iso, cache_dir)
      pop_iso <- boundary_info$boundary_iso

      if (!file.exists(original_output)) {
        write_status("running_original", started_at = started_at)
        original_df <- compute_country_source_original(
          iso = iso,
          source_name = source_name,
          cache_dir = cache_dir,
          wdpa_dir = wdpa_dir,
          wdpa_oct_zip = wdpa_oct_zip,
          wdpa_oct_extract_dir = wdpa_oct_extract_dir,
          use_land_mask = use_land_mask
        )
        write_csv_atomic(original_df, original_output)
      }

      write_status("original_done", started_at = started_at)

      if (!file.exists(fix_output)) {
        write_status("running_fix", started_at = started_at)
        fix_df <- compute_country_source_fix(
          iso = iso,
          source_name = source_name,
          cache_dir = cache_dir,
          wdpa_dir = wdpa_dir,
          wdpa_oct_zip = wdpa_oct_zip,
          wdpa_oct_extract_dir = wdpa_oct_extract_dir,
          use_land_mask = use_land_mask
        )
        write_csv_atomic(fix_df, fix_output)
      }

      if (source_name == "WP") {
        cleanup_task_cache(cache_dir, pop_iso)
      }

      list(ok = TRUE, message = "")
    },
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    }
  )
})

finished_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
cache_size_after <- dir_size_bytes(cache_dir)
original_bytes <- if (file.exists(original_output)) file.info(original_output)$size else NA_real_
fix_bytes <- if (file.exists(fix_output)) file.info(fix_output)$size else NA_real_

write_csv(
  tibble(
    task_id = task_id,
    iso3 = iso,
    source = source_name,
    cache_size_before_bytes = cache_size_before,
    cache_size_after_bytes = cache_size_after,
    original_output_bytes = original_bytes,
    fix_output_bytes = fix_bytes,
    elapsed_seconds = unname(time_taken[["elapsed"]]),
    user_seconds = unname(time_taken[["user.self"]]),
    system_seconds = unname(time_taken[["sys.self"]]),
    gc_trigger_mb = sum(gc()[, 6], na.rm = TRUE) / 1024
  ),
  profile_path,
  na = ""
)

if (isTRUE(result$ok)) {
  write_status(
    status = "success",
    message = result$message,
    started_at = started_at,
    finished_at = finished_at,
    elapsed_seconds = unname(time_taken[["elapsed"]])
  )
} else {
  write_status(
    status = "failed",
    message = result$message,
    started_at = started_at,
    finished_at = finished_at,
    elapsed_seconds = unname(time_taken[["elapsed"]])
  )
  stop(result$message)
}