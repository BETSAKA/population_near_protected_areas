suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript code/summarise_reproduction_run.R <run_dir>")
}

run_dir <- args[[1]]
status_files <- Sys.glob(file.path(run_dir, "status", "*.csv"))

if (length(status_files) == 0) {
  stop("No status files found in ", file.path(run_dir, "status"))
}

status_spec <- cols(iso3 = col_character(), task_id = col_character(), source = col_character(), status = col_character(), message = col_character(), started_at = col_character(), finished_at = col_character(), elapsed_seconds = col_double(), original_output = col_character(), fix_output = col_character())

summary_tbl <- bind_rows(lapply(status_files, function(path) read_csv(path, col_types = status_spec, show_col_types = FALSE))) |>
  arrange(source, iso3)

write_csv(summary_tbl, file.path(run_dir, "run_summary.csv"), na = "")
print(summary_tbl |>
  count(status, name = "n_tasks") |>
  arrange(desc(n_tasks)))

invisible(summary_tbl)