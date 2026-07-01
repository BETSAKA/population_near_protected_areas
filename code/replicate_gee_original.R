source("code/replicate_gee_common.R")

# This runs the original reviewed GEE logic in R.
# It keeps the separate Confirmed_2000, Confirmed_2020, and Unknown_Year rows.

run_reviewed_reproduction_original <- function(
  iso_subset = ISO3_LIST_REVIEWED,
  sources = c("GHSL", "WP"),
  output_dir = "data/Output_R_mapme_reviewed_original",
  cache_dir = "data/cache_direct",
  wdpa_dir = "data/WDPA_2021_05_GEE",
  wdpa_oct_zip = "/tmp/WDPA_2021.zip",
  wdpa_oct_extract_dir = "data/cache_wdpa_oct2021",
  use_land_mask = TRUE,
  skip_missing_wdpa = TRUE
) {
# This writes one output file per country and source.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(wdpa_oct_extract_dir, recursive = TRUE, showWarnings = FALSE)

  unavailable <- list()

  for (iso in iso_subset) {
# This loads the boundary and WDPA data for one country.
    boundary_info <- load_boundary_units(iso, cache_dir)

    wdpa_info <- tryCatch(
      load_wdpa_country(iso, wdpa_dir, wdpa_oct_zip, wdpa_oct_extract_dir),
      error = function(e) e
    )

    if (inherits(wdpa_info, "error")) {
      if (!skip_missing_wdpa) {
        stop(wdpa_info)
      }
      unavailable[[iso]] <- wdpa_info$message
      next
    }

    for (source_name in sources) {
# This loops over GHSL and WorldPop.
      rows <- purrr::map_dfr(seq_len(nrow(boundary_info$regions)), function(i) {
        region <- boundary_info$regions[i, ]
        purrr::pmap_dfr(
          SCENARIOS_ORIGINAL,
          function(scenario, pop_year) {
            compute_region_result(
              region = region,
              iso = iso,
              adm_level = boundary_info$adm_level,
              source = source_name,
              scenario = scenario,
              pop_year = pop_year,
              wdpa_country = wdpa_info$data,
              cache_dir = cache_dir,
              pop_iso = boundary_info$boundary_iso,
              use_land_mask = use_land_mask
            )
          }
        )
      })

      out_path <- file.path(output_dir, sprintf("PA_Pop_%s_%s.csv", iso, source_name))
      readr::write_csv(rows, out_path, na = "")
    }
  }

  if (length(unavailable) > 0) {
    unavailable_tbl <- tibble::tibble(
      iso3 = names(unavailable),
      reason = unlist(unavailable, use.names = FALSE)
    )
    readr::write_csv(unavailable_tbl, file.path(output_dir, "unavailable_wdpa_sources.csv"), na = "")
  }

  invisible(output_dir)
}

args <- commandArgs(trailingOnly = TRUE)

if (sys.nframe() == 0) {
  iso_subset <- if (length(args) == 0) ISO3_LIST_REVIEWED else strsplit(args[[1]], ",", fixed = TRUE)[[1]]
  run_reviewed_reproduction_original(iso_subset = iso_subset)
}