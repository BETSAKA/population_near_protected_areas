suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(sf)
  library(stringr)
  library(tibble)
})

sf::sf_use_s2(FALSE)

s3_prefix <- "s3://projet-betsaka/diffusion/population_pas/wdpa_as_gee"
s3_listing_prefix <- paste0(s3_prefix, "/")
local_dir <- "tmp/wdpa_as_gee_audit"

ensure_local_file <- function(file_name) {
  local_path <- file.path(local_dir, file_name)

  if (!file.exists(local_path)) {
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    system2(
      "aws",
      c("s3", "cp", file.path(s3_prefix, file_name), local_path, "--only-show-errors"),
      stdout = TRUE,
      stderr = TRUE
    )
  }

  local_path
}

list_remote_files <- function() {
  lines <- system2("aws", c("s3", "ls", s3_listing_prefix), stdout = TRUE, stderr = TRUE)

  tibble(raw = lines) |>
    filter(str_detect(raw, "WDPA_202105_.*\\.geojson$")) |>
    mutate(file_name = str_extract(raw, "WDPA_202105_.*\\.geojson$")) |>
    pull(file_name)
}

inspect_one <- function(file_name) {
  iso_target <- file_name |>
    str_remove("^WDPA_202105_") |>
    str_remove("\\.geojson$")

  wdpa <- ensure_local_file(file_name) |>
    read_sf(quiet = TRUE)

  if (nrow(wdpa) == 0) {
    return(
      tibble(
        iso_target = iso_target,
        n_features = 0L,
        n_foreign_iso3 = 0L,
        foreign_iso3 = "",
        max_foreign_geom_area_km2 = NA_real_,
        max_foreign_area_ratio = NA_real_,
        suspect_rows = 0L
      )
    )
  }

  area_col <- intersect(c("REP_AREA", "GIS_AREA", "AREA_KM2", "REP_M_AREA"), names(wdpa))
  iso_col <- intersect(c("ISO3", "PARENT_ISO3", "ISO3_CODE"), names(wdpa))

  wdpa <- wdpa |>
    mutate(
      feature_iso3 = if (length(iso_col) > 0) as.character(.data[[iso_col[[1]]]]) else NA_character_,
      area_attr_km2 = if (length(area_col) > 0) suppressWarnings(as.numeric(.data[[area_col[[1]]]])) else NA_real_,
      geom_area_km2 = as.numeric(st_area(st_transform(geometry, 6933))) / 1e6,
      area_ratio = if_else(!is.na(area_attr_km2) & area_attr_km2 > 0, geom_area_km2 / area_attr_km2, NA_real_),
      foreign_iso = !is.na(feature_iso3) & feature_iso3 != iso_target,
      suspect = foreign_iso | (!is.na(area_ratio) & area_ratio < 0.02)
    )

  foreign_rows <- wdpa |>
    filter(foreign_iso)

  tibble(
    iso_target = iso_target,
    n_features = nrow(wdpa),
    n_foreign_iso3 = nrow(foreign_rows),
    foreign_iso3 = paste(sort(unique(foreign_rows$feature_iso3)), collapse = ","),
    max_foreign_geom_area_km2 = if (nrow(foreign_rows) == 0) NA_real_ else max(foreign_rows$geom_area_km2, na.rm = TRUE),
    max_foreign_area_ratio = if (nrow(foreign_rows) == 0) NA_real_ else max(foreign_rows$area_ratio, na.rm = TRUE),
    suspect_rows = sum(wdpa$suspect, na.rm = TRUE)
  )
}

summary_tbl <- list_remote_files() |>
  map_dfr(inspect_one) |>
  arrange(desc(n_foreign_iso3), desc(suspect_rows), iso_target)

write_csv(summary_tbl, "results/reproduction_runs/reviewed_refactor/wdpa_neighbor_audit.csv")

summary_tbl |>
  filter(n_foreign_iso3 > 0 | suspect_rows > 0) |>
  slice_head(n = 50) |>
  print(n = 50)

cat("total_with_foreign=", sum(summary_tbl$n_foreign_iso3 > 0), "\n", sep = "")
