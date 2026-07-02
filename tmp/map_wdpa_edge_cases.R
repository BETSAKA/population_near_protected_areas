suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(stringr)
})

sf::sf_use_s2(FALSE)

load_case_boundary <- function(iso) {
  if (iso %in% c("118", "129")) {
    region_name <- c("118" = "Gaza", "129" = "West Bank")[[iso]]
    sf::read_sf(
      "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/PSE/ADM1/geoBoundaries-PSE-ADM1.geojson",
      quiet = TRUE
    ) |>
      filter(shapeName == region_name) |>
      summarise() |>
      mutate(case_iso = iso, case_label = region_name)
  } else {
    sf::read_sf(
      sprintf(
        "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/%s/ADM0/geoBoundaries-%s-ADM0.geojson",
        iso,
        iso
      ),
      quiet = TRUE
    ) |>
      summarise() |>
      mutate(case_iso = iso, case_label = iso)
  }
}

load_case_pa <- function(iso) {
  path <- file.path("tmp", "wdpa_as_gee", sprintf("WDPA_202105_%s.geojson", iso))
  x <- sf::read_sf(path, quiet = TRUE)
  if (nrow(x) == 0) {
    x <- st_as_sf(tibble::tibble(case_iso = character(), case_label = character(), geometry = st_sfc(crs = 4326)))
  } else {
    x <- x |>
      mutate(case_iso = iso, case_label = iso)
  }
  x
}

cases <- c("118", "129", "SOM", "BGD")
case_names <- c("118" = "Gaza", "129" = "West Bank", "SOM" = "Somalia", "BGD" = "Bangladesh")

boundaries <- setNames(lapply(cases, load_case_boundary), cases)
pas_raw <- setNames(lapply(cases, load_case_pa), cases)

pa_counts <- tibble(case_iso = cases) |>
  left_join(
    tibble(
      case_iso = cases,
      n_pa = vapply(pas_raw, nrow, integer(1))
    ),
    by = "case_iso"
  ) |>
  mutate(
    n_pa = coalesce(n_pa, 0L),
    case_label = sprintf("%s\n%s WDPA features", case_names[case_iso], n_pa)
  )

out_path <- file.path("results", "reproduction_runs", "reviewed_refactor", "wdpa_strategy_edge_cases.png")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
png(out_path, width = 1600, height = 1200, res = 160)
par(mfrow = c(2, 2), mar = c(1, 1, 3, 1), oma = c(0, 0, 3, 0))

for (iso in cases) {
  boundary <- boundaries[[iso]]
  pa <- pas_raw[[iso]]
  label <- pa_counts$case_label[pa_counts$case_iso == iso]

  plot(st_geometry(boundary), col = "#f4efe6", border = "#5b564d", main = label)
  if (nrow(pa) > 0) {
    plot(st_geometry(pa), col = adjustcolor("#2e8b57", alpha.f = 0.7), border = "#145a32", add = TRUE)
  }
  box()
}

mtext(
  "WDPA spatial slices from s3://projet-betsaka/diffusion/population_pas/wdpa_as_gee\nGaza, West Bank, and Somalia are empty after spatial filtering; Bangladesh is a control.",
  outer = TRUE,
  cex = 1
)

dev.off()
write.csv(pa_counts, file.path("results", "reproduction_runs", "reviewed_refactor", "wdpa_strategy_edge_cases_counts.csv"), row.names = FALSE)
cat(out_path, "\n")
