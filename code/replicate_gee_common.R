suppressPackageStartupMessages({
  library(dplyr)
  library(exactextractr)
  library(readr)
  library(sf)
  library(terra)
})

# This file holds the shared pieces for the R replication.
# The country loops in the other files call these helpers.

sf::sf_use_s2(FALSE)

BUFFER_METERS <- 10000
WORK_CRS <- 6933
WORLD_COVER_TILE_ROOT <- "/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/v100/2020/map"
POP_PA_S3_PREFIX <- Sys.getenv("POP_PA_S3_PREFIX", "")

# This is the full list used in the reviewed GEE code.
ISO3_LIST_REVIEWED <- c(
  "AFG", "AGO", "BGD", "BEN", "BTN", "BOL", "BFA", "BDI", "CPV", "KHM",
  "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "EGY", "SLV",
  "ERI", "SWZ", "ETH", "GMB", "GHA", "GIN", "GNB", "HTI", "HND", "IND",
  "IDN", "KEN", "KIR", "PRK", "KGZ", "LAO", "LSO", "LBR", "MDG", "MWI",
  "MLI", "MRT", "FSM", "MDA", "MNG", "MAR", "MOZ", "MMR", "NPL", "NIC",
  "NER", "NGA", "PAK", "PNG", "PHL", "RWA", "STP", "SEN", "SLE", "SLB",
  "SOM", "SSD", "SDN", "SYR", "TJK", "TZA", "TLS", "TGO", "TUN", "UGA",
  "UKR", "UZB", "VUT", "VNM", "118", "129", "YEM", "ZMB", "ZWE"
)

# This is the paper sample after dropping India.
ISO3_LIST_PAPER <- setdiff(ISO3_LIST_REVIEWED, "IND")

# These are the three rows used in the original GEE workflow.
SCENARIOS_ORIGINAL <- tibble::tribble(
  ~scenario, ~pop_year,
  "Confirmed_2000", 2000,
  "Confirmed_2020", 2020,
  "Unknown_Year", 2020
)

# These two codes are the Palestine split used in the paper files.
SPECIAL_BOUNDARY_NAMES <- c("118" = "Gaza", "129" = "West Bank")
FALLBACK_WDPA_ISOS <- c("118" = "PSE", "129" = "PSE")

geom_empty <- function(crs = WORK_CRS) {
  sf::st_sfc(sf::st_multipolygon(), crs = crs)
}

geom_is_empty <- function(x) {
  length(x) == 0 || all(sf::st_is_empty(x))
}

geom_make_valid <- function(x, crs = NULL) {
  if (inherits(x, "sf")) {
    x <- sf::st_geometry(x)
  }
  if (geom_is_empty(x)) {
    return(geom_empty(if (is.null(crs)) sf::st_crs(x) else crs))
  }
  out <- sf::st_make_valid(x)
  out <- sf::st_collection_extract(out, "POLYGON", warn = FALSE)
  if (geom_is_empty(out)) {
    return(geom_empty(if (is.null(crs)) sf::st_crs(x) else crs))
  }
  out
}

geom_union_safe <- function(x, y) {
  if (geom_is_empty(x)) {
    return(y)
  }
  if (geom_is_empty(y)) {
    return(x)
  }
  geom_make_valid(sf::st_union(x, y), sf::st_crs(x))
}

geom_difference_safe <- function(x, y) {
  if (geom_is_empty(x)) {
    return(x)
  }
  if (geom_is_empty(y)) {
    return(x)
  }
  out <- suppressWarnings(sf::st_difference(x, y))
  geom_make_valid(out, sf::st_crs(x))
}

geom_intersection_safe <- function(x, y) {
  if (geom_is_empty(x) || geom_is_empty(y)) {
    return(geom_empty(sf::st_crs(x)))
  }
  out <- suppressWarnings(sf::st_intersection(x, y))
  geom_make_valid(out, sf::st_crs(x))
}

geom_buffer_safe <- function(x, dist) {
  if (geom_is_empty(x)) {
    return(x)
  }
  geom_make_valid(sf::st_buffer(x, dist = dist), sf::st_crs(x))
}

make_geom_sf <- function(geom) {
  if (geom_is_empty(geom)) {
    return(sf::st_sf(id = integer(), geometry = geom))
  }
  geom_one <- if (length(geom) > 1) geom_make_valid(sf::st_union(geom), sf::st_crs(geom)) else geom
  sf::st_sf(id = 1L, geometry = geom_one)
}

scenario_filter_mask <- function(wdpa, scenario) {
# This keeps the same year rules as the GEE code.
  if (scenario == "Confirmed_2000") {
    wdpa |> filter(STATUS_YR > 0, STATUS_YR <= 2000)
  } else if (scenario == "Confirmed_2020") {
    wdpa |> filter(STATUS_YR > 0, STATUS_YR <= 2020)
  } else if (scenario == "Unknown_Year") {
    wdpa |> filter(STATUS_YR == 0)
  } else if (scenario == "All_2020_Fixed") {
    wdpa |> filter(STATUS_YR == 0 | (STATUS_YR > 0 & STATUS_YR <= 2020))
  } else {
    stop("Unknown scenario: ", scenario)
  }
}

filter_wdpa_base <- function(wdpa) {
# This keeps the same basic WDPA filters as the paper.
  wdpa |>
    filter(STATUS %in% c("Designated", "Established", "Inscribed")) |>
    filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") |>
    filter(MARINE != "2")
}

load_boundary_units <- function(iso, cache_dir) {
# This loads ADM1 when it exists and ADM0 otherwise.
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (iso %in% names(SPECIAL_BOUNDARY_NAMES)) {
    url <- "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/PSE/ADM1/geoBoundaries-PSE-ADM1.geojson"
    x <- sf::read_sf(url, quiet = TRUE) |>
      filter(shapeName == SPECIAL_BOUNDARY_NAMES[[iso]]) |>
      mutate(shapeGroup = iso)
    return(list(regions = x, adm_level = 1L, boundary_iso = "PSE"))
  }

  adm1_url <- sprintf(
    "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/%s/ADM1/geoBoundaries-%s-ADM1.geojson",
    iso,
    iso
  )
  adm0_url <- sprintf(
    "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/%s/ADM0/geoBoundaries-%s-ADM0.geojson",
    iso,
    iso
  )

  adm1 <- tryCatch(sf::read_sf(adm1_url, quiet = TRUE), error = function(e) NULL)
  if (!is.null(adm1) && nrow(adm1) > 0) {
    return(list(regions = adm1, adm_level = 1L, boundary_iso = iso))
  }

  adm0 <- tryCatch(sf::read_sf(adm0_url, quiet = TRUE), error = function(e) NULL)
  if (is.null(adm0) || nrow(adm0) == 0) {
    stop("No geoBoundaries ADM0/ADM1 geometry found for ", iso)
  }
  list(regions = adm0, adm_level = 0L, boundary_iso = iso)
}

extract_wdpa_from_oct2021 <- function(target_iso, oct_zip, extract_dir) {
# This is only a fallback when the local May 2021 slice is missing.
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- sprintf("WDPA_Oct2021_%s", target_iso)
  shp_path <- file.path(extract_dir, paste0(stem, ".shp"))

  if (!file.exists(shp_path)) {
    utils::unzip(
      oct_zip,
      files = sprintf("WDPA_2021/%s.%s", stem, c("cpg", "dbf", "fix", "prj", "shp", "shx")),
      exdir = extract_dir,
      junkpaths = TRUE
    )
  }

  if (!file.exists(shp_path)) {
    return(NULL)
  }

  sf::read_sf(shp_path, quiet = TRUE)
}

load_wdpa_country <- function(iso, wdpa_dir, oct_zip = NULL, extract_dir = NULL) {
# This reads the local WDPA slice first.
# It only falls back to October 2021 when needed.
  may_path <- file.path(wdpa_dir, sprintf("WDPA_202105_%s.shp", iso))
  if (file.exists(may_path)) {
    return(list(data = sf::read_sf(may_path, quiet = TRUE), source = "wdpa_202105_local"))
  }

  if (iso == "SOM") {
    template <- sf::read_sf(file.path(wdpa_dir, "WDPA_202105_AFG.shp"), quiet = TRUE)[0, ]
    return(list(data = template, source = "wdpa_missing_zero_pa"))
  }

  if (!is.null(oct_zip)) {
    fallback_iso <- if (iso %in% names(FALLBACK_WDPA_ISOS)) FALLBACK_WDPA_ISOS[[iso]] else iso
    wdpa_oct <- extract_wdpa_from_oct2021(fallback_iso, oct_zip, extract_dir)
    if (!is.null(wdpa_oct)) {
      return(list(data = wdpa_oct, source = paste0("wdpa_oct2021_fallback_", fallback_iso)))
    }
  }

  stop("No WDPA vector source available for ", iso)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

run_cmd <- function(args) {
  out <- system2(args[[1]], args[-1], stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop(paste(c(args, out), collapse = "\n"))
  }
  invisible(out)
}

worldpop_public_url <- function(year, iso) {
  sprintf(
    "https://data.worldpop.org/GIS/Population/Global_2000_2020/%s/%s/%s_ppp_%s.tif",
    year,
    iso,
    tolower(iso),
    year
  )
}

ghsl_public_url <- function(year, resolution = "3ss") {
  sprintf(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E%s_GLOBE_R2023A_4326_%s/V1-0/GHS_POP_E%s_GLOBE_R2023A_4326_%s_V1_0.zip",
    year,
    resolution,
    year,
    resolution
  )
}

ensure_worldpop_file <- function(year, iso, cache_dir, s3_prefix = POP_PA_S3_PREFIX) {
# This gets the WorldPop raster for one country and one year.
  dst <- file.path(cache_dir, "worldpop", as.character(year), iso, sprintf("%s_ppp_%s.tif", tolower(iso), year))
  if (file.exists(dst)) {
    return(dst)
  }

  ensure_dir(dirname(dst))
  options(timeout = max(getOption("timeout"), 3600))

  copied <- FALSE
  if (nzchar(s3_prefix) && nzchar(Sys.which("aws"))) {
    s3_path <- sprintf("%s/worldpop/%s/%s/%s_ppp_%s.tif", s3_prefix, year, iso, tolower(iso), year)
    copied <- tryCatch({
      run_cmd(c("aws", "s3", "cp", s3_path, dst, "--only-show-errors"))
      TRUE
    }, error = function(e) FALSE)
  }

  if (!copied) {
    download.file(worldpop_public_url(year, iso), dst, mode = "wb", quiet = FALSE)
  }

  dst
}

ensure_ghsl_file <- function(year, cache_dir, s3_prefix = POP_PA_S3_PREFIX, resolution = "3ss") {
# This gets the GHSL raster for one year.
  tif_name <- sprintf("GHS_POP_E%s_GLOBE_R2023A_4326_%s_V1_0.tif", year, resolution)
  tif_path <- file.path(cache_dir, "ghsl", resolution, tif_name)
  if (file.exists(tif_path)) {
    return(tif_path)
  }

  ensure_dir(dirname(tif_path))
  options(timeout = max(getOption("timeout"), 3600))

  zip_name <- sprintf("GHS_POP_E%s_GLOBE_R2023A_4326_%s_V1_0.zip", year, resolution)
  zip_path <- file.path(cache_dir, "ghsl", resolution, zip_name)

  if (!file.exists(zip_path)) {
    copied <- FALSE
    if (nzchar(s3_prefix) && nzchar(Sys.which("aws"))) {
      s3_path <- sprintf("%s/ghsl/%s", s3_prefix, zip_name)
      copied <- tryCatch({
        run_cmd(c("aws", "s3", "cp", s3_path, zip_path, "--only-show-errors"))
        TRUE
      }, error = function(e) FALSE)
    }

    if (!copied) {
      download.file(ghsl_public_url(year, resolution), zip_path, mode = "wb", quiet = FALSE)
    }
  }

  utils::unzip(zip_path, files = tif_name, exdir = dirname(tif_path), overwrite = FALSE)
  if (tolower(Sys.getenv("GHSL_KEEP_ZIP", "false")) != "true" && file.exists(zip_path)) {
    unlink(zip_path)
  }
  tif_path
}

get_pop_raster_path <- function(source, year, cache_dir, pop_iso = NULL) {
  if (source == "WP") {
    if (is.null(pop_iso)) {
      stop("pop_iso is required for WorldPop")
    }
    ensure_worldpop_file(year, pop_iso, cache_dir)
  } else if (source == "GHSL") {
    ensure_ghsl_file(year, cache_dir)
  } else {
    stop("Unknown source: ", source)
  }
}

crop_extent_or_null <- function(r, extent_geom) {
  if (is.null(extent_geom)) {
    return(r)
  }

  ext_ll <- sf::st_transform(extent_geom, 4326)
  target_ext <- terra::ext(sf::st_bbox(ext_ll))
  overlap_ext <- tryCatch(
    terra::intersect(terra::ext(r), target_ext),
    error = function(e) NULL
  )

  if (is.null(overlap_ext)) {
    return(NULL)
  }

  terra::crop(r, overlap_ext)
}

get_pop_raster <- function(source, year, cache_dir, pop_iso = NULL, extent_geom = NULL, use_land_mask = TRUE, allow_empty_overlap = FALSE) {
# This reads the raster and clips it to the area we need.
  path <- get_pop_raster_path(source, year, cache_dir, pop_iso = pop_iso)
  if (!file.exists(path)) {
    stop("Missing population raster: ", path)
  }

  r <- terra::rast(path)
  if (source == "WP") {
    r <- terra::clamp(r, lower = -Inf, upper = 65534, values = FALSE)
  }

  if (!is.null(extent_geom)) {
    r <- crop_extent_or_null(r, extent_geom)
    if (is.null(r)) {
      if (allow_empty_overlap) {
        return(NULL)
      }
      stop("Population raster does not overlap requested extent")
    }
  }

  if (!use_land_mask) {
    return(r)
  }

  mask <- get_land_mask(r, extent_geom)
  terra::mask(r, mask, maskvalues = 0, updatevalue = NA)
}

format_worldcover_axis <- function(value, axis = c("lat", "lon")) {
  axis <- match.arg(axis)
  prefix <- if (axis == "lat") ifelse(value >= 0, "N", "S") else ifelse(value >= 0, "E", "W")
  width <- if (axis == "lat") 2 else 3
  paste0(prefix, sprintf(paste0("%0", width, "d"), abs(as.integer(value))))
}

worldcover_tile_urls <- function(extent_geom) {
  bbox <- sf::st_bbox(sf::st_transform(extent_geom, 4326))

  lon_seq <- seq(floor(bbox[["xmin"]] / 3) * 3, floor((bbox[["xmax"]] - 1e-9) / 3) * 3, by = 3)
  lat_seq <- seq(floor(bbox[["ymin"]] / 3) * 3, floor((bbox[["ymax"]] - 1e-9) / 3) * 3, by = 3)

  expand.grid(lat = lat_seq, lon = lon_seq, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      lat_code = vapply(lat, format_worldcover_axis, character(1), axis = "lat"),
      lon_code = vapply(lon, format_worldcover_axis, character(1), axis = "lon"),
      url = sprintf(
        "%s/ESA_WorldCover_10m_2020_v100_%s%s_Map.tif",
        WORLD_COVER_TILE_ROOT,
        lat_code,
        lon_code
      )
    ) |>
    dplyr::pull(url)
}

read_worldcover_tiles <- function(extent_geom) {
# This builds the land mask from the public WorldCover tiles.
  urls <- worldcover_tile_urls(extent_geom)
  crop_ext <- terra::ext(sf::st_bbox(sf::st_transform(extent_geom, 4326)))
  rasters <- lapply(urls, function(url) {
    tryCatch(
      terra::crop(terra::rast(url), crop_ext),
      error = function(e) NULL
    )
  })
  rasters <- Filter(Negate(is.null), rasters)

  if (length(rasters) == 0) {
    stop("No readable WorldCover tiles found for extent")
  }

  if (length(rasters) == 1) {
    return(rasters[[1]])
  }

  do.call(terra::merge, rasters)
}

get_land_mask <- function(template_raster, extent_geom) {
  if (is.null(extent_geom)) {
    stop("extent_geom is required for land-mask construction")
  }

  wc <- read_worldcover_tiles(extent_geom)
  ext_ll <- sf::st_transform(extent_geom, 4326)
  wc <- terra::crop(wc, terra::ext(sf::st_bbox(ext_ll)))
  land <- wc != 80
  terra::resample(land, template_raster, method = "near")
}

build_category_masks <- function(wdpa_sf) {
# This splits PAs into strict, non-strict, and unknown groups.
  if (nrow(wdpa_sf) == 0) {
    empty <- geom_empty(WORK_CRS)
    return(list(
      strict = empty,
      nonStrict = empty,
      unknownCat = empty,
      counts = c(strict = 0L, nonStrict = 0L, unknownCat = 0L)
    ))
  }

  wdpa_proj <- wdpa_sf |>
    sf::st_transform(WORK_CRS) |>
    mutate(geometry = sf::st_make_valid(geometry))

  strict_fc <- wdpa_proj |> filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III"))
  nonstrict_fc <- wdpa_proj |> filter(IUCN_CAT %in% c("IV", "V", "VI"))
  unknown_fc <- wdpa_proj |> filter(!IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI"))

  strict_geom <- if (nrow(strict_fc) == 0) geom_empty(WORK_CRS) else geom_make_valid(sf::st_union(strict_fc), WORK_CRS)
  nonstrict_geom <- if (nrow(nonstrict_fc) == 0) geom_empty(WORK_CRS) else geom_make_valid(sf::st_union(nonstrict_fc), WORK_CRS)
  unknown_geom <- if (nrow(unknown_fc) == 0) geom_empty(WORK_CRS) else geom_make_valid(sf::st_union(unknown_fc), WORK_CRS)

  nonstrict_final <- geom_difference_safe(nonstrict_geom, strict_geom)
  unknown_final <- geom_difference_safe(unknown_geom, geom_union_safe(strict_geom, nonstrict_final))

  list(
    strict = strict_geom,
    nonStrict = nonstrict_final,
    unknownCat = unknown_final,
    counts = c(strict = nrow(strict_fc), nonStrict = nrow(nonstrict_fc), unknownCat = nrow(unknown_fc))
  )
}

build_exclusive_slices <- function(category_masks, buffer_m = BUFFER_METERS) {
# This keeps the same no-double-counting rule as the GEE code.
  claimed <- geom_empty(WORK_CRS)
  out <- list()

  for (cat_name in c("strict", "nonStrict", "unknownCat")) {
    inside_geom <- geom_difference_safe(category_masks[[cat_name]], claimed)
    claimed <- geom_union_safe(claimed, inside_geom)

    buffer_raw <- geom_buffer_safe(category_masks[[cat_name]], buffer_m)
    buffer_geom <- geom_difference_safe(buffer_raw, claimed)
    claimed <- geom_union_safe(claimed, buffer_geom)

    out[[cat_name]] <- inside_geom
    out[[paste0(cat_name, "10")]] <- buffer_geom
  }

  out
}

exact_sum <- function(r, geom) {
# This sums raster values over one geometry.
  if (geom_is_empty(geom)) {
    return(0)
  }
  geom_ll <- sf::st_transform(make_geom_sf(geom), terra::crs(r))
  val <- exactextractr::exact_extract(r, geom_ll, "sum", progress = FALSE)
  if (length(val) == 0 || is.null(val)) {
    return(0)
  }
  if (is.data.frame(val)) {
    val <- unlist(val, use.names = FALSE)
  } else if (is.list(val)) {
    val <- unlist(val, recursive = TRUE, use.names = FALSE)
  } else if (is.matrix(val) || is.array(val)) {
    val <- as.vector(val)
  }
  val_num <- suppressWarnings(as.numeric(val))
  if (length(val_num) == 0 || all(is.na(val_num))) 0 else sum(val_num, na.rm = TRUE)
}

region_extent_with_buffer <- function(region) {
  region |>
    sf::st_transform(WORK_CRS) |>
    sf::st_buffer(BUFFER_METERS) |>
    sf::st_transform(4326)
}

compute_region_result <- function(region, iso, adm_level, source, scenario, pop_year, wdpa_country, cache_dir, pop_iso = iso, use_land_mask = TRUE) {
# This computes one region row in the same shape as the GEE output.
  region_proj <- sf::st_transform(region, WORK_CRS)
  search_geom <- region_extent_with_buffer(region)

  wdpa_subset <- wdpa_country |>
    filter_wdpa_base() |>
    sf::st_filter(search_geom, .predicate = sf::st_intersects) |>
    scenario_filter_mask(scenario)

  masks <- build_category_masks(wdpa_subset)
  slices <- build_exclusive_slices(masks)

  pop_raster <- get_pop_raster(
    source,
    pop_year,
    cache_dir,
    pop_iso = pop_iso,
    extent_geom = search_geom,
    use_land_mask = use_land_mask,
    allow_empty_overlap = TRUE
  )

  if (is.null(pop_raster)) {
    return(tibble::tibble(
      iso3 = iso,
      adm_level = adm_level,
      adm_name = region$shapeName,
      adm_id = region$shapeID,
      source = source,
      scenario = scenario,
      pop_year = pop_year,
      count_strict = unname(masks$counts[["strict"]]),
      count_nonstrict = unname(masks$counts[["nonStrict"]]),
      count_unknowncat = unname(masks$counts[["unknownCat"]]),
      pop_total = 0,
      pop_strict = 0,
      pop_strict10 = 0,
      pop_nonstrict = 0,
      pop_nonstrict10 = 0,
      pop_unknowncat = 0,
      pop_unknowncat10 = 0,
      area_strict = 0,
      area_strict10 = 0,
      area_nonstrict = 0,
      area_nonstrict10 = 0,
      area_unknowncat = 0,
      area_unknowncat10 = 0
    ))
  }

  area_raster <- terra::cellSize(pop_raster, unit = "km")

  region_pop_total <- exact_sum(pop_raster, sf::st_geometry(sf::st_transform(region_proj, terra::crs(pop_raster))))

  clip_and_sum <- function(geom, raster) {
    clipped <- geom_intersection_safe(geom, sf::st_geometry(region_proj))
    exact_sum(raster, clipped)
  }

  tibble::tibble(
    iso3 = iso,
    adm_level = adm_level,
    adm_name = region$shapeName,
    adm_id = region$shapeID,
    source = source,
    scenario = scenario,
    pop_year = pop_year,
    count_strict = unname(masks$counts[["strict"]]),
    count_nonstrict = unname(masks$counts[["nonStrict"]]),
    count_unknowncat = unname(masks$counts[["unknownCat"]]),
    pop_total = region_pop_total,
    pop_strict = clip_and_sum(slices$strict, pop_raster),
    pop_strict10 = clip_and_sum(slices$strict10, pop_raster),
    pop_nonstrict = clip_and_sum(slices$nonStrict, pop_raster),
    pop_nonstrict10 = clip_and_sum(slices$nonStrict10, pop_raster),
    pop_unknowncat = clip_and_sum(slices$unknownCat, pop_raster),
    pop_unknowncat10 = clip_and_sum(slices$unknownCat10, pop_raster),
    area_strict = clip_and_sum(slices$strict, area_raster),
    area_strict10 = clip_and_sum(slices$strict10, area_raster),
    area_nonstrict = clip_and_sum(slices$nonStrict, area_raster),
    area_nonstrict10 = clip_and_sum(slices$nonStrict10, area_raster),
    area_unknowncat = clip_and_sum(slices$unknownCat, area_raster),
    area_unknowncat10 = clip_and_sum(slices$unknownCat10, area_raster)
  )
}

read_reviewed_output <- function(iso, source, reviewed_dir) {
  readr::read_csv(
    file.path(reviewed_dir, sprintf("PA_Pop_%s_%s.csv", iso, source)),
    show_col_types = FALSE
  )
}

compute_country_source_original <- function(iso, source_name, cache_dir, wdpa_dir, wdpa_oct_zip, wdpa_oct_extract_dir, use_land_mask = TRUE) {
  boundary_info <- load_boundary_units(iso, cache_dir)
  wdpa_info <- load_wdpa_country(iso, wdpa_dir, wdpa_oct_zip, wdpa_oct_extract_dir)

  purrr::map_dfr(seq_len(nrow(boundary_info$regions)), function(i) {
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
}

compute_country_source_fix <- function(iso, source_name, cache_dir, wdpa_dir, wdpa_oct_zip, wdpa_oct_extract_dir, use_land_mask = TRUE) {
  boundary_info <- load_boundary_units(iso, cache_dir)
  wdpa_info <- load_wdpa_country(iso, wdpa_dir, wdpa_oct_zip, wdpa_oct_extract_dir)

  purrr::map_dfr(seq_len(nrow(boundary_info$regions)), function(i) {
    region <- boundary_info$regions[i, ]
    compute_region_result(
      region = region,
      iso = iso,
      adm_level = boundary_info$adm_level,
      source = source_name,
      scenario = "All_2020_Fixed",
      pop_year = 2020,
      wdpa_country = wdpa_info$data,
      cache_dir = cache_dir,
      pop_iso = boundary_info$boundary_iso,
      use_land_mask = use_land_mask
    )
  })
}

write_csv_atomic <- function(df, output_path) {
  ensure_dir(dirname(output_path))
  tmp_path <- paste0(output_path, ".tmp")
  readr::write_csv(df, tmp_path, na = "")
  ok <- file.rename(tmp_path, output_path)
  if (!ok) {
    stop("Failed to move temporary output into place: ", output_path)
  }
  invisible(output_path)
}

cleanup_task_cache <- function(cache_dir, pop_iso) {
  wp_root <- file.path(cache_dir, "worldpop")
  if (dir.exists(wp_root)) {
    unlink(file.path(wp_root, "2000", pop_iso), recursive = TRUE, force = TRUE)
    unlink(file.path(wp_root, "2020", pop_iso), recursive = TRUE, force = TRUE)
  }
}

run_country_source_pair <- function(
  iso,
  source_name,
  original_output_path,
  fix_output_path,
  cache_dir = "data/cache_direct",
  wdpa_dir = "data/WDPA_2021_05_GEE",
  wdpa_oct_zip = "/tmp/WDPA_2021.zip",
  wdpa_oct_extract_dir = "data/cache_wdpa_oct2021",
  use_land_mask = TRUE,
  cleanup_worldpop_cache = TRUE
) {
# This is the smallest complete local run for one country and one source.
  boundary_info <- load_boundary_units(iso, cache_dir)
  pop_iso <- boundary_info$boundary_iso

  if (!file.exists(original_output_path)) {
    original_df <- compute_country_source_original(
      iso = iso,
      source_name = source_name,
      cache_dir = cache_dir,
      wdpa_dir = wdpa_dir,
      wdpa_oct_zip = wdpa_oct_zip,
      wdpa_oct_extract_dir = wdpa_oct_extract_dir,
      use_land_mask = use_land_mask
    )
    write_csv_atomic(original_df, original_output_path)
  }

  if (!file.exists(fix_output_path)) {
    fix_df <- compute_country_source_fix(
      iso = iso,
      source_name = source_name,
      cache_dir = cache_dir,
      wdpa_dir = wdpa_dir,
      wdpa_oct_zip = wdpa_oct_zip,
      wdpa_oct_extract_dir = wdpa_oct_extract_dir,
      use_land_mask = use_land_mask
    )
    write_csv_atomic(fix_df, fix_output_path)
  }

  if (cleanup_worldpop_cache && source_name == "WP") {
    cleanup_task_cache(cache_dir, pop_iso)
  }

  invisible(list(original = original_output_path, fix = fix_output_path))
}