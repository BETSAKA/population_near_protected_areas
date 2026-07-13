suppressPackageStartupMessages({
  library(dplyr)
  library(exactextractr)
  library(purrr)
  library(readr)
  library(sf)
  library(stringr)
  library(terra)
  library(tibble)
})

# This script is meant to be launched from the RStudio Source pane.
# Comment out ISO3 codes below to run only a subset of countries.
# Set run_on_source to FALSE if you only want to load the functions.

sf::sf_use_s2(FALSE)

# Core spatial settings shared by all geometry operations.
buffer_meters <- 10000
work_crs <- 6933
world_cover_tile_root <- "/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/v100/2020/map"
empty_geojson <- "{\"type\":\"MultiPoint\",\"coordinates\":[]}"

# These S3 prefixes store local caches that are too large or too volatile for git.
default_s3_raster_prefix <- "s3://projet-betsaka/diffusion/population_pas/rasters"
default_s3_wdpa_spatial_prefix <- "s3://projet-betsaka/diffusion/population_pas/wdpa_as_gee"
default_s3_output_prefix <- "s3://projet-betsaka/diffusion/population_pas/reviewed_PA_Pop_GHSL_Worldpop"

# These folders define the default local layout used by the reproduction.
default_output_dir <- "data/reviewed_PA_Pop_new"
default_national_output_dir <- file.path(default_output_dir, "national_totals")
default_raster_cache_dir <- "data/cache_population_pas/rasters"
default_wdpa_dir <- "data/WDPA_2021_05_GEE"
default_wdpa_spatial_cache_dir <- "data/cache_population_pas/wdpa_as_gee"
default_diagnostic_dir <- file.path("tests", "review2_profile")
default_progress_dir <- file.path(
  "results",
  "reproduction_runs",
  "reviewed_refactor"
)
default_diagnostic_s3_prefix <- paste0(
  default_s3_output_prefix,
  "/diagnostics"
)

# This is the full reviewed country list used by the GEE national aggregation scripts.
reviewed_iso3 <- c(
  "AFG",
  "AGO",
  "BGD",
  "BEN",
  "BTN",
  "BOL",
  "BFA",
  "BDI",
  "CPV",
  "KHM",
  "CMR",
  "CAF",
  "TCD",
  "COM",
  "COD",
  "COG",
  "CIV",
  "DJI",
  "EGY",
  "SLV",
  "ERI",
  "SWZ",
  "ETH",
  "GMB",
  "GHA",
  "GIN",
  "GNB",
  "HTI",
  "HND",
  "IND",
  "IDN",
  "KEN",
  "KIR",
  "PRK",
  "KGZ",
  "LAO",
  "LSO",
  "LBR",
  "MDG",
  "MWI",
  "MLI",
  "MRT",
  "FSM",
  "MDA",
  "MNG",
  "MAR",
  "MOZ",
  "MMR",
  "NPL",
  "NIC",
  "NER",
  "NGA",
  "PAK",
  "PNG",
  "PHL",
  "RWA",
  "STP",
  "SEN",
  "SLE",
  "SLB",
  "SOM",
  "SSD",
  "SDN",
  "SYR",
  "TJK",
  "TZA",
  "TLS",
  "TGO",
  "TUN",
  "UGA",
  "UKR",
  "UZB",
  "VUT",
  "VNM",
  "118",
  "129",
  "YEM",
  "ZMB",
  "ZWE"
)

# This is the list that will run when you click Source in RStudio.
# Comment out countries here to work on a smaller subset.
run_iso3 <- c(
  # "AFG",
  # "AGO",
  # "BGD",
  # "BEN",
  # "BTN",
  # "BOL",
  # "BFA",
  # "BDI",
  # "CPV",
  # "KHM",
  # "CMR",
  # "CAF",
  # "TCD",
  # "COM",
  # "COD",
  # "COG",
  # "CIV",
  # "DJI",
  # "EGY",
  # "SLV",
  # "ERI",
  # "SWZ",
  # "ETH",
  # "GMB",
  # "GHA",
  # "GIN",
  # "GNB",
  # "HTI",
  # "HND",
  # "IND",
  # "IDN",
  # "KEN",
  # "KIR",
  # "PRK",
  # "KGZ",
  # "LAO",
  # "LSO",
  # "LBR",
  "MDG"#,
  # "MWI",
  # "MLI",
  # "MRT",
  # "FSM",
  # "MDA",
  # "MNG",
  # "MAR",
  # "MOZ",
  # "MMR",
  # "NPL",
  # "NIC",
  # "NER",
  # "NGA",
  # "PAK",
  # "PNG",
  # "PHL",
  # "RWA",
  # "STP",
  # "SEN",
  # "SLE",
  # "SLB",
  # "SOM",
  # "SSD",
  # "SDN",
  # "SYR",
  # "TJK",
  # "TZA",
  # "TLS",
  # "TGO",
  # "TUN",
  # "UGA",
  # "UKR",
  # "UZB",
  # "VUT",
  # "VNM",
  # "118",
  # "129",
  # "YEM",
  # "ZMB",
  # "ZWE"
)

run_sources <- c("GHSL", "WP")
run_overwrite <- FALSE
run_on_source <- TRUE
run_national_only <- FALSE

# These are the scenario and population-year pairs. All_2020 has been added
# (see refactor note 1 above); it is now computed directly, not derived by
# summing Confirmed_2020 + Unknown_Year downstream.
scenarios_reviewed <- tribble(
  ~scenario        , ~pop_year ,
  "Confirmed_2000" ,      2000 ,
  "Confirmed_2020" ,      2020 ,
  "Unknown_Year"   ,      2020 ,
  "All_2020"       ,      2020
)

# The hierarchy matches the reviewed GEE script: strict first, then non-strict, then unknown.
strict_iucn <- c("Ia", "Ib", "II", "III")
nonstrict_iucn <- c("IV", "V", "VI")
special_boundary_names <- c("118" = "Gaza", "129" = "West Bank")

wdpa_lookup_iso <- function(iso) {
  if (iso %in% names(special_boundary_names)) {
    return("PSE")
  }

  iso
}

new_reproduction_config <- function(
  iso3 = run_iso3,
  sources = c("GHSL", "WP"),
  output_dir = default_output_dir,
  national_output_dir = file.path(output_dir, "national_totals"),
  raster_cache_dir = default_raster_cache_dir,
  wdpa_dir = default_wdpa_dir,
  wdpa_spatial_cache_dir = default_wdpa_spatial_cache_dir,
  diagnostic_dir = default_diagnostic_dir,
  progress_dir = default_progress_dir,
  s3_raster_prefix = default_s3_raster_prefix,
  s3_wdpa_spatial_prefix = default_s3_wdpa_spatial_prefix,
  s3_output_prefix = default_s3_output_prefix,
  diagnostic_s3_prefix = default_diagnostic_s3_prefix,
  use_land_mask = TRUE,
  overwrite = FALSE,
  diagnostic_enabled = TRUE,
  sync_raster_cache_on_startup = FALSE,
  sync_wdpa_spatial_cache_on_startup = TRUE,
  national_only = run_national_only
) {
  list(
    iso3 = iso3,
    sources = sources,
    output_dir = output_dir,
    national_output_dir = national_output_dir,
    raster_cache_dir = raster_cache_dir,
    wdpa_dir = wdpa_dir,
    wdpa_spatial_cache_dir = wdpa_spatial_cache_dir,
    diagnostic_dir = diagnostic_dir,
    progress_dir = progress_dir,
    s3_raster_prefix = s3_raster_prefix,
    s3_wdpa_spatial_prefix = s3_wdpa_spatial_prefix,
    s3_output_prefix = s3_output_prefix,
    diagnostic_s3_prefix = diagnostic_s3_prefix,
    use_land_mask = use_land_mask,
    overwrite = overwrite,
    diagnostic_enabled = diagnostic_enabled,
    sync_raster_cache_on_startup = sync_raster_cache_on_startup,
    sync_wdpa_spatial_cache_on_startup = sync_wdpa_spatial_cache_on_startup,
    national_only = national_only
  )
}

default_config <- new_reproduction_config()

# This normalizes derived paths after the caller overrides output_dir or cache folders.
normalize_config <- function(config) {
  defaults <- new_reproduction_config()
  config <- utils::modifyList(defaults, config)

  if (
    is.null(config$national_output_dir) || !nzchar(config$national_output_dir)
  ) {
    config$national_output_dir <- file.path(
      config$output_dir,
      "national_totals"
    )
  }

  config
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

run_cmd <- function(args) {
  out <- system2(args[[1]], args[-1], stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")

  if (!is.null(status) && status != 0) {
    stop(paste(c(args, out), collapse = "\n"), call. = FALSE)
  }

  invisible(out)
}

write_csv_atomic <- function(df, output_path, na = "") {
  ensure_dir(dirname(output_path))
  tmp_path <- paste0(output_path, ".tmp")
  readr::write_csv(df, tmp_path, na = na)

  if (!file.rename(tmp_path, output_path)) {
    stop(
      "Failed to move temporary file into place: ",
      output_path,
      call. = FALSE
    )
  }

  invisible(output_path)
}

append_delim_rows <- function(df, output_path, sep = ",", na = "") {
  ensure_dir(dirname(output_path))
  utils::write.table(
    df,
    file = output_path,
    sep = sep,
    row.names = FALSE,
    col.names = !file.exists(output_path),
    append = file.exists(output_path),
    quote = TRUE,
    na = na,
    qmethod = "double"
  )

  invisible(output_path)
}

append_csv_rows <- function(df, output_path, na = "") {
  append_delim_rows(df, output_path, sep = ",", na = na)
}

append_tsv_rows <- function(df, output_path, na = "") {
  append_delim_rows(df, output_path, sep = "\t", na = na)
}

upsert_csv_row <- function(row, output_path, key_cols, col_types = NULL) {
  existing <- if (file.exists(output_path)) {
    readr::read_csv(output_path, show_col_types = FALSE, col_types = col_types)
  } else {
    tibble()
  }

  if (nrow(existing) > 0) {
    key_data <- row |>
      select(all_of(key_cols))
    existing <- anti_join(existing, key_data, by = key_cols)
  }

  updated <- bind_rows(existing, row)
  write_csv_atomic(updated, output_path)
  invisible(updated)
}

trim_trailing_slash <- function(path) {
  sub("/+$", "", path)
}

s3_join <- function(prefix, ...) {
  paste(c(trim_trailing_slash(prefix), ...), collapse = "/")
}

legacy_s3_raster_prefix <- function(s3_prefix) {
  sub("/rasters$", "", trim_trailing_slash(s3_prefix))
}

preferred_s3_raster_prefix <- function(s3_prefix) {
  trim_trailing_slash(s3_prefix)
}

upload_local_file_to_s3 <- function(local_path, s3_path) {
  if (!file.exists(local_path) || !nzchar(Sys.which("aws"))) {
    return(invisible(FALSE))
  }

  uploaded <- tryCatch(
    {
      run_cmd(c("aws", "s3", "cp", local_path, s3_path, "--only-show-errors"))
      TRUE
    },
    error = function(e) FALSE
  )

  invisible(uploaded)
}

upload_output_if_needed <- function(local_path, output_root, s3_output_prefix) {
  if (!nzchar(s3_output_prefix) || !file.exists(local_path)) {
    return(invisible(FALSE))
  }

  rel_path <- sub(
    paste0(
      "^",
      normalizePath(output_root, winslash = "/", mustWork = TRUE),
      "/?"
    ),
    "",
    normalizePath(local_path, winslash = "/", mustWork = TRUE)
  )

  if (!nzchar(rel_path) || identical(rel_path, local_path)) {
    return(invisible(FALSE))
  }

  upload_local_file_to_s3(local_path, s3_join(s3_output_prefix, rel_path))
}

read_proc_status_value <- function(status_lines, field) {
  line <- status_lines[str_detect(status_lines, paste0("^", field, ":"))][1]

  if (is.na(line)) {
    return(NA_real_)
  }

  value <- str_match(line, "^\\w+:\\s+([0-9]+)")[, 2]
  suppressWarnings(as.numeric(value))
}

capture_disk_free_kb <- function(path = ".") {
  df_out <- tryCatch(
    system2("df", c("-Pk", path), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )

  if (length(df_out) < 2) {
    return(NA_real_)
  }

  fields <- str_split(str_squish(df_out[2]), "\\s+")[[1]]
  if (length(fields) < 4) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(fields[4]))
}

capture_process_metrics <- function(path = ".") {
  status_lines <- tryCatch(readLines("/proc/self/status"), error = function(e) character())
  gc_stats <- gc(verbose = FALSE)
  used_mb <- if ("used" %in% colnames(gc_stats)) sum(gc_stats[, "used"]) else NA_real_

  list(
    pid = Sys.getpid(),
    rss_kb = read_proc_status_value(status_lines, "VmRSS"),
    hwm_kb = read_proc_status_value(status_lines, "VmHWM"),
    vm_size_kb = read_proc_status_value(status_lines, "VmSize"),
    gc_used_mb = used_mb,
    disk_free_kb = capture_disk_free_kb(path)
  )
}

new_diagnostic_context <- function(config, iso, source, step, output_path = "") {
  source_label <- if (is.null(source) || !nzchar(source)) "ALL" else source
  root_dir <- file.path(config$diagnostic_dir, iso, source_label, step)

  ensure_dir(root_dir)

  list(
    enabled = isTRUE(config$diagnostic_enabled),
    iso3 = iso,
    source = source_label,
    step = step,
    root_dir = root_dir,
    events_path = file.path(root_dir, "events.tsv"),
    latest_path = file.path(root_dir, "latest_status.csv"),
    partial_output_path = file.path(root_dir, "partial_output.csv"),
    output_path = output_path,
    diagnostic_root = config$diagnostic_dir,
    s3_prefix = config$diagnostic_s3_prefix
  )
}

sync_diagnostic_file <- function(context, local_path) {
  if (is.null(context) || !isTRUE(context$enabled)) {
    return(invisible(FALSE))
  }

  upload_output_if_needed(local_path, context$diagnostic_root, context$s3_prefix)
}

record_diagnostic_event <- function(
  context,
  stage,
  status,
  details = "",
  region_index = NA_integer_,
  region_total = NA_integer_,
  region_id = NA_character_,
  region_name = NA_character_,
  scenario = NA_character_,
  pop_year = NA_integer_,
  wdpa_rows = NA_integer_,
  raster_cells = NA_real_,
  elapsed_seconds = NA_real_,
  output_path = NA_character_
) {
  if (is.null(context) || !isTRUE(context$enabled)) {
    return(invisible(NULL))
  }

  metrics <- capture_process_metrics(context$root_dir)
  event <- tibble(
    event_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    pid = metrics$pid,
    iso3 = context$iso3,
    source = context$source,
    step = context$step,
    stage = stage,
    status = status,
    region_index = region_index,
    region_total = region_total,
    region_id = region_id,
    region_name = region_name,
    scenario = scenario,
    pop_year = pop_year,
    wdpa_rows = wdpa_rows,
    raster_cells = raster_cells,
    elapsed_seconds = elapsed_seconds,
    rss_kb = metrics$rss_kb,
    hwm_kb = metrics$hwm_kb,
    vm_size_kb = metrics$vm_size_kb,
    gc_used_mb = metrics$gc_used_mb,
    disk_free_kb = metrics$disk_free_kb,
    output_path = dplyr::coalesce(output_path, context$output_path),
    details = details
  )

  append_tsv_rows(event, context$events_path)
  write_csv_atomic(event, context$latest_path)
  sync_diagnostic_file(context, context$events_path)
  sync_diagnostic_file(context, context$latest_path)

  invisible(event)
}

append_partial_output <- function(rows, context) {
  if (is.null(context) || !isTRUE(context$enabled) || nrow(rows) == 0) {
    return(invisible(NULL))
  }

  append_csv_rows(rows, context$partial_output_path)
  sync_diagnostic_file(context, context$partial_output_path)
  invisible(rows)
}

copy_first_available_s3_object <- function(local_path, s3_paths) {
  if (file.exists(local_path) || !nzchar(Sys.which("aws"))) {
    return(file.exists(local_path))
  }

  ensure_dir(dirname(local_path))

  for (s3_path in unique(s3_paths)) {
    copied <- tryCatch(
      {
        run_cmd(c("aws", "s3", "cp", s3_path, local_path, "--only-show-errors"))
        TRUE
      },
      error = function(e) FALSE
    )

    if (copied && file.exists(local_path)) {
      return(TRUE)
    }
  }

  FALSE
}

# This syncs a whole cache only when the local folder is still empty.
# It keeps startup cheap on reruns while still supporting fresh pods.
sync_directory_from_s3 <- function(local_dir, s3_prefix, label) {
  ensure_dir(local_dir)

  has_local_files <- length(list.files(
    local_dir,
    recursive = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )) >
    0
  if (has_local_files || !nzchar(s3_prefix) || !nzchar(Sys.which("aws"))) {
    return(invisible(FALSE))
  }

  message("Syncing ", label, " from ", s3_prefix)

  tryCatch(
    {
      run_cmd(c(
        "aws",
        "s3",
        "sync",
        trim_trailing_slash(s3_prefix),
        local_dir,
        "--no-progress",
        "--only-show-errors"
      ))
      TRUE
    },
    error = function(e) {
      warning(
        "Startup sync failed for ",
        label,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
      FALSE
    }
  )
}

# This copies one object when we only need a single missing file.
ensure_s3_object_local <- function(local_path, s3_path) {
  if (file.exists(local_path)) {
    return(local_path)
  }
  if (!nzchar(Sys.which("aws"))) {
    return(NULL)
  }

  ensure_dir(dirname(local_path))

  copied <- tryCatch(
    {
      run_cmd(c("aws", "s3", "cp", s3_path, local_path, "--only-show-errors"))
      TRUE
    },
    error = function(e) FALSE
  )

  if (!copied || !file.exists(local_path)) {
    return(NULL)
  }

  local_path
}

# Raster download helpers ----------------------------------------------------

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

# This prefers the S3 cache and only falls back to public download when needed.
ensure_worldpop_file <- function(year, iso, raster_cache_dir, s3_prefix) {
  file_name <- sprintf("%s_ppp_%s.tif", tolower(iso), year)
  dst <- file.path(
    raster_cache_dir,
    "worldpop",
    as.character(year),
    iso,
    file_name
  )

  if (file.exists(dst)) {
    return(dst)
  }

  ensure_dir(dirname(dst))

  copied <- FALSE
  if (nzchar(s3_prefix)) {
    preferred_prefix <- preferred_s3_raster_prefix(s3_prefix)
    legacy_prefix <- legacy_s3_raster_prefix(s3_prefix)
    copied <- copy_first_available_s3_object(
      dst,
      c(
        sprintf("%s/worldpop/%s/%s/%s", preferred_prefix, year, iso, file_name),
        sprintf("%s/worldpop/%s/%s/%s", legacy_prefix, year, iso, file_name)
      )
    )
  }

  if (!copied) {
    options(timeout = max(getOption("timeout"), 3600))
    download.file(
      worldpop_public_url(year, iso),
      dst,
      mode = "wb",
      quiet = FALSE
    )

    if (nzchar(s3_prefix)) {
      upload_local_file_to_s3(
        dst,
        sprintf(
          "%s/worldpop/%s/%s/%s",
          preferred_s3_raster_prefix(s3_prefix),
          year,
          iso,
          file_name
        )
      )
    }
  }

  dst
}

# GHSL is global, so we cache a single tif per year and resolution.
ensure_ghsl_file <- function(
  year,
  raster_cache_dir,
  s3_prefix,
  resolution = "3ss"
) {
  tif_name <- sprintf(
    "GHS_POP_E%s_GLOBE_R2023A_4326_%s_V1_0.tif",
    year,
    resolution
  )
  tif_path <- file.path(raster_cache_dir, "ghsl", resolution, tif_name)
  zip_name <- sprintf(
    "GHS_POP_E%s_GLOBE_R2023A_4326_%s_V1_0.zip",
    year,
    resolution
  )
  zip_path <- file.path(raster_cache_dir, "ghsl", resolution, zip_name)

  if (file.exists(tif_path)) {
    return(tif_path)
  }

  ensure_dir(dirname(tif_path))

  copied <- FALSE
  if (nzchar(s3_prefix)) {
    preferred_prefix <- preferred_s3_raster_prefix(s3_prefix)
    legacy_prefix <- legacy_s3_raster_prefix(s3_prefix)

    copied <- copy_first_available_s3_object(
      tif_path,
      c(
        sprintf("%s/ghsl/%s/%s", preferred_prefix, resolution, tif_name),
        sprintf("%s/ghsl/%s", legacy_prefix, tif_name)
      )
    )

    if (!copied) {
      copied <- copy_first_available_s3_object(
        zip_path,
        c(
          sprintf("%s/ghsl/%s/%s", preferred_prefix, resolution, zip_name),
          sprintf("%s/ghsl/%s", preferred_prefix, zip_name),
          sprintf("%s/ghsl/%s", legacy_prefix, zip_name)
        )
      )

      if (copied && file.exists(zip_path)) {
        utils::unzip(
          zip_path,
          files = tif_name,
          exdir = dirname(tif_path),
          overwrite = FALSE
        )
      }
    }
  }

  if (!file.exists(tif_path)) {
    options(timeout = max(getOption("timeout"), 3600))
    download.file(
      ghsl_public_url(year, resolution),
      zip_path,
      mode = "wb",
      quiet = FALSE
    )
    utils::unzip(
      zip_path,
      files = tif_name,
      exdir = dirname(tif_path),
      overwrite = FALSE
    )

    if (nzchar(s3_prefix)) {
      upload_local_file_to_s3(
        zip_path,
        sprintf(
          "%s/ghsl/%s/%s",
          preferred_s3_raster_prefix(s3_prefix),
          resolution,
          zip_name
        )
      )
      upload_local_file_to_s3(
        tif_path,
        sprintf(
          "%s/ghsl/%s/%s",
          preferred_s3_raster_prefix(s3_prefix),
          resolution,
          tif_name
        )
      )
    }
  }

  if (file.exists(zip_path)) {
    unlink(zip_path)
  }

  tif_path
}

get_pop_raster_path <- function(
  source,
  year,
  raster_cache_dir,
  pop_iso,
  s3_prefix
) {
  if (source == "WP") {
    return(ensure_worldpop_file(year, pop_iso, raster_cache_dir, s3_prefix))
  }
  if (source == "GHSL") {
    return(ensure_ghsl_file(year, raster_cache_dir, s3_prefix))
  }

  stop("Unknown population source: ", source, call. = FALSE)
}

# Geometry helpers -----------------------------------------------------------

geom_empty <- function(crs = work_crs) {
  st_sfc(st_multipolygon(), crs = crs)
}

geom_is_empty <- function(x) {
  length(x) == 0 || all(st_is_empty(x))
}

# These wrappers keep downstream geometry calls predictable even after invalid unions.
geom_make_valid <- function(x, crs = NULL) {
  if (inherits(x, "sf")) {
    x <- st_geometry(x)
  }

  if (geom_is_empty(x)) {
    return(geom_empty(if (is.null(crs)) st_crs(x) else crs))
  }

  out <- st_make_valid(x)
  out <- st_collection_extract(out, "POLYGON", warn = FALSE)

  if (geom_is_empty(out)) {
    return(geom_empty(if (is.null(crs)) st_crs(x) else crs))
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

  geom_make_valid(st_union(x, y), st_crs(x))
}

geom_difference_safe <- function(x, y) {
  if (geom_is_empty(x) || geom_is_empty(y)) {
    return(x)
  }

  out <- suppressWarnings(st_difference(x, y))
  geom_make_valid(out, st_crs(x))
}

geom_intersection_safe <- function(x, y) {
  if (geom_is_empty(x) || geom_is_empty(y)) {
    return(geom_empty(st_crs(x)))
  }

  out <- suppressWarnings(st_intersection(x, y))
  geom_make_valid(out, st_crs(x))
}

geom_buffer_safe <- function(x, dist) {
  if (geom_is_empty(x)) {
    return(x)
  }

  geom_make_valid(st_buffer(x, dist = dist), st_crs(x))
}

make_geom_sf <- function(geom) {
  if (geom_is_empty(geom)) {
    return(st_sf(id = integer(), geometry = geom))
  }

  geom_one <- if (length(geom) > 1) {
    geom_make_valid(st_union(geom), st_crs(geom))
  } else {
    geom
  }
  st_sf(id = 1L, geometry = geom_one)
}

# WDPA filters and boundary loading -----------------------------------------

# These filters reproduce the reviewed GEE WDPA screening.
filter_wdpa_base <- function(wdpa) {
  wdpa |>
    filter(STATUS %in% c("Designated", "Established", "Inscribed")) |>
    filter(DESIG_ENG != "UNESCO-MAB Biosphere Reserve") |>
    filter(MARINE != "2")
}

# These year rules reproduce the four reviewed scenarios. All_2020 mirrors the
# GEE script's explicit OR-filter rather than a simplified STATUS_YR <= 2020,
# so it stays correct even if STATUS_YR ever contains an unexpected negative
# sentinel value in some future WDPA release.
scenario_filter_mask <- function(wdpa, scenario) {
  if (scenario == "Confirmed_2000") {
    return(filter(wdpa, STATUS_YR > 0, STATUS_YR <= 2000))
  }
  if (scenario == "Confirmed_2020") {
    return(filter(wdpa, STATUS_YR > 0, STATUS_YR <= 2020))
  }
  if (scenario == "Unknown_Year") {
    return(filter(wdpa, STATUS_YR == 0))
  }
  if (scenario == "All_2020") {
    return(filter(wdpa, (STATUS_YR > 0 & STATUS_YR <= 2020) | STATUS_YR == 0))
  }

  stop("Unknown scenario: ", scenario, call. = FALSE)
}

# Gaza and West Bank are represented as separate study units but share the PSE boundary source.
load_boundary_units <- function(iso) {
  if (iso %in% names(special_boundary_names)) {
    url <- "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/PSE/ADM1/geoBoundaries-PSE-ADM1.geojson"
    regions <- read_sf(url, quiet = TRUE) |>
      filter(shapeName == special_boundary_names[[iso]]) |>
      mutate(shapeGroup = iso)

    return(list(regions = regions, adm_level = 1L, boundary_iso = "PSE"))
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

  adm1 <- tryCatch(read_sf(adm1_url, quiet = TRUE), error = function(e) NULL)
  if (!is.null(adm1) && nrow(adm1) > 0) {
    return(list(regions = adm1, adm_level = 1L, boundary_iso = iso))
  }

  adm0 <- tryCatch(read_sf(adm0_url, quiet = TRUE), error = function(e) NULL)
  if (is.null(adm0) || nrow(adm0) == 0) {
    stop(
      "No geoBoundaries ADM0 or ADM1 geometry found for ",
      iso,
      call. = FALSE
    )
  }

  list(regions = adm0, adm_level = 0L, boundary_iso = iso)
}

local_wdpa_shapefile_iso3 <- function(wdpa_dir) {
  if (!dir.exists(wdpa_dir)) {
    return(character())
  }

  list.files(wdpa_dir, pattern = "^WDPA_202105_.*\\.shp$") |>
    str_remove("^WDPA_202105_") |>
    str_remove("\\.shp$") |>
    sort()
}

local_wdpa_spatial_iso3 <- function(wdpa_spatial_cache_dir) {
  if (!dir.exists(wdpa_spatial_cache_dir)) {
    return(character())
  }

  list.files(wdpa_spatial_cache_dir, pattern = "^WDPA_202105_.*\\.geojson$") |>
    str_remove("^WDPA_202105_") |>
    str_remove("\\.geojson$") |>
    sort()
}

# The local May 2021 shapefiles stay the primary source because they match the
# reviewed exports for most countries. The spatial GeoJSON cache is a fallback
# for countries that are missing from the ISO-based local directory.
load_wdpa_country <- function(iso, config) {
  wdpa_iso <- wdpa_lookup_iso(iso)
  wdpa_path <- file.path(
    config$wdpa_dir,
    sprintf("WDPA_202105_%s.shp", wdpa_iso)
  )
  if (file.exists(wdpa_path)) {
    return(list(
      data = read_sf(wdpa_path, quiet = TRUE),
      source = "wdpa_202105_local_iso"
    ))
  }

  spatial_file <- file.path(
    config$wdpa_spatial_cache_dir,
    sprintf("WDPA_202105_%s.geojson", wdpa_iso)
  )

  if (!file.exists(spatial_file) && nzchar(config$s3_wdpa_spatial_prefix)) {
    s3_path <- sprintf(
      "%s/WDPA_202105_%s.geojson",
      trim_trailing_slash(config$s3_wdpa_spatial_prefix),
      wdpa_iso
    )
    ensure_s3_object_local(spatial_file, s3_path)
  }

  if (file.exists(spatial_file)) {
    spatial_wdpa <- read_sf(spatial_file, quiet = TRUE)

    if (nrow(spatial_wdpa) == 0 && iso %in% names(special_boundary_names)) {
      warning(
        "The spatial WDPA fallback for ",
        iso,
        " (resolved to ",
        wdpa_iso,
        ")",
        " is empty, but the reviewed ADM exports in this repository show non-zero PA exposure for this Palestine subunit. ",
        "Current local assets are not enough to reproduce that case faithfully.",
        call. = FALSE
      )
    }

    return(list(data = spatial_wdpa, source = "wdpa_202105_spatial_geojson"))
  }

  stop("No WDPA source available for ", iso, call. = FALSE)
}

# Raster masking and extraction ---------------------------------------------

format_worldcover_axis <- function(value, axis = c("lat", "lon")) {
  axis <- match.arg(axis)
  prefix <- if (axis == "lat") {
    ifelse(value >= 0, "N", "S")
  } else {
    ifelse(value >= 0, "E", "W")
  }
  width <- if (axis == "lat") 2 else 3
  paste0(prefix, sprintf(paste0("%0", width, "d"), abs(as.integer(value))))
}

worldcover_tile_urls <- function(extent_geom) {
  bbox <- st_bbox(st_transform(extent_geom, 4326))

  lon_seq <- seq(
    floor(bbox[["xmin"]] / 3) * 3,
    floor((bbox[["xmax"]] - 1e-9) / 3) * 3,
    by = 3
  )
  lat_seq <- seq(
    floor(bbox[["ymin"]] / 3) * 3,
    floor((bbox[["ymax"]] - 1e-9) / 3) * 3,
    by = 3
  )

  expand.grid(
    lat = lat_seq,
    lon = lon_seq,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    mutate(
      lat_code = vapply(
        lat,
        format_worldcover_axis,
        character(1),
        axis = "lat"
      ),
      lon_code = vapply(
        lon,
        format_worldcover_axis,
        character(1),
        axis = "lon"
      ),
      url = sprintf(
        "%s/ESA_WorldCover_10m_2020_v100_%s%s_Map.tif",
        world_cover_tile_root,
        lat_code,
        lon_code
      )
    ) |>
    pull(url)
}

read_worldcover_tiles <- function(extent_geom) {
  crop_ext <- terra::ext(st_bbox(st_transform(extent_geom, 4326)))
  rasters <- lapply(worldcover_tile_urls(extent_geom), function(url) {
    tryCatch(terra::crop(terra::rast(url), crop_ext), error = function(e) NULL)
  })
  rasters <- Filter(Negate(is.null), rasters)

  if (length(rasters) == 0) {
    stop("No readable WorldCover tiles found for extent", call. = FALSE)
  }
  if (length(rasters) == 1) {
    return(rasters[[1]])
  }

  do.call(terra::merge, rasters)
}

get_land_mask <- function(template_raster, extent_geom) {
  wc <- read_worldcover_tiles(extent_geom)
  ext_ll <- st_transform(extent_geom, 4326)
  wc <- terra::crop(wc, terra::ext(st_bbox(ext_ll)))
  land <- wc != 80
  terra::resample(land, template_raster, method = "near")
}

crop_extent_or_null <- function(raster, extent_geom) {
  ext_ll <- st_transform(extent_geom, 4326)
  target_ext <- terra::ext(st_bbox(ext_ll))
  overlap_ext <- tryCatch(
    terra::intersect(terra::ext(raster), target_ext),
    error = function(e) NULL
  )

  if (is.null(overlap_ext)) {
    return(NULL)
  }

  terra::crop(raster, overlap_ext)
}

# This clips the raster early so exact extraction only touches the country
# neighborhood we need. `extent_geom` must be the *buffered* search geometry
# (see region_extent_with_buffer / compute_region_all_scenarios below), not
# the bare ADM1 boundary, so that population in the 10km ring outside the
# ADM1 is available when later slices get clipped back to the ADM1.
get_pop_raster <- function(
  source,
  year,
  raster_cache_dir,
  pop_iso,
  extent_geom,
  use_land_mask,
  s3_prefix,
  allow_empty_overlap = FALSE
) {
  path <- get_pop_raster_path(
    source,
    year,
    raster_cache_dir,
    pop_iso,
    s3_prefix
  )
  raster <- terra::rast(path)

  if (source == "WP") {
    raster <- terra::clamp(raster, lower = -Inf, upper = 65534, values = FALSE)
  }

  raster <- crop_extent_or_null(raster, extent_geom)
  if (is.null(raster)) {
    if (allow_empty_overlap) {
      return(NULL)
    }
    stop("Population raster does not overlap requested extent", call. = FALSE)
  }

  if (!use_land_mask) {
    return(raster)
  }

  mask <- get_land_mask(raster, extent_geom)
  terra::mask(raster, mask, maskvalues = 0, updatevalue = NA)
}

# Category masks and extraction ---------------------------------------------

# This builds the exclusive PA hierarchy used throughout the reproduction.
# Called once per (region, scenario) on the WDPA subset already restricted to
# that scenario's year rule -- see compute_region_result_for_scenario().
build_category_masks <- function(wdpa_sf) {
  if (nrow(wdpa_sf) == 0) {
    empty <- geom_empty(work_crs)
    return(list(
      strict = empty,
      nonstrict = empty,
      unknown = empty,
      counts = c(strict = 0L, nonstrict = 0L, unknown = 0L, total = 0L)
    ))
  }

  wdpa_proj <- wdpa_sf |>
    st_transform(work_crs) |>
    mutate(geometry = st_make_valid(geometry))

  strict_fc <- wdpa_proj |> filter(IUCN_CAT %in% strict_iucn)
  nonstrict_fc <- wdpa_proj |> filter(IUCN_CAT %in% nonstrict_iucn)
  unknown_fc <- wdpa_proj |>
    filter(!IUCN_CAT %in% c(strict_iucn, nonstrict_iucn))

  strict_geom <- if (nrow(strict_fc) == 0) {
    geom_empty(work_crs)
  } else {
    geom_make_valid(st_union(strict_fc), work_crs)
  }
  nonstrict_geom <- if (nrow(nonstrict_fc) == 0) {
    geom_empty(work_crs)
  } else {
    geom_make_valid(st_union(nonstrict_fc), work_crs)
  }
  unknown_geom <- if (nrow(unknown_fc) == 0) {
    geom_empty(work_crs)
  } else {
    geom_make_valid(st_union(unknown_fc), work_crs)
  }

  nonstrict_final <- geom_difference_safe(nonstrict_geom, strict_geom)
  unknown_final <- geom_difference_safe(
    unknown_geom,
    geom_union_safe(strict_geom, nonstrict_final)
  )

  list(
    strict = strict_geom,
    nonstrict = nonstrict_final,
    unknown = unknown_final,
    counts = c(
      strict = nrow(strict_fc),
      nonstrict = nrow(nonstrict_fc),
      unknown = nrow(unknown_fc),
      total = nrow(wdpa_proj)
    )
  )
}

# This reproduces the no-double-counting rule between inside-PA and 10 km rings.
build_exclusive_slices <- function(category_masks, buffer_m = buffer_meters) {
  claimed <- geom_empty(work_crs)
  slices <- list()

  for (category_name in c("strict", "nonstrict", "unknown")) {
    inside_geom <- geom_difference_safe(
      category_masks[[category_name]],
      claimed
    )
    claimed <- geom_union_safe(claimed, inside_geom)

    buffer_geom <- geom_buffer_safe(category_masks[[category_name]], buffer_m)
    buffer_geom <- geom_difference_safe(buffer_geom, claimed)
    claimed <- geom_union_safe(claimed, buffer_geom)

    slices[[category_name]] <- inside_geom
    slices[[paste0(category_name, "10")]] <- buffer_geom
  }

  slices
}

exact_sum <- function(raster, geom) {
  if (is.null(raster) || geom_is_empty(geom)) {
    return(0)
  }

  geom_ll <- st_transform(make_geom_sf(geom), terra::crs(raster))
  values <- exactextractr::exact_extract(
    raster,
    geom_ll,
    "sum",
    progress = FALSE
  )

  if (length(values) == 0 || is.null(values)) {
    return(0)
  }

  values <- if (is.list(values)) {
    unlist(values, recursive = TRUE, use.names = FALSE)
  } else {
    as.vector(values)
  }
  values <- suppressWarnings(as.numeric(values))

  if (length(values) == 0 || all(is.na(values))) {
    return(0)
  }

  sum(values, na.rm = TRUE)
}

# Buffers the ADM1 unit outward by 10km. This is the geometry used to (a)
# pull in WDPA polygons from outside the ADM1 and (b) crop the population
# raster wide enough to cover the 10km ring. Everything downstream is later
# clipped back to the true (unbuffered) ADM1 boundary in clip_and_sum(), so
# the only effect of this buffer is to make sure nearby PAs and their ring
# population aren't missed just because the PA polygon itself sits outside
# this ADM1's own boundary.
region_extent_with_buffer <- function(region) {
  region |>
    st_transform(work_crs) |>
    st_buffer(buffer_meters) |>
    st_transform(4326)
}

clip_and_sum <- function(geom, region_geom, raster) {
  clipped <- geom_intersection_safe(geom, region_geom)
  exact_sum(raster, clipped)
}

scenario_index <- function(scenario) {
  match(scenario, scenarios_reviewed$scenario) - 1L
}

# This computes one reviewed-format row for one region/source/scenario,
# reusing a pop/area raster pair that was already loaded and cropped for
# this region and this scenario's pop_year (see compute_region_all_scenarios).
compute_region_result_for_scenario <- function(
  region,
  iso,
  adm_level,
  source,
  scenario,
  pop_year,
  region_geom,
  wdpa_spatial_subset,
  pop_raster,
  area_raster
) {
  wdpa_subset <- wdpa_spatial_subset |> scenario_filter_mask(scenario)

  masks <- build_category_masks(wdpa_subset)
  slices <- build_exclusive_slices(masks)

  tibble(
    `system:index` = paste0(region$shapeID, "_", scenario_index(scenario)),
    adm_id = region$shapeID,
    adm_level = adm_level,
    adm_name = region$shapeName,
    area_nonstrict = clip_and_sum(slices$nonstrict, region_geom, area_raster),
    area_nonstrict10 = clip_and_sum(
      slices$nonstrict10,
      region_geom,
      area_raster
    ),
    area_strict = clip_and_sum(slices$strict, region_geom, area_raster),
    area_strict10 = clip_and_sum(slices$strict10, region_geom, area_raster),
    area_unknowncat = clip_and_sum(slices$unknown, region_geom, area_raster),
    area_unknowncat10 = clip_and_sum(
      slices$unknown10,
      region_geom,
      area_raster
    ),
    count_nonstrict = unname(masks$counts[["nonstrict"]]),
    count_strict = unname(masks$counts[["strict"]]),
    count_unknowncat = unname(masks$counts[["unknown"]]),
    iso3 = iso,
    pop_nonstrict = clip_and_sum(slices$nonstrict, region_geom, pop_raster),
    pop_nonstrict10 = clip_and_sum(slices$nonstrict10, region_geom, pop_raster),
    pop_strict = clip_and_sum(slices$strict, region_geom, pop_raster),
    pop_strict10 = clip_and_sum(slices$strict10, region_geom, pop_raster),
    pop_total = exact_sum(pop_raster, region_geom),
    pop_unknowncat = clip_and_sum(slices$unknown, region_geom, pop_raster),
    pop_unknowncat10 = clip_and_sum(slices$unknown10, region_geom, pop_raster),
    pop_year = pop_year,
    scenario = scenario,
    source = source,
    `.geo` = empty_geojson
  )
}

# This is the per-region entry point. It loads the WDPA search subset and the
# population/area rasters ONCE per distinct pop_year needed by the scenario
# table, then loops over all scenarios reusing that cache. This keeps memory
# bounded to "one region's worth of buffered-extent rasters at a time" while
# avoiding redundant raster/WorldCover fetches across scenarios that share a
# pop_year (Confirmed_2020, Unknown_Year, All_2020 all use 2020).
compute_region_all_scenarios <- function(
  region,
  iso,
  adm_level,
  source,
  wdpa_country,
  boundary_iso,
  config,
  diag_context = NULL,
  region_index = NA_integer_,
  region_total = NA_integer_
) {
  region_proj <- st_transform(region, work_crs)
  region_geom <- st_geometry(region_proj)
  region_id <- as.character(region$shapeID[[1]])
  region_name <- as.character(region$shapeName[[1]])

  # See the note on region_extent_with_buffer(): this MUST stay the buffered
  # search geometry, not region_geom, or cross-ADM1-boundary PA exposure
  # within 10km will silently be dropped.
  search_geom <- region_extent_with_buffer(region)

  wdpa_started_at <- Sys.time()
  wdpa_spatial_subset <- wdpa_country |>
    filter_wdpa_base() |>
    st_filter(search_geom, .predicate = st_intersects)
  record_diagnostic_event(
    diag_context,
    stage = "wdpa_subset",
    status = "success",
    details = "WDPA subset prepared for buffered region extent",
    region_index = region_index,
    region_total = region_total,
    region_id = region_id,
    region_name = region_name,
    wdpa_rows = nrow(wdpa_spatial_subset),
    elapsed_seconds = as.numeric(difftime(Sys.time(), wdpa_started_at, units = "secs"))
  )

  distinct_years <- sort(unique(scenarios_reviewed$pop_year))

  raster_cache <- purrr::map(distinct_years, function(yr) {
    raster_started_at <- Sys.time()

    tryCatch(
      {
        pop_raster <- get_pop_raster(
          source = source,
          year = yr,
          raster_cache_dir = config$raster_cache_dir,
          pop_iso = boundary_iso,
          extent_geom = search_geom,
          use_land_mask = config$use_land_mask,
          s3_prefix = config$s3_raster_prefix,
          allow_empty_overlap = TRUE
        )
        area_raster <- if (is.null(pop_raster)) {
          NULL
        } else {
          terra::cellSize(pop_raster, unit = "km", mask = TRUE)
        }
        record_diagnostic_event(
          diag_context,
          stage = "raster_cache",
          status = "success",
          details = paste("Cached population and area rasters for year", yr),
          region_index = region_index,
          region_total = region_total,
          region_id = region_id,
          region_name = region_name,
          pop_year = yr,
          wdpa_rows = nrow(wdpa_spatial_subset),
          raster_cells = if (is.null(pop_raster)) 0 else terra::ncell(pop_raster),
          elapsed_seconds = as.numeric(difftime(Sys.time(), raster_started_at, units = "secs"))
        )
        list(pop = pop_raster, area = area_raster)
      },
      error = function(e) {
        record_diagnostic_event(
          diag_context,
          stage = "raster_cache",
          status = "failed",
          details = conditionMessage(e),
          region_index = region_index,
          region_total = region_total,
          region_id = region_id,
          region_name = region_name,
          pop_year = yr,
          wdpa_rows = nrow(wdpa_spatial_subset),
          elapsed_seconds = as.numeric(difftime(Sys.time(), raster_started_at, units = "secs"))
        )
        stop(e)
      }
    )
  })
  names(raster_cache) <- as.character(distinct_years)

  result <- pmap_dfr(
    scenarios_reviewed,
    function(scenario, pop_year) {
      scenario_started_at <- Sys.time()

      tryCatch(
        {
          rasters <- raster_cache[[as.character(pop_year)]]
          scenario_result <- compute_region_result_for_scenario(
            region = region,
            iso = iso,
            adm_level = adm_level,
            source = source,
            scenario = scenario,
            pop_year = pop_year,
            region_geom = region_geom,
            wdpa_spatial_subset = wdpa_spatial_subset,
            pop_raster = rasters$pop,
            area_raster = rasters$area
          )
          record_diagnostic_event(
            diag_context,
            stage = "scenario",
            status = "success",
            details = paste("Scenario completed:", scenario),
            region_index = region_index,
            region_total = region_total,
            region_id = region_id,
            region_name = region_name,
            scenario = scenario,
            pop_year = pop_year,
            wdpa_rows = nrow(wdpa_spatial_subset),
            raster_cells = if (is.null(rasters$pop)) 0 else terra::ncell(rasters$pop),
            elapsed_seconds = as.numeric(difftime(Sys.time(), scenario_started_at, units = "secs"))
          )
          scenario_result
        },
        error = function(e) {
          record_diagnostic_event(
            diag_context,
            stage = "scenario",
            status = "failed",
            details = conditionMessage(e),
            region_index = region_index,
            region_total = region_total,
            region_id = region_id,
            region_name = region_name,
            scenario = scenario,
            pop_year = pop_year,
            wdpa_rows = nrow(wdpa_spatial_subset),
            elapsed_seconds = as.numeric(difftime(Sys.time(), scenario_started_at, units = "secs"))
          )
          stop(e)
        }
      )
    }
  )

  rm(raster_cache)
  result
}

# This assembles the full country export that matches the reviewed per-country ADM files.
compute_country_output <- function(iso, source, config) {
  boundary_info <- load_boundary_units(iso)
  wdpa_info <- load_wdpa_country(iso, config)
  diag_context <- new_diagnostic_context(
    config,
    iso = iso,
    source = source,
    step = "adm",
    output_path = output_file_for(iso, source, config$output_dir)
  )
  n_regions <- nrow(boundary_info$regions)

  record_diagnostic_event(
    diag_context,
    stage = "country_setup",
    status = "success",
    details = paste("Boundary and WDPA loaded from", wdpa_info$source),
    region_total = n_regions,
    wdpa_rows = nrow(wdpa_info$data)
  )

  region_rows <- seq_len(n_regions) |>
    map_dfr(function(index) {
      region <- boundary_info$regions[index, ]
      region_id <- as.character(region$shapeID[[1]])
      region_name <- as.character(region$shapeName[[1]])
      region_started_at <- Sys.time()

      record_diagnostic_event(
        diag_context,
        stage = "region",
        status = "started",
        details = "Starting region computation",
        region_index = index,
        region_total = n_regions,
        region_id = region_id,
        region_name = region_name,
        wdpa_rows = nrow(wdpa_info$data)
      )

      result <- tryCatch(
        {
          region_result <- compute_region_all_scenarios(
            region = region,
            iso = iso,
            adm_level = boundary_info$adm_level,
            source = source,
            wdpa_country = wdpa_info$data,
            boundary_iso = boundary_info$boundary_iso,
            config = config,
            diag_context = diag_context,
            region_index = index,
            region_total = n_regions
          )
          append_partial_output(region_result, diag_context)
          record_diagnostic_event(
            diag_context,
            stage = "region",
            status = "success",
            details = "Region computation completed",
            region_index = index,
            region_total = n_regions,
            region_id = region_id,
            region_name = region_name,
            wdpa_rows = nrow(wdpa_info$data),
            elapsed_seconds = as.numeric(difftime(Sys.time(), region_started_at, units = "secs")),
            output_path = diag_context$partial_output_path
          )
          region_result
        },
        error = function(e) {
          record_diagnostic_event(
            diag_context,
            stage = "region",
            status = "failed",
            details = conditionMessage(e),
            region_index = index,
            region_total = n_regions,
            region_id = region_id,
            region_name = region_name,
            wdpa_rows = nrow(wdpa_info$data),
            elapsed_seconds = as.numeric(difftime(Sys.time(), region_started_at, units = "secs"))
          )
          stop(e)
        }
      )

      invisible(gc(verbose = FALSE))
      result
    })

  list(rows = region_rows, boundary_info = boundary_info, wdpa_info = wdpa_info)
}

read_country_output <- function(iso, source, output_dir) {
  path <- output_file_for(iso, source, output_dir)

  if (!file.exists(path)) {
    stop(
      "Missing ADM output required for national aggregation: ",
      path,
      call. = FALSE
    )
  }

  read_csv(
    path,
    col_types = cols(iso3 = col_character()),
    show_col_types = FALSE
  )
}

sum_metric <- function(data, metric, filter_expr = TRUE) {
  data |>
    filter({{ filter_expr }}) |>
    summarize(value = sum(.data[[metric]], na.rm = TRUE)) |>
    pull(value)
}

# This computes one national totals row from existing ADM outputs plus lightweight
# country-level WDPA counts. It avoids the heavy country-wide raster extraction.
#
# FIX relative to the earlier version: area/pop figures for "2020" are read
# directly from the All_2020 scenario rows (computed jointly, see refactor
# note 1) instead of being reconstructed by summing Confirmed_2020 +
# Unknown_Year, which double-counted overlapping buffer/core zones between
# the two independently-hierarchized scenarios. Confirmed-year-only figures
# are kept as separate *_confirmed2020 columns for anyone doing the temporal
# 2000-vs-2020 comparison, so nothing that previously existed is silently
# renamed away.
compute_country_national_totals <- function(iso, config) {
  ghsl <- read_country_output(iso, "GHSL", config$output_dir)
  wp <- read_country_output(iso, "WP", config$output_dir)
  wdpa_info <- load_wdpa_country(iso, config)

  # Counts restricted to the All_2020-eligible set (STATUS_YR <= 2020 or
  # missing), so they line up with the All_2020 area/pop figures below,
  # rather than mixing an unrestricted country-wide count against a
  # year-restricted area (a mismatch present in the earlier version, which
  # could include PAs designated after 2020 in the counts but not the areas).
  wdpa_all2020 <- wdpa_info$data |>
    filter_wdpa_base() |>
    scenario_filter_mask("All_2020")
  if (iso %in% names(special_boundary_names)) {
    boundary_info <- load_boundary_units(iso)
    wdpa_all2020 <- wdpa_all2020 |>
      st_filter(boundary_info$regions, .predicate = st_intersects)
  }
  wdpa_masks <- build_category_masks(wdpa_all2020)

  tibble(
    `system:index` = "0",
    iso3 = iso,

    # --- All_2020 (headline cross-sectional figures; matches Table 1 / Figure 1) ---
    area_strict = sum_metric(ghsl, "area_strict", scenario == "All_2020"),
    area_nonstrict = sum_metric(ghsl, "area_nonstrict", scenario == "All_2020"),
    area_unknown = sum_metric(ghsl, "area_unknowncat", scenario == "All_2020"),
    area_total_pa = area_strict + area_nonstrict + area_unknown,
    nat_pop_gh_20 = sum_metric(ghsl, "pop_total", scenario == "All_2020"),
    nat_pop_wp_20 = sum_metric(wp, "pop_total", scenario == "All_2020"),

    # --- Confirmed-year-only figures (for the 2000-vs-2020 temporal comparison) ---
    area_strict_confirmed2020 = sum_metric(
      ghsl,
      "area_strict",
      scenario == "Confirmed_2020"
    ),
    area_nonstrict_confirmed2020 = sum_metric(
      ghsl,
      "area_nonstrict",
      scenario == "Confirmed_2020"
    ),
    area_unknown_confirmed2020 = sum_metric(
      ghsl,
      "area_unknowncat",
      scenario == "Confirmed_2020"
    ),
    nat_pop_gh_20_confirmed2020 = sum_metric(
      ghsl,
      "pop_total",
      scenario == "Confirmed_2020"
    ),
    nat_pop_wp_20_confirmed2020 = sum_metric(
      wp,
      "pop_total",
      scenario == "Confirmed_2020"
    ),

    nat_pop_gh_00 = sum_metric(ghsl, "pop_total", scenario == "Confirmed_2000"),
    nat_pop_wp_00 = sum_metric(wp, "pop_total", scenario == "Confirmed_2000"),

    count_nonstrict = unname(wdpa_masks$counts[["nonstrict"]]),
    count_strict = unname(wdpa_masks$counts[["strict"]]),
    count_total = unname(wdpa_masks$counts[["total"]]),
    count_unknown = unname(wdpa_masks$counts[["unknown"]]),

    `.geo` = empty_geojson
  )
}

# Output paths and progress --------------------------------------------------

output_file_for <- function(iso, source, output_dir) {
  file.path(output_dir, sprintf("PA_Pop_%s_%s.csv", iso, source))
}

national_output_file_for <- function(iso, national_output_dir) {
  file.path(
    national_output_dir,
    sprintf("National_PA_Totals_Refactored_%s.csv", iso)
  )
}

progress_message <- function(
  step_index,
  step_total,
  label,
  elapsed_seconds,
  average_seconds
) {
  remaining_steps <- max(step_total - step_index, 0)
  eta_seconds <- remaining_steps * average_seconds
  eta <- format(Sys.time() + eta_seconds, tz = "UTC", usetz = TRUE)

  message(
    sprintf(
      "[%s/%s] %s finished in %.1fs | avg %.1fs | ETA %s",
      step_index,
      step_total,
      label,
      elapsed_seconds,
      average_seconds,
      eta
    )
  )
}

task_start_message <- function(step_index, step_total, label, details = NULL) {
  msg <- sprintf("[%s/%s] starting %s", step_index, step_total, label)

  if (!is.null(details) && nzchar(details)) {
    msg <- paste0(msg, " | ", details)
  }

  message(msg)
}

record_task_status <- function(task_row, manifest_path) {
  updated <- upsert_csv_row(
    row = task_row,
    output_path = manifest_path,
    key_cols = c("task_id"),
    col_types = cols(
      task_id = col_character(),
      iso3 = col_character(),
      source = col_character(),
      step = col_character(),
      status = col_character(),
      message = col_character(),
      started_at = col_character(),
      finished_at = col_character(),
      elapsed_seconds = col_double(),
      output_path = col_character()
    )
  )

  invisible(updated)
}

# Validation and main loop ---------------------------------------------------

validate_config <- function(config) {
  ensure_dir(config$wdpa_dir)
  ensure_dir(config$wdpa_spatial_cache_dir)

  local_spatial <- local_wdpa_spatial_iso3(config$wdpa_spatial_cache_dir)
  local_iso <- local_wdpa_shapefile_iso3(config$wdpa_dir)
  required_wdpa_iso <- unique(vapply(
    config$iso3,
    wdpa_lookup_iso,
    character(1)
  ))
  missing_local <- setdiff(required_wdpa_iso, union(local_spatial, local_iso))

  if (
    length(missing_local) > 0 &&
      !nzchar(Sys.which("aws")) &&
      !nzchar(config$s3_wdpa_spatial_prefix)
  ) {
    warning(
      "Some ISO3 codes have no local WDPA source and no S3 fallback is available: ",
      paste(missing_local, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(config)
}

# This is the public entry point for the workflow.
# It runs sequentially, writes progress after every step, and keeps outputs on disk.
run_population_pa_reproduction <- function(config = default_config) {
  config <- normalize_config(config)
  config <- validate_config(config)

  ensure_dir(config$output_dir)
  ensure_dir(config$national_output_dir)
  ensure_dir(config$diagnostic_dir)
  ensure_dir(config$progress_dir)
  ensure_dir(config$raster_cache_dir)
  ensure_dir(config$wdpa_spatial_cache_dir)

  if (isTRUE(config$sync_raster_cache_on_startup)) {
    sync_directory_from_s3(
      config$raster_cache_dir,
      config$s3_raster_prefix,
      "raster cache"
    )
  } else {
    message(
      "Skipping full raster cache sync; files will be fetched from S3 on demand"
    )
  }
  if (isTRUE(config$sync_wdpa_spatial_cache_on_startup)) {
    sync_directory_from_s3(
      config$wdpa_spatial_cache_dir,
      config$s3_wdpa_spatial_prefix,
      "spatial WDPA cache"
    )
  }

  manifest_path <- file.path(config$progress_dir, "run_summary.csv")
  n_steps_per_country <- if (isTRUE(config$national_only)) {
    1L
  } else {
    length(config$sources) + 1L
  }
  total_steps <- length(config$iso3) * n_steps_per_country
  completed_seconds <- numeric()
  step_index <- 0L

  message("Starting reviewed population and protected-area reproduction")
  if (isTRUE(config$national_only)) {
    message(
      "National-only mode is enabled: ADM exports will be skipped and only national totals will be computed"
    )
  }

  for (iso in config$iso3) {
    country_ok <- TRUE

    if (!isTRUE(config$national_only)) {
      for (source in config$sources) {
        step_index <- step_index + 1L
        task_id <- sprintf("%s_%s_adm", iso, source)
        output_path <- output_file_for(iso, source, config$output_dir)
        diag_context <- new_diagnostic_context(
          config,
          iso = iso,
          source = source,
          step = "adm",
          output_path = output_path
        )

        task_start_message(
          step_index,
          total_steps,
          task_id,
          sprintf("ADM export -> %s", output_path)
        )
        record_diagnostic_event(
          diag_context,
          stage = "task",
          status = "started",
          details = "ADM export task started",
          output_path = output_path
        )

        if (file.exists(output_path) && !isTRUE(config$overwrite)) {
          task_row <- tibble(
            task_id = task_id,
            iso3 = iso,
            source = source,
            step = "adm",
            status = "skipped",
            message = "existing output kept",
            started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
            elapsed_seconds = 0,
            output_path = output_path
          )
          record_task_status(task_row, manifest_path)
          upload_output_if_needed(
            manifest_path,
            config$progress_dir,
            s3_join(config$s3_output_prefix, "progress")
          )
          record_diagnostic_event(
            diag_context,
            stage = "task",
            status = "skipped",
            details = "Existing ADM output kept",
            output_path = output_path
          )
          message(sprintf(
            "[%s/%s] %s skipped because %s already exists",
            step_index,
            total_steps,
            task_id,
            output_path
          ))
          progress_message(
            step_index,
            total_steps,
            task_id,
            0,
            if (length(completed_seconds) == 0) 0 else mean(completed_seconds)
          )
          next
        }

        started_at <- Sys.time()
        task_status <- tryCatch(
          {
            result <- compute_country_output(iso, source, config)
            write_csv_atomic(result$rows, output_path)
            upload_output_if_needed(
              output_path,
              config$output_dir,
              config$s3_output_prefix
            )
            record_diagnostic_event(
              diag_context,
              stage = "task",
              status = "success",
              details = "ADM export task completed",
              output_path = output_path
            )
            list(status = "success", message = "", output_path = output_path)
          },
          error = function(e) {
            country_ok <<- FALSE
            record_diagnostic_event(
              diag_context,
              stage = "task",
              status = "failed",
              details = conditionMessage(e),
              output_path = output_path
            )
            list(
              status = "failed",
              message = conditionMessage(e),
              output_path = output_path
            )
          }
        )

        elapsed_seconds <- as.numeric(difftime(
          Sys.time(),
          started_at,
          units = "secs"
        ))
        completed_seconds <- c(completed_seconds, elapsed_seconds)
        task_row <- tibble(
          task_id = task_id,
          iso3 = iso,
          source = source,
          step = "adm",
          status = task_status$status,
          message = task_status$message,
          started_at = format(started_at, tz = "UTC", usetz = TRUE),
          finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
          elapsed_seconds = elapsed_seconds,
          output_path = task_status$output_path
        )
        record_task_status(task_row, manifest_path)
        upload_output_if_needed(
          manifest_path,
          config$progress_dir,
          s3_join(config$s3_output_prefix, "progress")
        )
        progress_message(
          step_index,
          total_steps,
          task_id,
          elapsed_seconds,
          mean(completed_seconds)
        )
      }
    }

    step_index <- step_index + 1L
    national_task_id <- sprintf("%s_national", iso)
    national_output_path <- national_output_file_for(
      iso,
      config$national_output_dir
    )
    national_diag_context <- new_diagnostic_context(
      config,
      iso = iso,
      source = "ALL",
      step = "national",
      output_path = national_output_path
    )

    task_start_message(
      step_index,
      total_steps,
      national_task_id,
      sprintf("national totals -> %s", national_output_path)
    )
    record_diagnostic_event(
      national_diag_context,
      stage = "task",
      status = "started",
      details = "National totals task started",
      output_path = national_output_path
    )

    if (!country_ok) {
      task_row <- tibble(
        task_id = national_task_id,
        iso3 = iso,
        source = "ALL",
        step = "national",
        status = "skipped",
        message = "country ADM outputs failed",
        started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        elapsed_seconds = 0,
        output_path = national_output_path
      )
      record_task_status(task_row, manifest_path)
      upload_output_if_needed(
        manifest_path,
        config$progress_dir,
        s3_join(config$s3_output_prefix, "progress")
      )
      record_diagnostic_event(
        national_diag_context,
        stage = "task",
        status = "skipped",
        details = "National totals skipped because an ADM task failed",
        output_path = national_output_path
      )
      message(sprintf(
        "[%s/%s] %s skipped because one ADM export failed for %s",
        step_index,
        total_steps,
        national_task_id,
        iso
      ))
      progress_message(
        step_index,
        total_steps,
        national_task_id,
        0,
        if (length(completed_seconds) == 0) 0 else mean(completed_seconds)
      )
      next
    }

    if (file.exists(national_output_path) && !isTRUE(config$overwrite)) {
      task_row <- tibble(
        task_id = national_task_id,
        iso3 = iso,
        source = "ALL",
        step = "national",
        status = "skipped",
        message = "existing national totals kept",
        started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        elapsed_seconds = 0,
        output_path = national_output_path
      )
      record_task_status(task_row, manifest_path)
      upload_output_if_needed(
        manifest_path,
        config$progress_dir,
        s3_join(config$s3_output_prefix, "progress")
      )
      record_diagnostic_event(
        national_diag_context,
        stage = "task",
        status = "skipped",
        details = "Existing national totals kept",
        output_path = national_output_path
      )
      message(sprintf(
        "[%s/%s] %s skipped because %s already exists",
        step_index,
        total_steps,
        national_task_id,
        national_output_path
      ))
      progress_message(
        step_index,
        total_steps,
        national_task_id,
        0,
        if (length(completed_seconds) == 0) 0 else mean(completed_seconds)
      )
      next
    }

    started_at <- Sys.time()
    national_status <- tryCatch(
      {
        national_row <- compute_country_national_totals(iso, config)
        write_csv_atomic(national_row, national_output_path)
        upload_output_if_needed(
          national_output_path,
          config$national_output_dir,
          s3_join(config$s3_output_prefix, "national_totals")
        )
        record_diagnostic_event(
          national_diag_context,
          stage = "task",
          status = "success",
          details = "National totals task completed",
          output_path = national_output_path
        )
        list(status = "success", message = "")
      },
      error = function(e) {
        record_diagnostic_event(
          national_diag_context,
          stage = "task",
          status = "failed",
          details = conditionMessage(e),
          output_path = national_output_path
        )
        list(status = "failed", message = conditionMessage(e))
      }
    )

    elapsed_seconds <- as.numeric(difftime(
      Sys.time(),
      started_at,
      units = "secs"
    ))
    completed_seconds <- c(completed_seconds, elapsed_seconds)
    task_row <- tibble(
      task_id = national_task_id,
      iso3 = iso,
      source = "ALL",
      step = "national",
      status = national_status$status,
      message = national_status$message,
      started_at = format(started_at, tz = "UTC", usetz = TRUE),
      finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      elapsed_seconds = elapsed_seconds,
      output_path = national_output_path
    )
    record_task_status(task_row, manifest_path)
    upload_output_if_needed(
      manifest_path,
      config$progress_dir,
      s3_join(config$s3_output_prefix, "progress")
    )
    progress_message(
      step_index,
      total_steps,
      national_task_id,
      elapsed_seconds,
      mean(completed_seconds)
    )

    invisible(gc(verbose = FALSE))
  }

  invisible(
    list(
      output_dir = config$output_dir,
      national_output_dir = config$national_output_dir,
      manifest_path = manifest_path
    )
  )
}

# This runs the full reviewed country list with the default folders.
run_full_reviewed_reproduction <- function(overwrite = FALSE) {
  config <- new_reproduction_config(
    iso3 = reviewed_iso3,
    overwrite = overwrite
  )

  run_population_pa_reproduction(config)
}

run_selected_reproduction <- function(overwrite = run_overwrite) {
  config <- new_reproduction_config(
    iso3 = run_iso3,
    sources = run_sources,
    overwrite = overwrite,
    national_only = run_national_only
  )

  run_population_pa_reproduction(config)
}

# This is a smaller example for quick interactive checks.
run_example_reproduction <- function() {
  config <- new_reproduction_config(
    iso3 = c("AFG", "BGD"),
    overwrite = FALSE
  )

  run_population_pa_reproduction(config)
}

if (
  interactive() &&
    identical(environment(), globalenv()) &&
    isTRUE(run_on_source)
) {
  message("Executing reproduction for the ISO3 codes listed in run_iso3")
  run_selected_reproduction()
}
