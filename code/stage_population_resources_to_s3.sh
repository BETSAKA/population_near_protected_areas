#!/usr/bin/env bash

set -euo pipefail

S3_PREFIX="${1:-s3://projet-betsaka/diffusion/population_pas}"
TMP_DIR="${TMP_DIR:-/tmp/population_pas_stage}"
LOG_DIR="${LOG_DIR:-$TMP_DIR/logs}"
WORLDPOP_PARALLEL="${WORLDPOP_PARALLEL:-2}"

mkdir -p "$TMP_DIR" "$LOG_DIR"

YEARS=(2000 2020)
ISO_CODES=(
  AFG AGO BGD BEN BTN BOL BFA BDI CPV KHM CMR CAF TCD COM COD COG CIV DJI EGY SLV
  ERI SWZ ETH GMB GHA GIN GNB HTI HND IND IDN KEN KIR PRK KGZ LAO LSO LBR MDG MWI
  MLI MRT FSM MDA MNG MAR MOZ MMR NPL NIC NER NGA PAK PNG PHL RWA STP SEN SLE SLB
  SOM SSD SDN SYR TJK TZA TLS TGO TUN UGA UKR UZB VUT VNM YEM ZMB ZWE PSE
)

ghsl_urls=(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2000_GLOBE_R2023A_4326_3ss/V1-0/GHS_POP_E2000_GLOBE_R2023A_4326_3ss_V1_0.zip"
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_3ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip"
)

upload_stream() {
  local url="$1"
  local s3_path="$2"

  if aws s3 ls "$s3_path" >/dev/null 2>&1; then
    echo "SKIP $s3_path"
    return 0
  fi

  echo "PUT  $s3_path"
  curl -L --fail "$url" | aws s3 cp - "$s3_path"
}

upload_worldpop_one() {
  local year="$1"
  local iso="$2"
  local iso_lc
  iso_lc="$(printf '%s' "$iso" | tr 'A-Z' 'a-z')"
  local file_name="${iso_lc}_ppp_${year}.tif"
  local url="https://data.worldpop.org/GIS/Population/Global_2000_2020/${year}/${iso}/${file_name}"
  local s3_path="${S3_PREFIX}/worldpop/${year}/${iso}/${file_name}"

  upload_stream "$url" "$s3_path"
}

export S3_PREFIX
export -f upload_stream
export -f upload_worldpop_one

printf '%s\n' "${ghsl_urls[@]}" > "$TMP_DIR/ghsl_urls.txt"

for ghsl_url in "${ghsl_urls[@]}"; do
  upload_stream "$ghsl_url" "${S3_PREFIX}/ghsl/$(basename "$ghsl_url")"
done

worldpop_jobs_file="$TMP_DIR/worldpop_jobs.tsv"
: > "$worldpop_jobs_file"

for year in "${YEARS[@]}"; do
  for iso in "${ISO_CODES[@]}"; do
    printf '%s\t%s\n' "$year" "$iso" >> "$worldpop_jobs_file"
  done
done

xargs -a "$worldpop_jobs_file" -n 2 -P "$WORLDPOP_PARALLEL" bash -lc 'upload_worldpop_one "$1" "$2"' _