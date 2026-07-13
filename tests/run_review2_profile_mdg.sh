#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
iso="${1:-MDG}"
run_id="${2:-$(date -u +%Y%m%dT%H%M%SZ)}"
run_root="$repo_root/tests/review2_profile/$iso/$run_id"
script_path="$repo_root/code/review2_compute_land_pop_restimates_noGEE.R"
stdout_log="$run_root/logs/stdout.log"
metrics_log="$run_root/logs/system_metrics.tsv"
s3_prefix="${REVIEW2_DIAGNOSTIC_S3_PREFIX:-s3://projet-betsaka/diffusion/population_pas/reviewed_PA_Pop_GHSL_Worldpop/diagnostics/$iso/$run_id}"

mkdir -p \
  "$run_root/logs" \
  "$run_root/output/national_totals" \
  "$run_root/progress" \
  "$run_root/diagnostics"

cat <<EOF
Run root: $run_root
ISO3: $iso
Diagnostics S3 prefix: $s3_prefix
EOF

Rscript --vanilla -e "source('$script_path'); config <- new_reproduction_config(iso3 = c('$iso'), sources = c('GHSL', 'WP'), output_dir = '$run_root/output', national_output_dir = '$run_root/output/national_totals', progress_dir = '$run_root/progress', diagnostic_dir = '$run_root/diagnostics', diagnostic_s3_prefix = '$s3_prefix', overwrite = TRUE, aggressive_cleanup = TRUE, sync_raster_cache_on_startup = FALSE, sync_wdpa_spatial_cache_on_startup = TRUE); run_population_pa_reproduction(config)" \
  > >(tee -a "$stdout_log") \
  2> >(tee -a "$stdout_log" >&2) &
r_pid=$!

printf 'timestamp_utc\tpid\tpcpu\tpmem\trss_kb\tvsz_kb\tstat\tetime\tdisk_free_kb\tstdout_log\n' > "$metrics_log"

while kill -0 "$r_pid" 2>/dev/null; do
  ps_line="$(ps -p "$r_pid" -o pid=,%cpu=,%mem=,rss=,vsz=,stat=,etime= | xargs)"
  disk_free_kb="$(df -Pk "$run_root" | awk 'NR==2 {print $4}')"
  printf '%s\t%s\t%s\t%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    "$ps_line" \
    "$disk_free_kb" \
    "$stdout_log" >> "$metrics_log"
  sleep 30
done

wait "$r_pid"
status=$?

printf 'Process exited with status %s\n' "$status" | tee -a "$stdout_log"

if command -v aws >/dev/null 2>&1; then
  aws s3 sync "$run_root/diagnostics" "$s3_prefix" --no-progress --only-show-errors || true
  aws s3 cp "$metrics_log" "$s3_prefix/system_metrics.tsv" --only-show-errors || true
  aws s3 cp "$stdout_log" "$s3_prefix/stdout.log" --only-show-errors || true
fi

exit "$status"