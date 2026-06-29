#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

RUN_ID="${RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)}"
RUN_DIR="${RUN_DIR:-results/reproduction_runs/${RUN_ID}}"
CACHE_DIR="${CACHE_DIR:-data/cache_direct}"
WDPA_DIR="${WDPA_DIR:-data/WDPA_2020_05_GEE}"
WDPA_OCT_ZIP="${WDPA_OCT_ZIP:-/tmp/WDPA_2021.zip}"
WDPA_OCT_EXTRACT_DIR="${WDPA_OCT_EXTRACT_DIR:-data/cache_wdpa_oct2021}"
USE_LAND_MASK="${USE_LAND_MASK:-true}"
MAX_PARALLEL="${MAX_PARALLEL:-2}"
MANIFEST_ORDER="${MANIFEST_ORDER:-forward}"
SHARD_INDEX="${SHARD_INDEX:-0}"
SHARD_COUNT="${SHARD_COUNT:-1}"

ORIGINAL_DIR="${ORIGINAL_DIR:-data/Output_R_mapme_reviewed_original}"
FIX_DIR="${FIX_DIR:-data/Output_R_mapme_all2020_fix}"

mkdir -p "$RUN_DIR" "$RUN_DIR/logs" "$RUN_DIR/profiles" "$RUN_DIR/status" "$RUN_DIR/meta" "$RUN_DIR/manifests"
mkdir -p "$ORIGINAL_DIR" "$FIX_DIR" "$CACHE_DIR" "$WDPA_OCT_EXTRACT_DIR"

BATCH_EVENT_LOG="$RUN_DIR/meta/batch_events.log"
touch "$BATCH_EVENT_LOG"

log_batch_event() {
  printf '%s\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$1" >> "$BATCH_EVENT_LOG"
}

trap 'log_batch_event "signal:TERM"' TERM
trap 'log_batch_event "signal:INT"' INT
trap 'log_batch_event "signal:HUP"' HUP

log_batch_event "batch:start run_dir=${RUN_DIR} max_parallel=${MAX_PARALLEL} manifest_order=${MANIFEST_ORDER} shard_index=${SHARD_INDEX} shard_count=${SHARD_COUNT}"

if [[ -f data/tests/pod_config.md ]]; then
  cp data/tests/pod_config.md "$RUN_DIR/meta/pod_config.md"
else
  printf '%s\n' 'pod_config.md not available in this clone' > "$RUN_DIR/meta/pod_config.md"
fi
git rev-parse HEAD > "$RUN_DIR/meta/git_rev.txt"
env | sort > "$RUN_DIR/meta/environment.txt"

MANIFEST="$RUN_DIR/manifests/tasks.tsv"

python - <<'PY' > "$MANIFEST"
import os

iso_list = [
  'AFG','AGO','BGD','BEN','BTN','BOL','BFA','BDI','CPV','KHM','CMR','CAF','TCD','COM','COD','COG','CIV','DJI','EGY','SLV',
  'ERI','SWZ','ETH','GMB','GHA','GIN','GNB','HTI','HND','IDN','KEN','KIR','PRK','KGZ','LAO','LSO','LBR','MDG','MWI',
  'MLI','MRT','FSM','MDA','MNG','MAR','MOZ','MMR','NPL','NIC','NER','NGA','PAK','PNG','PHL','RWA','STP','SEN','SLE','SLB',
  'SOM','SSD','SDN','SYR','TJK','TZA','TLS','TGO','TUN','UGA','UKR','UZB','VUT','VNM','118','129','YEM','ZMB','ZWE'
]

order_mode = os.environ.get('MANIFEST_ORDER', 'forward').strip().lower()
shard_index = int(os.environ.get('SHARD_INDEX', '0'))
shard_count = int(os.environ.get('SHARD_COUNT', '1'))

if shard_count < 1:
  raise ValueError('SHARD_COUNT must be >= 1')
if shard_index < 0 or shard_index >= shard_count:
  raise ValueError('SHARD_INDEX must satisfy 0 <= SHARD_INDEX < SHARD_COUNT')

tasks = [(iso, source) for iso in iso_list for source in ('GHSL', 'WP')]

if order_mode == 'reverse':
  tasks = list(reversed(tasks))
elif order_mode != 'forward':
  raise ValueError("MANIFEST_ORDER must be 'forward' or 'reverse'")

tasks = [task for idx, task in enumerate(tasks) if idx % shard_count == shard_index]

for iso, source in tasks:
  print(f"{iso}\t{source}")
PY

run_one() {
  local iso="$1"
  local source_name="$2"
  local task_id="${iso}_${source_name}"
  local original_output="${ORIGINAL_DIR}/PA_Pop_${iso}_${source_name}.csv"
  local fix_output="${FIX_DIR}/PA_Pop_${iso}_${source_name}.csv"
  local log_file="${RUN_DIR}/logs/${task_id}.log"
  local status_file="${RUN_DIR}/status/${task_id}.csv"

  if [[ -f "$status_file" ]] && grep -q ',success,' "$status_file" 2>/dev/null; then
    echo "SKIP ${task_id} status=success"
    log_batch_event "task:skip_success ${task_id}"
    return 0
  fi

  if [[ -f "$original_output" && -f "$fix_output" ]]; then
    echo "SKIP ${task_id} outputs-present"
    log_batch_event "task:skip_outputs ${task_id}"
    return 0
  fi

  echo "RUN  ${task_id}"
  log_batch_event "task:start ${task_id}"

  if Rscript code/run_reproduction_task.R \
    "$iso" "$source_name" "$RUN_DIR" \
    "$original_output" "$fix_output" \
    "$CACHE_DIR" "$WDPA_DIR" "$WDPA_OCT_ZIP" "$WDPA_OCT_EXTRACT_DIR" "$USE_LAND_MASK" \
    > "$log_file" 2>&1; then
    log_batch_event "task:success ${task_id}"
  else
    log_batch_event "task:failed ${task_id}"
  fi

  return 0
}

export RUN_DIR ORIGINAL_DIR FIX_DIR CACHE_DIR WDPA_DIR WDPA_OCT_ZIP WDPA_OCT_EXTRACT_DIR USE_LAND_MASK BATCH_EVENT_LOG
export -f log_batch_event
export -f run_one

xargs -a "$MANIFEST" -n 2 -P "$MAX_PARALLEL" bash -lc 'run_one "$1" "$2"' _

Rscript code/summarise_reproduction_run.R "$RUN_DIR"
log_batch_event "batch:end run_dir=${RUN_DIR}"