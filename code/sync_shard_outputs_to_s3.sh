#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 4 ]]; then
  echo "Usage: bash code/sync_shard_outputs_to_s3.sh <shard_name> <run_dir> <original_dir> <fix_dir> [s3_prefix]" >&2
  exit 1
fi

SHARD_NAME="$1"
RUN_DIR="$2"
ORIGINAL_DIR="$3"
FIX_DIR="$4"
S3_PREFIX="${5:-s3://projet-betsaka/diffusion/population_pas/shards}"

TARGET_BASE="${S3_PREFIX%/}/${SHARD_NAME}"

echo "Syncing shard '${SHARD_NAME}' to ${TARGET_BASE}"

if [[ -d "$ORIGINAL_DIR" ]]; then
  aws s3 sync "$ORIGINAL_DIR" "${TARGET_BASE}/original/" --only-show-errors
else
  echo "Original dir not found: $ORIGINAL_DIR" >&2
fi

if [[ -d "$FIX_DIR" ]]; then
  aws s3 sync "$FIX_DIR" "${TARGET_BASE}/fix/" --only-show-errors
else
  echo "Fix dir not found: $FIX_DIR" >&2
fi

if [[ -d "$RUN_DIR" ]]; then
  aws s3 sync "$RUN_DIR" "${TARGET_BASE}/run/" --only-show-errors
else
  echo "Run dir not found: $RUN_DIR" >&2
fi

echo "Sync complete"