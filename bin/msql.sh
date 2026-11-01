#!/usr/bin/env bash
set -euo pipefail

# Ensure required env vars exist
: "${ADZERK_MSQL_DATABASE:?ADZERK_MSQL_DATABASE is not set. Run sql-env.}"
: "${ADZERK_MSQL_PASSWORD:?ADZERK_MSQL_PASSWORD is not set. Run sql-env.}"
: "${ADZERK_MSQL_HOSTNAME:?ADZERK_MSQL_HOSTNAME is not set. Run sql-env.}"
: "${ADZERK_MSQL_PORT:?ADZERK_MSQL_PORT is not set. Run sql-env.}"
: "${ADZERK_MSQL_USER:?ADZERK_MSQL_USER is not set. Run sql-env.}"

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <script.sql> [additional sqlcmd args...]"
  exit 1
fi

SCRIPT_FILE="$1"
shift

sqlcmd \
  -S "${ADZERK_MSQL_HOSTNAME},${ADZERK_MSQL_PORT}" \
  -d "${ADZERK_MSQL_DATABASE}" \
  -U "${ADZERK_MSQL_USER}" \
  -P "${ADZERK_MSQL_PASSWORD}" \
  -i "$SCRIPT_FILE" \
  "$@"
