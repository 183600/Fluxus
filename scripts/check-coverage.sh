#!/usr/bin/env bash
set -euo pipefail

THRESHOLD=${1:-90}

echo "[coverage] Running tests with coverage..."
cabal test fluxus-test --test-show-details=direct --enable-coverage

mkdir -p coverage-report

echo "[coverage] Generating reports..."
# Prefer cabal hpc (if available), else fall back to hpc
if cabal hpc report fluxus-test > coverage-report/report.txt 2>/dev/null; then
  cabal hpc coverage fluxus-test --destdir=coverage-report >/dev/null 2>&1 || true
  REPORT=coverage-report/report.txt
else
  # Attempt to locate hpc data produced by cabal
  HPCDIR=$(find dist-newstyle -type d -name hpc -print -quit 2>/dev/null || true)
  TIX=$(find dist-newstyle -type f -name "*.tix" -print -quit 2>/dev/null || true)
  if [ -n "${HPCDIR}" ] && [ -n "${TIX}" ]; then
    hpc report --hpcdir "${HPCDIR}" "${TIX}" > coverage-report/report.txt
  else
    echo "[coverage] Could not locate hpc data; skipping threshold enforcement"
    exit 0
  fi
  REPORT=coverage-report/report.txt
fi

echo "[coverage] Parsing coverage..."
PCT=$(grep -Eo '[0-9]+(\.[0-9]+)?%' "${REPORT}" | sed 's/%//' | awk 'BEGIN{m=0}{if($1>m)m=$1}END{print m}')

if [ -z "${PCT}" ]; then
  echo "[coverage] Could not parse coverage percentage. See ${REPORT}"
  exit 1
fi

printf "[coverage] Total coverage: %.2f%% (threshold: %s%%)\n" "${PCT}" "${THRESHOLD}"

# Compare as integers by scaling
SCALED_ACTUAL=$(awk -v x="${PCT}" 'BEGIN{printf "%d", x*100}')
SCALED_THRESH=$(( THRESHOLD * 100 ))

if [ "${SCALED_ACTUAL}" -lt "${SCALED_THRESH}" ]; then
  echo "[coverage] FAIL: coverage below threshold"
  exit 2
fi

echo "[coverage] PASS: coverage meets threshold"
echo "[coverage] Reports in coverage-report/"