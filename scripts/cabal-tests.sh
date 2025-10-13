#!/usr/bin/env bash
set -euo pipefail

THRESHOLD=${1:-90}

echo "[cabal-tests] Building and running tests with coverage..."
cabal test fluxus-test --test-show-details=direct --enable-coverage

if [ -x scripts/check-coverage.sh ]; then
  echo "[cabal-tests] Checking coverage (threshold: ${THRESHOLD}%)..."
  scripts/check-coverage.sh "${THRESHOLD}"
else
  echo "[cabal-tests] Coverage checker not found, skipping threshold enforcement"
fi

echo "[cabal-tests] Done."
