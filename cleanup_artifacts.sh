#!/bin/bash

# Utility script to purge generated binaries and logs from the repository root.
# This helps keep version control history clean and prevents large build artefacts
# from being accidentally committed again.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")" && pwd)"

cd "$REPO_ROOT"

echo "[cleanup] Removing extensionless binaries from repository root..."
find "$REPO_ROOT" -maxdepth 1 -type f ! -name '*.*' ! -name 'LICENSE' -delete

echo "[cleanup] Removing root-level logs and temporary text outputs..."
rm -f \
  "$REPO_ROOT"/*.log \
  "$REPO_ROOT"/*.tmp \
  "$REPO_ROOT"/*_output.txt \
  "$REPO_ROOT"/*_output_*.txt \
  "$REPO_ROOT"/*_log.txt \
  "$REPO_ROOT"/result_*.txt \
  "$REPO_ROOT"/stack_test_output.txt \
  "$REPO_ROOT"/warnings*.txt

echo "[cleanup] All done. Generated artefacts now live under dist/ or tmp/."
