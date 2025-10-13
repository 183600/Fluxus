#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "Scanning for .py files under: $PROJECT_ROOT"
mapfile -t PY_FILES < <(find "$PROJECT_ROOT" -type f -name "*.py" \
  -not -path "*/.git/*" \
  -not -path "*/__pycache__/*" \
  -not -path "*/dist-newstyle/*" \
  -not -path "*/.stack-work/*")

echo "Found ${#PY_FILES[@]} Python files"

failures=()
idx=0
for f in "${PY_FILES[@]}"; do
  idx=$((idx+1))
  echo "[$idx/${#PY_FILES[@]}] Compiling: $f"
  if ! stack exec fluxus -- --python -O2 "$f" -o fibonacci; then
    echo "FAILED: $f"
    failures+=("$f")
  fi
done

if (( ${#failures[@]} > 0 )); then
  echo "\nCompilation failures (${#failures[@]}):"
  for f in "${failures[@]}"; do
    echo "  $f"
  done
  exit 1
fi

echo "\nAll Python files compiled successfully."
exit 0
