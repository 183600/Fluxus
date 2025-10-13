#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"

mapfile -t PY_FILES < <(find "$PROJECT_ROOT" -type f -name "*.py" \
  -not -path "*/.git/*" -not -path "*/__pycache__/*" -not -path "*/dist-newstyle/*" -not -path "*/.stack-work/*")

failures=()
idx=0
for f in "${PY_FILES[@]}"; do
  idx=$((idx+1))
  echo "[$idx/${#PY_FILES[@]}] Compile+verify: $f"
  stack exec fluxus -- --python -O2 "$f" -o fibonacci >/dev/null
  expected=$(python3 "$f" 2>/dev/null || true)
  actual=$(./fibonacci 2>/dev/null || true)
  if [[ "$expected" != "$actual" ]]; then
    echo "  MISMATCH";
    failures+=("$f")
  fi
done

if (( ${#failures[@]} > 0 )); then
  echo "\nBehavior mismatches (${#failures[@]}):"
  for f in "${failures[@]}"; do
    echo "  $f"
  done
  exit 1
fi

echo "\nAll compiled executables matched Python outputs (no-arg run)."
exit 0
