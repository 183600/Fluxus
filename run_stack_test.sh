#!/bin/bash

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_ROOT"

LOG_DIR="$PROJECT_ROOT/dist/logs"
LOG_FILE="$LOG_DIR/stack_test_output.txt"

mkdir -p "$LOG_DIR"

format_duration() {
  local total_seconds=$1
  printf "%02d:%02d:%02d" $((total_seconds / 3600)) $(((total_seconds % 3600) / 60)) $((total_seconds % 60))
}

STACK_ARGS=()
FAST_FLAG="${STACK_TEST_FAST:-1}"
FAST_ENABLED=1
case "$FAST_FLAG" in
  0|false|False|FALSE|off|OFF)
    FAST_ENABLED=0
    ;;
esac

if [[ $FAST_ENABLED -eq 1 ]]; then
  STACK_ARGS+=("--fast")
fi

STACK_ARGS+=("--no-terminal")

if [[ $# -gt 0 ]]; then
  STACK_ARGS+=("$@")
fi

if [[ $FAST_ENABLED -eq 1 ]]; then
  echo "[stack-test] Fast mode enabled (override with STACK_TEST_FAST=0)."
else
  echo "[stack-test] Fast mode disabled via STACK_TEST_FAST."
fi

echo "[stack-test] Running: stack test ${STACK_ARGS[*]}"

START_TIME=$(date +%s)

set +e
stack test "${STACK_ARGS[@]}" 2>&1 | tee "$LOG_FILE"
STACK_EXIT=${PIPESTATUS[0]}
set -e

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))
HUMAN_DURATION=$(format_duration "$DURATION")

echo "[stack-test] Exit code: $STACK_EXIT"
echo "[stack-test] Duration: ${HUMAN_DURATION}"
echo "[stack-test] Test log saved to: $LOG_FILE"

exit $STACK_EXIT
