#!/bin/bash

LOG_DIR="dist/logs"
LOG_FILE="$LOG_DIR/stack_test_output.txt"

mkdir -p "$LOG_DIR"

stack test 2>&1 | tee "$LOG_FILE"
echo "Exit code: $?"
echo "Test log saved to: $LOG_FILE"
