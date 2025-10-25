#!/bin/bash
stack clean
stack build 2>&1 | tee build_output.txt
echo "=== WARNINGS SUMMARY ==="
grep -n "warning:" build_output.txt
