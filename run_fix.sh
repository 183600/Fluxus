#!/bin/bash
python3 auto_fix_remaining_warnings.py > fix_output.txt 2>&1
cat fix_output.txt
