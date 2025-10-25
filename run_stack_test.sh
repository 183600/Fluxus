#!/bin/bash
stack test 2>&1 | tee stack_test_output.txt
echo "Exit code: $?"
