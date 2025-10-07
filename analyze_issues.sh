#!/bin/bash

echo "=== Analyzing Go Parser Struct Declaration Issue ==="
iflow -p "Go parser struct declaration parsing is failing. The test expects to parse: 'type Person struct { Name string; Age int; }' but the parser is not handling struct fields correctly. Check the parseStructType function and how it's integrated into parseTypeDecl." --yolo

echo ""
echo "=== Analyzing Go Parser Interface Declaration Issue ==="
iflow -p "Go parser interface declaration parsing is failing. The test expects to parse: 'type Writer interface { func Write(p []byte) (n int, err error); }' but the parser is not handling interface method specifications correctly. Check the parseInterfaceType function and method parsing." --yolo

echo ""
echo "=== Analyzing Escape Analysis Issue ==="
iflow -p "Escape analysis is failing to identify escaping variables. The test expects variable 'x' in 'def func(): x = [1, 2, 3]; return x' to be identified as escaping, but the analysis returns empty list. Check the analyzeEscape function and getEscapingVariables logic." --yolo

echo ""
echo "=== Analyzing Ownership Inference Borrowing Issue ==="
iflow -p "Ownership inference is failing to identify immutable and mutable borrows. Tests expect variables to be marked as Borrowed Immutable or Borrowed Mutable but they're showing as Owned. Check the borrowing analysis logic in inferOwnershipFromTextIO." --yolo

echo ""
echo "=== Analyzing Shape Analysis Timeout Issue ==="
iflow -p "Shape analysis is timing out during tests, likely due to infinite loop or performance issue. The test 'infers shape of primitive values' is hanging. Check the analyzeShapeFromText function and runShapeAnalysis for potential infinite loops or recursion issues." --yolo