#!/bin/bash

echo "=== Go Parser Struct Declaration Issue ==="
echo "Error: Parser failed when trying to parse struct declaration"
echo "Test case: type Person struct { Name string; Age int; }"
echo "Location: test/Test/Fluxus/Parser/Go.hs:326"
echo ""
iflow -p "Go parser is failing to parse struct declarations. The test at line 326 in test/Test/Fluxus/Parser/Go.hs expects to parse 'type Person struct { Name string; Age int; }' but the parser returns a failure. Check the parseStructType function in src/Fluxus/Parser/Go/Parser.hs and how struct field parsing is implemented. The issue is likely in parsing the struct fields with proper semicolon handling." --yolo

echo ""
echo "=== Go Parser Interface Declaration Issue ==="
echo "Error: Parser failed when trying to parse interface declaration"
echo "Test case: type Writer interface { func Write(p []byte) (n int, err error); }"
echo "Location: test/Test/Fluxus/Parser/Go.hs:372"
echo ""
iflow -p "Go parser is failing to parse interface declarations. The test at line 372 in test/Test/Fluxus/Parser/Go.hs expects to parse 'type Writer interface { func Write(p []byte) (n int, err error); }' but the parser returns a failure. Check the parseInterfaceType function in src/Fluxus/Parser/Go/Parser.hs and how interface method specifications are parsed. The issue is likely in parsing method signatures within interfaces." --yolo

echo ""
echo "=== Escape Analysis - Identifying Escaping Variables ==="
echo "Error: Expected [Identifier \"x\"] but got []"
echo "Test case: def func(): x = [1, 2, 3]; return x"
echo "Location: test/Test/Fluxus/Analysis/EscapeAnalysis.hs:43"
echo ""
iflow -p "Escape analysis is failing to identify escaping variables. The test expects variable 'x' in 'def func(): x = [1, 2, 3]; return x' to be identified as escaping, but getEscapingVariables returns an empty list. Check the analyzeEscape function in src/Fluxus/Analysis/EscapeAnalysis.hs and the logic for detecting when variables escape their scope through return statements." --yolo

echo ""
echo "=== Escape Analysis - Nested Functions ==="
echo "Error: Expected [Identifier \"x\"] but got []"
echo "Test case: def outer(): x = 42; def inner(): return x + 1; return inner"
echo "Location: test/Test/Fluxus/Analysis/EscapeAnalysis.hs:60"
echo ""
iflow -p "Escape analysis is failing to detect escaping variables in nested functions. The test expects variable 'x' to be identified as escaping when it's captured by an inner function that escapes. Check the analyzeEscape function for handling nested function scopes and variable capture analysis." --yolo

echo ""
echo "=== Escape Analysis - Data Structures ==="
echo "Error: Expected [Identifier \"y\"] but got []"
echo "Test case: def func(): x = 42; y = {'value': x}; return y"
echo "Location: test/Test/Fluxus/Analysis/EscapeAnalysis.hs:74"
echo ""
iflow -p "Escape analysis is failing to detect escaping variables in data structures. The test expects variable 'y' to be identified as escaping when it's returned, and potentially 'x' as indirectly escaping. Check the analyzeEscape function for handling data structure creation and indirect escaping through container objects." --yolo

echo ""
echo "=== Ownership Inference - Immutable Borrows ==="
echo "Error: Expected Borrowed Immutable but got Owned"
echo "Test case: def func(): x = [1, 2, 3]; y = len(x); return y"
echo "Location: test/Test/Fluxus/Analysis/OwnershipInference.hs:101"
echo ""
iflow -p "Ownership inference is failing to identify immutable borrows. The test expects variable 'x' to be marked as Borrowed Immutable when used in len(x), but it's showing as Owned. Check the inferOwnershipFromTextIO function in src/Fluxus/Analysis/OwnershipInference.hs and the borrowing analysis logic for function calls that don't consume ownership." --yolo

echo ""
echo "=== Ownership Inference - Mutable Borrows ==="
echo "Error: Expected Borrowed Mutable but got Owned"
echo "Test case: def func(): x = [1, 2, 3]; x.append(4); return x"
echo "Location: test/Test/Fluxus/Analysis/OwnershipInference.hs:115"
echo ""
iflow -p "Ownership inference is failing to identify mutable borrows. The test expects variable 'x' to be marked as Borrowed Mutable when calling x.append(4), but it's showing as Owned. Check the inferOwnershipFromTextIO function in src/Fluxus/Analysis/OwnershipInference.hs and the borrowing analysis logic for method calls that mutate the receiver." --yolo

echo ""
echo "=== Shape Analysis Timeout Issue ==="
echo "Error: Test hangs indefinitely during shape analysis"
echo "Test case: Basic shape analysis of primitive values"
echo "Location: test/Test/Fluxus/Analysis/ShapeAnalysis.hs"
echo ""
iflow -p "Shape analysis is timing out/hanging during tests. The test 'infers shape of primitive values' in test/Test/Fluxus/Analysis/ShapeAnalysis.hs hangs indefinitely. Check the analyzeShapeFromText function in src/Fluxus/Analysis/ShapeAnalysis.hs and the runShapeAnalysis function for potential infinite loops or recursion issues. The problem is likely in the shape inference algorithm or state management." --yolo