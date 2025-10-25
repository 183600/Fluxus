#!/bin/bash

echo "=== Debug Go Parser Script ==="

# Create a very simple Go file for testing
cat > debug_simple.go << 'EOF'
package main

func main() {
    fmt.Println("Hello")
}
EOF

echo "Original Go file:"
cat debug_simple.go

echo ""
echo "=== Testing with verbose output ==="

# Try to compile with verbose output
./bin/fluxus --go --verbose -o debug_simple_executable debug_simple.go 2>&1 | tee debug_output.log

echo ""
echo "=== Checking generated C++ code ==="
if [ -f "debug_simple.cpp" ]; then
    echo "Generated C++ code:"
    cat debug_simple.cpp
else
    echo "No C++ code generated!"
fi

echo ""
echo "=== Testing executable ==="
if [ -f "debug_simple_executable" ]; then
    echo "Running executable:"
    ./debug_simple_executable
    echo "Exit code: $?"
else
    echo "No executable generated!"
fi

echo ""
echo "=== Analysis ==="
echo "Checking for parsing issues in the output..."
if grep -q "0 declarations" debug_output.log; then
    echo "❌ ISSUE FOUND: Parser reports 0 declarations"
    echo "This indicates the Go parser is not correctly parsing function declarations"
else
    echo "✅ Parser seems to be working correctly"
fi

if grep -q "Processing Go file with 0 declarations" debug_simple.cpp 2>/dev/null; then
    echo "❌ ISSUE CONFIRMED: Generated code shows 0 declarations"
else
    echo "✅ Generated code shows correct parsing"
fi

# Clean up
rm -f debug_simple.go debug_simple_executable debug_simple.cpp debug_output.log