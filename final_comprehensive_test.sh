#\!/bin/bash

# Final comprehensive test for Fluxus compiler
FLUXUS=$(find dist-newstyle/build -name fluxus -type f | head -1)
TEST_COUNT=0
SUCCESS_COUNT=0
FAIL_COUNT=0

test_file() {
    local file="$1"
    local language="$2"
    local base_name=$(basename "$file" ."$language")
    local output="test_${base_name}"
    
    echo "Testing $file..."
    TEST_COUNT=$((TEST_COUNT + 1))
    
    if [ "$language" = "go" ]; then
        "$FLUXUS" --go "$file" -o "$output" 2>/dev/null
    else
        "$FLUXUS" "$file" -o "$output" 2>/dev/null
    fi
    
    if [ $? -eq 0 ] && [ -f "$output" ]; then
        echo "✓ Compilation successful"
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
        echo "Output:"
        ./"$output"
        echo ""
        rm -f "$output"
    else
        echo "✗ Compilation failed"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        echo ""
    fi
}

echo "=== Testing Python Examples ==="
test_file "examples/python/very_basic.py" "py"
test_file "examples/python/fibonacci.py" "py"

echo "=== Testing Go Examples ==="
test_file "examples/go/very_basic.go" "go"
test_file "examples/go/hello_simple.go" "go"
test_file "test_simple.go" "go"

echo "=== Test Summary ==="
echo "Total tests: $TEST_COUNT"
echo "Successful: $SUCCESS_COUNT"
echo "Failed: $FAIL_COUNT"
if [ $TEST_COUNT -gt 0 ]; then
    echo "Success rate: $(( SUCCESS_COUNT * 100 / TEST_COUNT ))%"
fi

if [ $FAIL_COUNT -eq 0 ]; then
    echo "🎉 All tests passed\!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
