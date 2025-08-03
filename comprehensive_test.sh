#\!/bin/bash

# Comprehensive test script for Fluxus compiler
# Tests both Go and Python compilation

FLUXUS="./dist-newstyle/build/x86_64-linux/ghc-*/fluxus-*/x/fluxus/build/fluxus/fluxus"
TEST_COUNT=0
SUCCESS_COUNT=0
FAIL_COUNT=0

# Function to test a file
test_file() {
    local file="$1"
    local language="$2"
    local base_name=$(basename "$file" ."$language")
    local output="test_${base_name}"
    
    echo "Testing $file..."
    TEST_COUNT=$((TEST_COUNT + 1))
    
    # Compile
    if [ "$language" = "go" ]; then
        "$FLUXUS" --go "$file" -o "$output" 2>/dev/null
    else
        "$FLUXUS" "$file" -o "$output" 2>/dev/null
    fi
    
    if [ $? -eq 0 ] && [ -f "$output" ]; then
        echo "✓ Compilation successful"
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
        
        # Run and capture output
        echo "Output:"
        ./"$output"
        echo ""
        
        # Clean up
        rm -f "$output"
    else
        echo "✗ Compilation failed"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        echo ""
    fi
}

echo "=== Testing Simple Go Examples ==="
test_file "examples/go/very_basic.go" "go"
test_file "examples/go/hello_simple.go" "go"
test_file "test_simple.go" "go"
test_file "test_fib_simple.go" "go"

echo "=== Testing Simple Python Examples ==="
test_file "examples/python/very_basic.py" "py"
test_file "examples/python/fibonacci.py" "py"
test_file "examples/python/simple_math.py" "py"

echo "=== Test Summary ==="
echo "Total tests: $TEST_COUNT"
echo "Successful: $SUCCESS_COUNT"
echo "Failed: $FAIL_COUNT"
echo "Success rate: $(( SUCCESS_COUNT * 100 / TEST_COUNT ))%"
