#!/bin/bash

echo "=== Go Language Support Comprehensive Test ==="
echo "Testing Fluxus compiler Go language support"
echo "=============================================="

# Check if Fluxus is built
FLUXUS_BIN="./dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
if [ ! -f "$FLUXUS_BIN" ]; then
    echo "Building Fluxus compiler..."
    cabal build
    if [ $? -ne 0 ]; then
        echo "Error: Failed to build Fluxus compiler"
        exit 1
    fi
fi

# Test function
test_go_file() {
    local test_file=$1
    local test_name=$2
    
    echo ""
    echo "Testing $test_name..."
    echo "File: $test_file"
    echo "----------------------------------------"
    
    # Try to compile
    $FLUXUS_BIN --go -O2 "$test_file" -o test_output
    
    if [ $? -eq 0 ]; then
        echo "✓ Compilation successful"
        
        # Try to run the compiled program
        if [ -f "test_output" ]; then
            ./test_output
            if [ $? -eq 0 ]; then
                echo "✓ Execution successful"
                rm -f test_output
                return 0
            else
                echo "✗ Execution failed"
                rm -f test_output
                return 1
            fi
        else
            echo "✗ No output file generated"
            return 1
        fi
    else
        echo "✗ Compilation failed"
        return 1
    fi
}

# Test results counter
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Run basic test
echo "Running basic Go feature tests..."
test_go_file "comprehensive_go_test.go" "Basic Go Features"
if [ $? -eq 0 ]; then
    ((PASSED_TESTS++))
else
    ((FAILED_TESTS++))
fi
((TOTAL_TESTS++))

# Run advanced test
echo ""
echo "Running advanced Go feature tests..."
test_go_file "advanced_go_test.go" "Advanced Go Features"
if [ $? -eq 0 ]; then
    ((PASSED_TESTS++))
else
    ((FAILED_TESTS++))
fi
((TOTAL_TESTS++))

# Test individual Go examples
echo ""
echo "Testing individual Go examples..."

for example in examples/go/*.go; do
    if [ -f "$example" ]; then
        filename=$(basename "$example")
        test_name="Example: $filename"
        
        echo ""
        test_go_file "$example" "$test_name"
        if [ $? -eq 0 ]; then
            ((PASSED_TESTS++))
        else
            ((FAILED_TESTS++))
        fi
        ((TOTAL_TESTS++))
    fi
done

# Generate report
echo ""
echo "=============================================="
echo "GO LANGUAGE SUPPORT TEST REPORT"
echo "=============================================="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo ""
if [ $FAILED_TESTS -eq 0 ]; then
    echo "🎉 All tests passed! Go language support appears to be working well."
else
    echo "⚠️  Some tests failed. Go language support may need improvements."
fi

# Calculate success rate
if [ $TOTAL_TESTS -gt 0 ]; then
    SUCCESS_RATE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "Success rate: $SUCCESS_RATE%"
fi

echo ""
echo "=============================================="

# Test specific language features
echo ""
echo "Testing specific Go language features..."

# Test 1: Basic types and variables
echo "1. Testing basic types and variables..."
cat > test_basic_types.go << 'EOF'
package main

import "fmt"

func main() {
    var x int = 42
    y := 3.14
    s := "Hello"
    b := true
    
    fmt.Printf("%d %f %s %t\n", x, y, s, b)
}
EOF

test_go_file "test_basic_types.go" "Basic Types"
rm -f test_basic_types.go

# Test 2: Control flow
echo ""
echo "2. Testing control flow..."
cat > test_control_flow.go << 'EOF'
package main

import "fmt"

func main() {
    // If statement
    x := 10
    if x > 5 {
        fmt.Println("x > 5")
    }
    
    // For loop
    for i := 0; i < 3; i++ {
        fmt.Printf("%d ", i)
    }
    fmt.Println()
}
EOF

test_go_file "test_control_flow.go" "Control Flow"
rm -f test_control_flow.go

# Test 3: Functions
echo ""
echo "3. Testing functions..."
cat > test_functions.go << 'EOF'
package main

import "fmt"

func add(a, b int) int {
    return a + b
}

func main() {
    result := add(5, 3)
    fmt.Printf("5 + 3 = %d\n", result)
}
EOF

test_go_file "test_functions.go" "Functions"
rm -f test_functions.go

# Test 4: Structs
echo ""
echo "4. Testing structs..."
cat > test_structs.go << 'EOF'
package main

import "fmt"

type Person struct {
    Name string
    Age  int
}

func main() {
    p := Person{Name: "Alice", Age: 30}
    fmt.Printf("%s is %d years old\n", p.Name, p.Age)
}
EOF

test_go_file "test_structs.go" "Structs"
rm -f test_structs.go

# Test 5: Slices
echo ""
echo "5. Testing slices..."
cat > test_slices.go << 'EOF'
package main

import "fmt"

func main() {
    s := []int{1, 2, 3, 4, 5}
    s = append(s, 6)
    
    for i, v := range s {
        fmt.Printf("%d: %d\n", i, v)
    }
}
EOF

test_go_file "test_slices.go" "Slices"
rm -f test_slices.go

# Test 6: Maps
echo ""
echo "6. Testing maps..."
cat > test_maps.go << 'EOF'
package main

import "fmt"

func main() {
    m := make(map[string]int)
    m["one"] = 1
    m["two"] = 2
    
    for k, v := range m {
        fmt.Printf("%s: %d\n", k, v)
    }
}
EOF

test_go_file "test_maps.go" "Maps"
rm -f test_maps.go

# Test 7: Interfaces
echo ""
echo "7. Testing interfaces..."
cat > test_interfaces.go << 'EOF'
package main

import "fmt"

type Shape interface {
    Area() float64
}

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func main() {
    var s Shape = Rectangle{Width: 3, Height: 4}
    fmt.Printf("Area: %f\n", s.Area())
}
EOF

test_go_file "test_interfaces.go" "Interfaces"
rm -f test_interfaces.go

# Test 8: Concurrency (basic)
echo ""
echo "8. Testing basic concurrency..."
cat > test_concurrency.go << 'EOF'
package main

import "fmt"

func main() {
    ch := make(chan int)
    
    go func() {
        ch <- 42
    }()
    
    value := <-ch
    fmt.Printf("Received: %d\n", value)
}
EOF

test_go_file "test_concurrency.go" "Basic Concurrency"
rm -f test_concurrency.go

# Test 9: Pointers
echo ""
echo "9. Testing pointers..."
cat > test_pointers.go << 'EOF'
package main

import "fmt"

func main() {
    x := 42
    p := &x
    *p = 21
    
    fmt.Printf("x = %d\n", x)
}
EOF

test_go_file "test_pointers.go" "Pointers"
rm -f test_pointers.go

# Test 10: Error handling
echo ""
echo "10. Testing error handling..."
cat > test_error_handling.go << 'EOF'
package main

import (
    "fmt"
    "errors"
)

func divide(a, b int) (int, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

func main() {
    result, err := divide(10, 2)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
    } else {
        fmt.Printf("Result: %d\n", result)
    }
}
EOF

test_go_file "test_error_handling.go" "Error Handling"
rm -f test_error_handling.go

echo ""
echo "=============================================="
echo "Individual feature tests completed."
echo "=============================================="

# Clean up
rm -f test_output

echo ""
echo "Test completed successfully!"