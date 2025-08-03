#!/bin/bash

# Advanced Go Test Suite for Fluxus Compiler
echo "=== Advanced Go Test Suite ==="

FLUXUS="dist-newstyle/build/x86_64-linux/ghc-9.6.7/fluxus-0.1.0.0/x/fluxus/build/fluxus/fluxus"
PASSED=0
FAILED=0
TOTAL=0

run_test() {
    local name="$1"
    local expected="$2"
    local description="$3"
    
    echo "--- Test: $name ---"
    echo "Description: $description"
    
    TOTAL=$((TOTAL + 1))
    
    # Compile
    if ! $FLUXUS --go "${name}.go" -o "${name}_exe" 2>/dev/null; then
        echo "❌ COMPILATION FAILED"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Run
    if [ -x "${name}_exe" ]; then
        actual=$(./"${name}_exe" 2>&1)
        if [ "$expected" = "$actual" ]; then
            echo "✅ PASSED"
            PASSED=$((PASSED + 1))
        else
            echo "❌ OUTPUT MISMATCH"
            echo "Expected: '$expected'"
            echo "Actual: '$actual'"
            FAILED=$((FAILED + 1))
        fi
    else
        echo "❌ EXECUTABLE NOT CREATED"
        FAILED=$((FAILED + 1))
    fi
    echo
}

# Test 1: For loop
echo 'package main

func main() {
    for i := 0; i < 3; i++ {
        println(i)
    }
}' > adv1.go

run_test "adv1" "0
1
2" "For loop with increment"

# Test 2: While-style for loop
echo 'package main

func main() {
    i := 0
    for i < 3 {
        println(i)
        i++
    }
}' > adv2.go

run_test "adv2" "0
1
2" "While-style for loop"

# Test 3: Nested functions
echo 'package main

func helper(x int) int {
    return x * 2
}

func main() {
    result := helper(5)
    println(result)
}' > adv3.go

run_test "adv3" "10" "Nested function call"

# Test 4: Multiple return values
echo 'package main

func divide(a int, b int) (int, int) {
    return a / b, a % b
}

func main() {
    q, r := divide(7, 3)
    println(q)
    println(r)
}' > adv4.go

run_test "adv4" "2
1" "Multiple return values"

# Test 5: Recursive function (fibonacci)
echo 'package main

func fib(n int) int {
    if n <= 1 {
        return n
    }
    return fib(n-1) + fib(n-2)
}

func main() {
    println(fib(5))
}' > adv5.go

run_test "adv5" "5" "Recursive fibonacci"

# Test 6: String manipulation
echo 'package main

func main() {
    s1 := "Hello"
    s2 := "World"
    println(s1)
    println(s2)
}' > adv6.go

run_test "adv6" "Hello
World" "String variables"

# Test 7: Boolean operations
echo 'package main

func main() {
    x := 5
    y := 10
    if x < y && y > 0 {
        println("true")
    } else {
        println("false")
    }
}' > adv7.go

run_test "adv7" "true" "Boolean operations"

# Test 8: Switch statement
echo 'package main

func main() {
    x := 2
    switch x {
    case 1:
        println("one")
    case 2:
        println("two")
    default:
        println("other")
    }
}' > adv8.go

run_test "adv8" "two" "Switch statement"

# Test 9: Arrays
echo 'package main

func main() {
    arr := [3]int{1, 2, 3}
    println(arr[0])
    println(arr[1])
    println(arr[2])
}' > adv9.go

run_test "adv9" "1
2
3" "Array operations"

# Test 10: Slices
echo 'package main

func main() {
    slice := []int{1, 2, 3, 4, 5}
    println(len(slice))
    println(slice[0])
    println(slice[4])
}' > adv10.go

run_test "adv10" "5
1
5" "Slice operations"

# Clean up
rm -f adv*.go adv*_exe

echo "=== Advanced Go Test Results ==="
echo "Total: $TOTAL, Passed: $PASSED, Failed: $FAILED"
if [ $TOTAL -gt 0 ]; then
    echo "Success rate: $((PASSED * 100 / TOTAL))%"
else
    echo "Success rate: 0%"
fi

if [ $FAILED -eq 0 ]; then
    echo "🎉 All advanced tests passed!"
    exit 0
else
    echo "⚠️  Some advanced tests failed"
    exit 1
fi