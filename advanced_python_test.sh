#!/bin/bash

# Advanced Python Test Suite for Fluxus Compiler
echo "=== Advanced Python Test Suite ==="

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
    if ! $FLUXUS "${name}.py" -o "${name}_exe" 2>/dev/null; then
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

# Test 1: For loop with range
echo 'for i in range(3):
    print(i)' > padv1.py

run_test "padv1" "0
1
2" "For loop with range"

# Test 2: While loop
echo 'i = 0
while i < 3:
    print(i)
    i += 1' > padv2.py

run_test "padv2" "0
1
2" "While loop"

# Test 3: Function with multiple parameters
echo 'def multiply(a, b, c):
    return a * b * c

result = multiply(2, 3, 4)
print(result)' > padv3.py

run_test "padv3" "24" "Function with multiple parameters"

# Test 4: Recursive function (factorial)
echo 'def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))' > padv4.py

run_test "padv4" "120" "Recursive factorial"

# Test 5: List operations
echo 'numbers = [1, 2, 3, 4, 5]
print(len(numbers))
print(numbers[0])
print(numbers[-1])' > padv5.py

run_test "padv5" "5
1
5" "List operations"

# Test 6: List comprehension
echo 'squares = [x*x for x in range(5)]
print(squares)' > padv6.py

run_test "padv6" "[0, 1, 4, 9, 16]" "List comprehension"

# Test 7: Dictionary operations
echo 'person = {"name": "Alice", "age": 30}
print(person["name"])
print(person["age"])' > padv7.py

run_test "padv7" "Alice
30" "Dictionary operations"

# Test 8: String formatting
echo 'name = "Bob"
age = 25
print(f"Hello, {name}! You are {age} years old.")' > padv8.py

run_test "padv8" "Hello, Bob! You are 25 years old." "F-string formatting"

# Test 9: Class definition
echo 'class Person:
    def __init__(self, name):
        self.name = name
    
    def greet(self):
        return f"Hello, I am {self.name}"

p = Person("Charlie")
print(p.greet())' > padv9.py

run_test "padv9" "Hello, I am Charlie" "Class definition and methods"

# Test 10: Exception handling
echo 'try:
    result = 10 / 2
    print(result)
except ZeroDivisionError:
    print("Cannot divide by zero")' > padv10.py

run_test "padv10" "5.0" "Exception handling"

# Clean up
rm -f padv*.py padv*_exe

echo "=== Advanced Python Test Results ==="
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