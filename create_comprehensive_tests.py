#!/usr/bin/env python3

# Test more complex expressions to verify the parser is working correctly
test_cases = [
    # Basic function calls
    "print(42)",
    "print(\"Hello\")", 
    "print(True)",
    "print(False)",
    "print(None)",
    
    # Variable assignments
    "x = 42",
    "y = \"world\"",
    "z = True",
    
    # Function calls with multiple arguments
    "print(x, y)",
    "max(1, 2, 3)",
    "min(4, 5, 6)",
    
    # Nested function calls
    "print(len(\"hello\"))",
    "print(str(42))",
    
    # Mathematical expressions
    "1 + 2",
    "3 * 4",
    "10 / 2",
    
    # Complex expressions
    "print(1 + 2 * 3)",
    "len(\"hello\" + \" \" + \"world\")",
]

print("Creating test cases...")
for i, test in enumerate(test_cases):
    print(f"Test {i+1}: {test}")
    try:
        with open(f"test_{i+1}.py", "w") as f:
            f.write(test + "\n")
        print(f"  ✓ Created test_{i+1}.py")
    except Exception as e:
        print(f"  ✗ Error: {e}")

print("\nTest cases created successfully!")