#!/usr/bin/env python3

# Test some more complex expressions to verify the parser fix
test_cases = [
    "print(42)",
    "print(\"Hello\")",
    "print(True)",
    "print(False)",
    "print(None)",
    "x = 42",
    "y = \"world\"",
    "print(x, y)",
    "len(\"hello\")",
    "max(1, 2, 3)",
    "sum([1, 2, 3])"
]

for i, test in enumerate(test_cases):
    print(f"Test {i+1}: {test}")
    try:
        # Create a simple Python file to test
        with open(f"test_{i+1}.py", "w") as f:
            f.write(test + "\n")
        print(f"  ✓ Created test file")
    except Exception as e:
        print(f"  ✗ Error: {e}")