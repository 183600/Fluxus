#!/usr/bin/env python3
"""
Simple Python test for basic features
"""

print("=== Simple Python Test ===")

# Variables
x = 42
y = 3.14
s = "Hello"
b = True

print(f"Variables: {x}, {y}, {s}, {b}")

# Basic operations
print(f"Addition: 5 + 3 = {5 + 3}")
print(f"Multiplication: 4 * 7 = {4 * 7}")

# Simple if statement
if x > 10:
    print("x is greater than 10")

# For loop
for i in range(3):
    print(f"Loop iteration: {i}")

# Simple function
def greet(name):
    return f"Hello, {name}!"

print(greet("World"))

print("=== Test Complete ===")