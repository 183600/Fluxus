#!/usr/bin/env python3
"""
Simple Python test without f-strings
"""

print("=== Simple Python Test (No f-strings) ===")

# Variables
x = 42
y = 3.14
s = "Hello"
b = True

print("Variables: ", x, ", ", y, ", ", s, ", ", b)

# Basic operations
print("Addition: 5 + 3 = ", 5 + 3)
print("Multiplication: 4 * 7 = ", 4 * 7)

# Simple if statement
if x > 10:
    print("x is greater than 10")

# For loop
for i in range(3):
    print("Loop iteration: ", i)

# Simple function
def greet(name):
    return "Hello, " + name + "!"

print(greet("World"))

# More basic operations
a = 10
b = 20
c = a + b
d = a * b
e = a / b
f = a - b
g = a % b
h = a ** 2

print("Arithmetic results:")
print("Sum: ", c)
print("Product: ", d)
print("Division: ", e)
print("Subtraction: ", f)
print("Modulus: ", g)
print("Power: ", h)

# Boolean operations
print("Boolean tests:")
print("True and False: ", True and False)
print("True or False: ", True or False)
print("not True: ", not True)

print("=== Test Complete ===")