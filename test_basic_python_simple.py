#!/usr/bin/env python3
"""
Simple test for basic Python language features
"""

# 1. Variables and Data Types
print("=== Testing Variables and Data Types ===")

# Integers
x = 42
y = -17
z = 0
print(f"Integers: {x}, {y}, {z}")

# Floats
a = 3.14
b = -2.5
c = 0.0
print(f"Floats: {a}, {b}, {c}")

# Strings
s1 = "Hello, World!"
s2 = 'Python'
print(f"Strings: {s1}, {s2}")

# Booleans
t = True
f = False
print(f"Booleans: {t}, {f}")

# None
n = None
print(f"None: {n}")

# 2. Basic Operations
print("\n=== Testing Basic Operations ===")

# Arithmetic operations
print(f"Addition: 5 + 3 = {5 + 3}")
print(f"Subtraction: 5 - 3 = {5 - 3}")
print(f"Multiplication: 5 * 3 = {5 * 3}")
print(f"Division: 5 / 3 = {5 / 3}")
print(f"Floor division: 5 // 3 = {5 // 3}")
print(f"Modulus: 5 % 3 = {5 % 3}")
print(f"Exponentiation: 5 ** 3 = {5 ** 3}")

# Comparison operations
print(f"Equal: 5 == 5 = {5 == 5}")
print(f"Not equal: 5 != 3 = {5 != 3}")
print(f"Greater than: 5 > 3 = {5 > 3}")
print(f"Less than: 5 < 3 = {5 < 3}")
print(f"Greater or equal: 5 >= 5 = {5 >= 5}")
print(f"Less or equal: 5 <= 3 = {5 <= 3}")

# Logical operations
print(f"AND: True and False = {True and False}")
print(f"OR: True or False = {True or False}")
print(f"NOT: not True = {not True}")

# 3. Control Flow
print("\n=== Testing Control Flow ===")

# If/else (no elif for now)
age = 25
if age < 18:
    print("Minor")
else:
    print("Adult")

# While loop
count = 0
while count < 3:
    print(f"While loop iteration: {count}")
    count += 1

# For loop with range
for i in range(3):
    print(f"For loop iteration: {i}")

# For loop with break/continue
for i in range(5):
    if i == 2:
        continue
    if i == 4:
        break
    print(f"For loop with break/continue: {i}")

# 4. Functions
print("\n=== Testing Functions ===")

# Basic function
def greet(name):
    return f"Hello, {name}!"

print(greet("World"))

# Function with default parameters
def power(base, exponent=2):
    return base ** exponent

print(f"Power with default: {power(5)}")
print(f"Power with custom: {power(5, 3)}")

# Function with variable arguments
def sum_all(*args):
    return sum(args)

print(f"Sum all: {sum_all(1, 2, 3, 4, 5)}")

# Function with keyword arguments
def person_info(name, age, **kwargs):
    info = f"Name: {name}, Age: {age}"
    for key, value in kwargs.items():
        info += f", {key}: {value}"
    return info

print(f"Person info: {person_info('Alice', 30, city='New York', job='Engineer')}")

# Lambda function
multiply = lambda x, y: x * y
print(f"Lambda multiply: {multiply(4, 5)}")

print("\n=== Basic Features Test Complete ===")