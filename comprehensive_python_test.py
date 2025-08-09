#!/usr/bin/env python3
"""
Comprehensive Python feature test for Fluxus compiler
This file tests various Python language features to determine support level
"""

# ===== BASIC VARIABLES AND TYPES =====
print("=== Testing Basic Variables and Types ===")

# Integer
x = 42
print(f"Integer: {x}")

# Float
y = 3.14159
print(f"Float: {y}")

# String
s = "Hello, World!"
print(f"String: {s}")

# Boolean
b = True
print(f"Boolean: {b}")

# None
n = None
print(f"None: {n}")

# Multiple assignment
a, b, c = 1, 2.5, "test"
print(f"Multiple assignment: {a}, {b}, {c}")

# ===== OPERATORS =====
print("\n=== Testing Operators ===")

# Arithmetic operators
print(f"Arithmetic: 5 + 3 = {5 + 3}")
print(f"Arithmetic: 5 - 3 = {5 - 3}")
print(f"Arithmetic: 5 * 3 = {5 * 3}")
print(f"Arithmetic: 5 / 3 = {5 / 3}")
print(f"Arithmetic: 5 // 3 = {5 // 3}")
print(f"Arithmetic: 5 % 3 = {5 % 3}")
print(f"Arithmetic: 5 ** 2 = {5 ** 2}")

# Comparison operators
print(f"Comparison: 5 > 3 = {5 > 3}")
print(f"Comparison: 5 < 3 = {5 < 3}")
print(f"Comparison: 5 == 5 = {5 == 5}")
print(f"Comparison: 5 != 3 = {5 != 3}")

# Logical operators
print(f"Logical: True and False = {True and False}")
print(f"Logical: True or False = {True or False}")
print(f"Logical: not True = {not True}")

# ===== CONTROL FLOW =====
print("\n=== Testing Control Flow ===")

# If/else statements
if x > 0:
    print("If/else: x is positive")
elif x == 0:
    print("If/else: x is zero")
else:
    print("If/else: x is negative")

# While loop
print("While loop:")
count = 0
while count < 3:
    print(f"  Count: {count}")
    count += 1

# For loop with range
print("For loop:")
for i in range(3):
    print(f"  Iteration: {i}")

# For loop with break and continue
print("For loop with break/continue:")
for i in range(5):
    if i == 2:
        continue
    if i == 4:
        break
    print(f"  Processed: {i}")

# ===== BASIC FUNCTIONS =====
print("\n=== Testing Basic Functions ===")

def simple_function():
    return "Hello from simple function"

print(simple_function())

def function_with_params(a, b):
    return a + b

print(f"Function with params: {function_with_params(2, 3)}")

def function_with_local_vars():
    local_var = "local"
    return local_var

print(f"Function with local vars: {function_with_local_vars()}")

# ===== BASIC DATA STRUCTURES =====
print("\n=== Testing Basic Data Structures ===")

# Lists
my_list = [1, 2, 3, 4, 5]
print(f"List: {my_list}")
print(f"List indexing: {my_list[0]}")
print(f"List length: {len(my_list)}")

# List append
my_list.append(6)
print(f"List after append: {my_list}")

# String operations
str1 = "Hello"
str2 = "World"
concatenated = str1 + " " + str2
print(f"String concatenation: {concatenated}")

print("\n=== Basic Test Complete ===")