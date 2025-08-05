#!/usr/bin/env python3
"""
Simple Python test with unique variable names
"""

print("=== Simple Python Test (Unique Variables) ===")

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

# More basic operations with unique variable names
num1 = 10
num2 = 20
sum_result = num1 + num2
product_result = num1 * num2
division_result = num1 / num2
subtraction_result = num1 - num2
modulus_result = num1 % num2
power_result = num1 ** 2

print("Arithmetic results:")
print("Sum: ", sum_result)
print("Product: ", product_result)
print("Division: ", division_result)
print("Subtraction: ", subtraction_result)
print("Modulus: ", modulus_result)
print("Power: ", power_result)

# Boolean operations
bool1 = True
bool2 = False
print("Boolean tests:")
print("True and False: ", bool1 and bool2)
print("True or False: ", bool1 or bool2)
print("not True: ", not bool1)

# String operations
str1 = "Hello"
str2 = "World"
concat_result = str1 + " " + str2
print("String concatenation: ", concat_result)

print("=== Test Complete ===")