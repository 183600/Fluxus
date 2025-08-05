#!/usr/bin/env python3
# Test 4: Basic language features - Functions
print("=== Testing Functions ===")

# Basic function
def greet(name):
    return f"Hello, {name}!"

print("Basic function:")
print(greet("World"))

# Function with default parameters
def power(base, exponent=2):
    return base ** exponent

print("\nFunction with default parameters:")
print(f"power(3) = {power(3)}")
print(f"power(3, 3) = {power(3, 3)}")

# Function with variable arguments
def sum_all(*args):
    return sum(args)

print("\nFunction with variable arguments:")
print(f"sum_all(1, 2, 3) = {sum_all(1, 2, 3)}")
print(f"sum_all(10, 20, 30, 40, 50) = {sum_all(10, 20, 30, 40, 50)}")

# Function with keyword arguments
def create_person(name, age, **kwargs):
    person = {"name": name, "age": age}
    person.update(kwargs)
    return person

print("\nFunction with keyword arguments:")
print(f"create_person('Alice', 30, city='New York', job='Engineer') = {create_person('Alice', 30, city='New York', job='Engineer')}")

# Recursive function
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print("\nRecursive function:")
print(f"factorial(5) = {factorial(5)}")

# Function with no return value
def print_message(message):
    print(f"Message: {message}")

print("\nFunction with no return value:")
result = print_message("Test message")
print(f"Return value: {result}")

print("=== Functions Test Complete ===\n")