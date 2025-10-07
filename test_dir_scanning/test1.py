#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Simple Python test file for Fluxus compiler directory scanning
"""

# Basic data types
int_var = 42
float_var = 3.14159
str_var = "Hello, World!"
bool_var = True

# Container types
list_var = [1, 2, 3, "four", 5.0]
tuple_var = (1, 2, 3, "four")
dict_var = {"name": "Alice", "age": 30, "scores": [85, 90, 95]}
set_var = {1, 2, 3, 3, 2, 1}  # {1, 2, 3}

# Control flow
if int_var > 0:
    print("Positive number")
elif int_var < 0:
    print("Negative number")
else:
    print("Zero")

# for loop
for i in range(5):
    print(f"Loop index: {i}")

# while loop
count = 0
while count < 3:
    print(f"Count: {count}")
    count += 1

# Functions
def greet(name):
    """Simple greeting function"""
    return f"Hello, {name}!"

def power(base, exponent=2):
    return base ** exponent

def sum_all(*args):
    return sum(args)

# Classes
class Person:
    """Person class"""
    species = "Homo sapiens"

    def __init__(self, name, age):
        self.name = name
        self.age = age

    def greet(self):
        return f"Hi, I'm {self.name}."

    def celebrate_birthday(self):
        self.age += 1
        return f"Happy {self.age}th birthday!"

# Test the code
if __name__ == "__main__":
    print("Simple test file executed successfully!")
    print(greet("World"))
    print(f"2^3 = {power(2, 3)}")
    print(f"Sum of 1+2+3 = {sum_all(1, 2, 3)}")

    person = Person("Alice", 30)
    print(person.greet())
    print(person.celebrate_birthday())

    print("All basic functionality works!")