#!/usr/bin/env python3

# Test basic Python syntax constructs

# Basic variable assignment
x = 42
y = "hello"
z = 3.14

# Function definition
def greet(name):
    return f"Hello, {name}!"

# For loop
for i in range(5):
    print(i)

# While loop
i = 0
while i < 3:
    print(i)
    i += 1

# If statement
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")

# Class definition
class Calculator:
    def __init__(self):
        self.result = 0
    
    def add(self, a, b):
        return a + b