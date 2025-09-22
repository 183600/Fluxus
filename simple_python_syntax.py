#!/usr/bin/env python3
"""
Simple Python syntax demonstration file
"""

# Basic variables
integer_var = 42
float_var = 3.14
string_var = "Hello Python"
bool_var = True
none_var = None

# Collection types
list_var = [1, 2, 3, 4, 5]
tuple_var = (1, 2, 3, 4, 5)
dict_var = {"key1": "value1", "key2": "value2"}

# List comprehensions
squares = [x**2 for x in range(10)]

# Basic function
def basic_function(x, y="default"):
    return f"{y}: {x}"

# Class definition
class BasicClass:
    class_variable = "I'm a class variable"
    
    def __init__(self, name, age=0):
        self.name = name
        self.age = age
    
    def instance_method(self):
        return f"Hello, I'm {self.name}"
    
    @classmethod
    def class_method(cls):
        return f"Class method called"
    
    @staticmethod
    def static_method():
        return "Static method called"

# Main execution function
def main():
    print("=== Simple Python Syntax Demo ===")
    
    # Basic operations
    print(f"Basic variables: {integer_var}, {float_var}, {string_var}")
    print(f"Collections: {list_var}, {dict_var}")
    print(f"List comprehension: {squares}")
    print(f"Function result: {basic_function(42, 'test')}")
    
    # Class operations
    obj = BasicClass("Test Object", 25)
    print(f"Object: {obj.name}, {obj.age}")
    print(f"Instance method: {obj.instance_method()}")
    print(f"Class method: {BasicClass.class_method()}")
    print(f"Static method: {BasicClass.static_method()}")
    
    print("=== Demo completed ===")

if __name__ == "__main__":
    main()