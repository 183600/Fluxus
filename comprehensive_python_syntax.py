#!/usr/bin/env python3
"""
Comprehensive Python syntax demonstration file
Contains all major Python language features
"""

# Import statements
import sys
import os
from typing import List, Dict, Optional, Union, Any, Callable, Iterator, Generator
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
import asyncio
import threading
import time
import json

# Constants and variables
CONSTANT_VALUE = 42
PI = 3.14159
STRING_CONSTANT = "Hello, World!"

# Basic data types
integer_var = 42
float_var = 3.14
string_var = "Hello Python"
bool_var = True
none_var = None

# Collection types
list_var = [1, 2, 3, 4, 5]
tuple_var = (1, 2, 3, 4, 5)
set_var = {1, 2, 3, 4, 5}
dict_var = {"key1": "value1", "key2": "value2"}

# List comprehensions
squares = [x**2 for x in range(10)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]
matrix = [[i*j for j in range(3)] for i in range(3)]

# Dictionary comprehensions
square_dict = {x: x**2 for x in range(5)}

# Set comprehensions
unique_squares = {x**2 for x in range(10)}

# Generator expressions
gen = (x**2 for x in range(10))

# Functions
def basic_function(x: int, y: str = "default") -> str:
    """Basic function with type hints"""
    return f"{y}: {x}"

def function_with_args(*args, **kwargs):
    """Function with variable arguments"""
    print(f"args: {args}")
    print(f"kwargs: {kwargs}")

def generator_function(n: int) -> Generator[int, None, None]:
    """Generator function"""
    for i in range(n):
        yield i

# Lambda functions
lambda_func = lambda x, y: x + y
lambda_with_condition = lambda x: "even" if x % 2 == 0 else "odd"

# Class definitions
class BasicClass:
    """Basic class with various features"""
    
    class_variable = "I'm a class variable"
    
    def __init__(self, name: str, age: int = 0):
        self.name = name
        self.age = age
        self._private_var = "private"
    
    def instance_method(self):
        return f"Hello, I'm {self.name}"
    
    @classmethod
    def class_method(cls):
        return f"Class method called on {cls.__name__}"
    
    @staticmethod
    def static_method():
        return "Static method called"
    
    @property
    def name_property(self):
        return self.name.upper()
    
    def __str__(self):
        return f"BasicClass(name='{self.name}', age={self.age})"
    
    def __repr__(self):
        return self.__str__()

# Inheritance
class ChildClass(BasicClass):
    """Child class demonstrating inheritance"""
    
    def __init__(self, name: str, age: int, specialty: str):
        super().__init__(name, age)
        self.specialty = specialty
    
    def child_method(self):
        return f"{self.name} specializes in {self.specialty}"

# Abstract base class
class AbstractShape(ABC):
    """Abstract base class"""
    
    @abstractmethod
    def area(self) -> float:
        pass
    
    @abstractmethod
    def perimeter(self) -> float:
        pass

class Rectangle(AbstractShape):
    """Concrete implementation of abstract class"""
    
    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height
    
    def area(self) -> float:
        return self.width * self.height
    
    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

# Data classes
@dataclass
class Person:
    """Data class example"""
    name: str
    age: int
    email: Optional[str] = None

# Enums
class Color(Enum):
    """Enum example"""
    RED = 1
    GREEN = 2
    BLUE = 3

# Exception handling
def demonstrate_exceptions():
    """Demonstrate exception handling"""
    try:
        result = 10 / 0
    except ZeroDivisionError as e:
        print(f"Caught ZeroDivisionError: {e}")
    except Exception as e:
        print(f"Caught general exception: {e}")
    else:
        print("No exception occurred")
    finally:
        print("Finally block executed")

# Context managers
class CustomContextManager:
    """Custom context manager"""
    
    def __enter__(self):
        print("Entering context")
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Exiting context")
        return False

def use_context_manager():
    """Use context manager"""
    with CustomContextManager() as cm:
        print("Inside context")

# Decorators
def simple_decorator(func):
    """Simple decorator"""
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__}")
        result = func(*args, **kwargs)
        print(f"Finished calling {func.__name__}")
        return result
    return wrapper

@simple_decorator
def decorated_function():
    """Function with decorator"""
    print("Decorated function executed")

# Async functions
async def async_function():
    """Async function"""
    print("Async function started")
    await asyncio.sleep(0.1)
    print("Async function completed")
    return "async result"

async def async_generator():
    """Async generator"""
    for i in range(3):
        await asyncio.sleep(0.1)
        yield i

# Threading
def worker_function(name: str, delay: float):
    """Worker function for threading"""
    print(f"Worker {name} started")
    time.sleep(delay)
    print(f"Worker {name} finished")

def demonstrate_threading():
    """Demonstrate threading"""
    threads = []
    for i in range(3):
        thread = threading.Thread(target=worker_function, args=(f"Thread-{i}", 0.1))
        threads.append(thread)
        thread.start()
    
    for thread in threads:
        thread.join()

# File I/O operations
def file_operations():
    """Demonstrate file operations"""
    # Write to file
    with open("test_file.txt", "w") as f:
        f.write("Hello, file!\n")
        f.write("Second line\n")
    
    # Read from file
    with open("test_file.txt", "r") as f:
        content = f.read()
        print(f"File content: {content}")
    
    # Clean up
    os.remove("test_file.txt")

# JSON operations
def json_operations():
    """Demonstrate JSON operations"""
    data = {
        "name": "John",
        "age": 30,
        "hobbies": ["reading", "coding", "gaming"]
    }
    
    # Convert to JSON
    json_str = json.dumps(data, indent=2)
    print(f"JSON string: {json_str}")
    
    # Parse JSON
    parsed_data = json.loads(json_str)
    print(f"Parsed data: {parsed_data}")

# Regular expressions
import re

def regex_operations():
    """Demonstrate regular expressions"""
    pattern = r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b"
    text = "Contact us at support@example.com or sales@company.org"
    
    matches = re.findall(pattern, text)
    print(f"Email matches: {matches}")

# Functional programming
def functional_programming():
    """Demonstrate functional programming concepts"""
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    
    # Map
    squared = list(map(lambda x: x**2, numbers))
    print(f"Squared numbers: {squared}")
    
    # Filter
    evens = list(filter(lambda x: x % 2 == 0, numbers))
    print(f"Even numbers: {evens}")
    
    # Reduce
    from functools import reduce
    sum_all = reduce(lambda x, y: x + y, numbers)
    print(f"Sum of all numbers: {sum_all}")

# Operator overloading
class Vector:
    """Class with operator overloading"""
    
    def __init__(self, x: float, y: float):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __mul__(self, scalar: float):
        return Vector(self.x * scalar, self.y * scalar)
    
    def __str__(self):
        return f"Vector({self.x}, {self.y})"

# Multiple inheritance
class MixinA:
    def method_a(self):
        return "MixinA method"

class MixinB:
    def method_b(self):
        return "MixinB method"

class MultipleInheritance(MixinA, MixinB):
    pass

# Metaclasses
class MyMeta(type):
    """Custom metaclass"""
    
    def __new__(cls, name, bases, namespace):
        print(f"Creating class {name}")
        return super().__new__(cls, name, bases, namespace)

class MetaClassExample(metaclass=MyMeta):
    pass

# Global and nonlocal
def demonstrate_scope():
    """Demonstrate global and nonlocal scope"""
    global_var = "I'm global"
    
    def outer_function():
        outer_var = "I'm outer"
        
        def inner_function():
            nonlocal outer_var
            inner_var = "I'm inner"
            outer_var = "Modified by inner"
            return inner_var
        
        result = inner_function()
        print(f"Outer var after inner call: {outer_var}")
        return result
    
    return outer_function()

# Walrus operator (Python 3.8+)
def walrus_operator_example():
    """Demonstrate walrus operator"""
    numbers = [1, 2, 3, 4, 5]
    
    # Traditional way
    n = len(numbers)
    if n > 3:
        print(f"List has {n} elements")
    
    # With walrus operator
    if (n := len(numbers)) > 3:
        print(f"List has {n} elements (walrus)")

# Pattern matching (Python 3.10+)
def pattern_matching_example(value):
    """Demonstrate pattern matching"""
    match value:
        case 1:
            return "One"
        case 2:
            return "Two"
        case 3:
            return "Three"
        case [x, y]:
            return f"List with {x} and {y}"
        case {"key": key_value}:
            return f"Dict with key: {key_value}"
        case _:
            return "Something else"

# Main execution function
def main():
    """Main function to demonstrate all features"""
    print("=== Comprehensive Python Syntax Demo ===")
    
    # Basic operations
    print(f"Basic variables: {integer_var}, {float_var}, {string_var}")
    print(f"Collections: {list_var}, {dict_var}")
    print(f"List comprehension: {squares}")
    print(f"Function result: {basic_function(42, 'test')}")
    
    # Class operations
    obj = BasicClass("Test Object", 25)
    print(f"Object: {obj}")
    print(f"Instance method: {obj.instance_method()}")
    print(f"Class method: {BasicClass.class_method()}")
    print(f"Static method: {BasicClass.static_method()}")
    
    # Inheritance
    child = ChildClass("Child", 10, "Python")
    print(f"Child: {child.child_method()}")
    
    # Abstract class
    rect = Rectangle(5, 3)
    print(f"Rectangle area: {rect.area()}")
    print(f"Rectangle perimeter: {rect.perimeter()}")
    
    # Data class
    person = Person("Alice", 30, "alice@example.com")
    print(f"Person: {person}")
    
    # Enum
    print(f"Color enum: {Color.RED}")
    
    # Exception handling
    demonstrate_exceptions()
    
    # Context manager
    use_context_manager()
    
    # Decorator
    decorated_function()
    
    # Threading
    demonstrate_threading()
    
    # File operations
    file_operations()
    
    # JSON operations
    json_operations()
    
    # Regex operations
    regex_operations()
    
    # Functional programming
    functional_programming()
    
    # Operator overloading
    v1 = Vector(1, 2)
    v2 = Vector(3, 4)
    v3 = v1 + v2
    print(f"Vector addition: {v3}")
    
    # Multiple inheritance
    multi = MultipleInheritance()
    print(f"Multiple inheritance: {multi.method_a()}, {multi.method_b()}")
    
    # Scope demonstration
    scope_result = demonstrate_scope()
    print(f"Scope result: {scope_result}")
    
    # Walrus operator
    walrus_operator_example()
    
    # Pattern matching
    print(f"Pattern matching 1: {pattern_matching_example(1)}")
    print(f"Pattern matching [1,2]: {pattern_matching_example([1, 2])}")
    print(f"Pattern matching dict: {pattern_matching_example({'key': 'value'})}")
    
    # Async operations (run in event loop)
    async def run_async():
        result = await async_function()
        print(f"Async result: {result}")
        
        async for item in async_generator():
            print(f"Async generator item: {item}")
    
    asyncio.run(run_async())
    
    print("=== Demo completed ===")

if __name__ == "__main__":
    main()