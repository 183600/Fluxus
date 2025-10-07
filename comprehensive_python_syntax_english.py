#!/usr/bin/env python3
"""
Comprehensive Python syntax demonstration
Contains all major Python syntax features
"""

# 1. Basic syntax and data types
import sys
import os
from typing import List, Dict, Any, Optional, Union, Callable, Tuple, Set
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum, auto, IntEnum, Flag
import asyncio
import threading
import time
import json
import math
import random
import itertools
import functools
import collections
import contextlib
import weakref
import gc
import re
import struct
import array
import heapq
import bisect
import copy
import pickle
import base64
import hashlib
import hmac
import secrets
import uuid
import datetime
import calendar
import zoneinfo
import decimal
import fractions
import statistics
import pathlib
import urllib.parse
import xml.etree.ElementTree as ET
import sqlite3
import csv
import io
import logging
import warnings
import traceback
import inspect
import types
import importlib
import unittest
import doctest
from contextvars import ContextVar
from collections import ChainMap, Counter, OrderedDict, defaultdict, deque

# 2. Enum classes
class Color(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3
    
    def __str__(self):
        return f"Color({self.name})"
    
    @property
    def is_warm(self):
        return self in (Color.RED, Color.GREEN)

class Status(IntEnum):
    PENDING = 0
    ACTIVE = 1
    COMPLETED = 2
    FAILED = 3

class Permission(Flag):
    READ = 1
    WRITE = 2
    EXECUTE = 4
    ALL = READ | WRITE | EXECUTE

# 3. Data classes
@dataclass
class Person:
    name: str
    age: int
    email: Optional[str] = None
    scores: List[float] = None
    
    def __post_init__(self):
        if self.scores is None:
            self.scores = []
    
    @property
    def average_score(self) -> float:
        return sum(self.scores) / len(self.scores) if self.scores else 0.0
    
    def add_score(self, score: float):
        self.scores.append(score)

# 4. Abstract base class
class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass
    
    @abstractmethod
    def perimeter(self) -> float:
        pass
    
    def describe(self) -> str:
        return f"This is a shape with area {self.area()} and perimeter {self.perimeter()}"

# 5. Concrete class implementation
class Rectangle(Shape):
    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height
    
    def area(self) -> float:
        return self.width * self.height
    
    def perimeter(self) -> float:
        return 2 * (self.width + self.height)
    
    def __str__(self) -> str:
        return f"Rectangle({self.width}, {self.height})"
    
    def __repr__(self) -> str:
        return f"Rectangle(width={self.width}, height={self.height})"
    
    def __eq__(self, other) -> bool:
        if isinstance(other, Rectangle):
            return self.width == other.width and self.height == other.height
        return False
    
    def __hash__(self) -> int:
        return hash((self.width, self.height))

class Circle(Shape):
    def __init__(self, radius: float):
        self.radius = radius
    
    def area(self) -> float:
        return math.pi * self.radius ** 2
    
    def perimeter(self) -> float:
        return 2 * math.pi * self.radius

# 6. Metaclasses and decorators
class SingletonMeta(type):
    _instances = {}
    _lock = threading.Lock()
    
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            with cls._lock:
                if cls not in cls._instances:
                    cls._instances[cls] = super().__call__(*args, **kwargs)
        return cls._instances[cls]

def timer_decorator(func: Callable) -> Callable:
    """Timer decorator"""
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        end_time = time.time()
        print(f"{func.__name__} took {end_time - start_time:.4f} seconds")
        return result
    return wrapper

def cache_decorator(func: Callable) -> Callable:
    """Cache decorator"""
    cache = {}
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        key = str(args) + str(sorted(kwargs.items()))
        if key not in cache:
            cache[key] = func(*args, **kwargs)
        return cache[key]
    return wrapper

# 7. Class using metaclass
class Database(metaclass=SingletonMeta):
    def __init__(self):
        self.connection = "database_connection"
        self.data = {}
    
    def query(self, sql: str) -> List[Dict]:
        return [{"id": 1, "name": "test"}]
    
    def insert(self, table: str, data: Dict):
        if table not in self.data:
            self.data[table] = []
        self.data[table].append(data)

# 8. Generators and iterators
class Fibonacci:
    """Fibonacci sequence iterator"""
    def __init__(self, max_n: int):
        self.max_n = max_n
        self.n = 0
        self.a, self.b = 0, 1
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.n >= self.max_n:
            raise StopIteration
        
        result = self.a
        self.a, self.b = self.b, self.a + self.b
        self.n += 1
        return result

def fibonacci_generator(n: int):
    """Fibonacci sequence generator"""
    a, b = 0, 1
    for _ in range(n):
        yield a
        a, b = b, a + b

# 9. Async functions
async def async_fetch_data(url: str) -> str:
    """Async fetch data"""
    await asyncio.sleep(0.1)  # Simulate network delay
    return f"Data from {url}"

async def async_process_data(data: str) -> str:
    """Async process data"""
    await asyncio.sleep(0.05)  # Simulate processing time
    return f"Processed: {data}"

# 10. Context managers
class FileManager:
    """File context manager"""
    def __init__(self, filename: str, mode: str):
        self.filename = filename
        self.mode = mode
        self.file = None
    
    def __enter__(self):
        self.file = open(self.filename, self.mode)
        return self.file
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.file:
            self.file.close()
        return False  # Don't suppress exceptions

# 11. Functional programming tools
class FunctionalUtils:
    @staticmethod
    def compose(*functions):
        """Function composition"""
        def composed(x):
            result = x
            for func in reversed(functions):
                result = func(result)
            return result
        return composed
    
    @staticmethod
    def curry(func):
        """Currying"""
        @functools.wraps(func)
        def curried(*args, **kwargs):
            if len(args) + len(kwargs) >= func.__code__.co_argcount:
                return func(*args, **kwargs)
            return lambda *more_args, **more_kwargs: curried(*(args + more_args), **{**kwargs, **more_kwargs})
        return curried
    
    @staticmethod
    def memoize(func):
        """Memoization"""
        cache = {}
        @functools.wraps(func)
        def memoized(*args, **kwargs):
            key = str(args) + str(sorted(kwargs.items()))
            if key not in cache:
                cache[key] = func(*args, **kwargs)
            return cache[key]
        return memoized

# 12. Advanced data structures
class BinaryTree:
    """Binary tree"""
    def __init__(self, value: Any):
        self.value = value
        self.left = None
        self.right = None
    
    def insert(self, value: Any):
        if value < self.value:
            if self.left is None:
                self.left = BinaryTree(value)
            else:
                self.left.insert(value)
        else:
            if self.right is None:
                self.right = BinaryTree(value)
            else:
                self.right.insert(value)
    
    def inorder_traversal(self) -> List[Any]:
        result = []
        if self.left:
            result.extend(self.left.inorder_traversal())
        result.append(self.value)
        if self.right:
            result.extend(self.right.inorder_traversal())
        return result

# 13. Regular expressions and string processing
def advanced_string_processing():
    """Advanced string processing"""
    text = "The quick brown fox jumps over the lazy dog"
    
    # Regular expression matching
    pattern = r"\b\w{5}\b"
    matches = re.findall(pattern, text)
    print(f"5-letter words: {matches}")
    
    # String formatting
    name = "Alice"
    age = 30
    formatted = f"My name is {name} and I am {age} years old"
    
    return {
        "matches": matches,
        "formatted": formatted
    }

# 14. Exception handling
def divide_with_exception_handling(a: float, b: float) -> Union[float, None]:
    """Division with exception handling"""
    try:
        result = a / b
    except ZeroDivisionError:
        print("Error: Division by zero")
        return None
    except TypeError:
        print("Error: Invalid input types")
        return None
    else:
        print("Division successful")
        return result
    finally:
        print("Division operation completed")

class CustomError(Exception):
    """Custom exception"""
    def __init__(self, message, code):
        super().__init__(message)
        self.code = code

# 15. List, dict, set comprehensions
def comprehensions_demo():
    """Comprehensions demo"""
    # List comprehension
    squares = [x**2 for x in range(10) if x % 2 == 0]
    
    # Dict comprehension
    square_dict = {x: x**2 for x in range(5)}
    
    # Set comprehension
    unique_squares = {x**2 for x in range(-5, 6)}
    
    # Generator expression
    sum_squares = sum(x**2 for x in range(100))
    
    return {
        "squares": squares,
        "square_dict": square_dict,
        "unique_squares": unique_squares,
        "sum_squares": sum_squares
    }

# 16. Decorator classes
class CountCalls:
    """Count calls decorator class"""
    def __init__(self, func):
        self.func = func
        self.count = 0
        functools.update_wrapper(self, func)
    
    def __call__(self, *args, **kwargs):
        self.count += 1
        print(f"{self.func.__name__} has been called {self.count} times")
        return self.func(*args, **kwargs)

# 17. Property decorators
class Temperature:
    """Temperature class with property decorators"""
    def __init__(self, celsius: float = 0.0):
        self._celsius = celsius
    
    @property
    def celsius(self) -> float:
        return self._celsius
    
    @celsius.setter
    def celsius(self, value: float):
        if value < -273.15:
            raise ValueError("Temperature cannot be below absolute zero")
        self._celsius = value
    
    @property
    def fahrenheit(self) -> float:
        return self._celsius * 9/5 + 32

# 18. Static methods and class methods
class MathUtils:
    """Math utilities class"""
    pi = 3.14159265359
    
    @staticmethod
    def add(x: float, y: float) -> float:
        return x + y
    
    @classmethod
    def circle_area(cls, radius: float) -> float:
        return cls.pi * radius ** 2

# 19. Multiple inheritance and Mixins
class LoggerMixin:
    """Logger mixin"""
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._logger = logging.getLogger(self.__class__.__name__)
    
    def log(self, message: str, level: int = logging.INFO):
        self._logger.log(level, message)

class SerializableMixin:
    """Serializable mixin"""
    def to_dict(self) -> Dict:
        result = {}
        for key, value in self.__dict__.items():
            if not key.startswith('_'):
                result[key] = value
        return result

class User(LoggerMixin, SerializableMixin):
    """User class using mixins"""
    def __init__(self, name: str, email: str):
        super().__init__()
        self.name = name
        self.email = email
        self.log(f"User created: {name}")

# 20. Generic types and protocols
from typing import TypeVar, Generic, Protocol, runtime_checkable

T = TypeVar('T')

class Container(Generic[T]):
    """Generic container"""
    def __init__(self, item: T):
        self.item = item
    
    def get_item(self) -> T:
        return self.item

@runtime_checkable
class Drawable(Protocol):
    """Drawable protocol"""
    def draw(self) -> str:
        ...

class CircleShape:
    def draw(self) -> str:
        return "Drawing a circle"

def draw_shape(shape: Drawable) -> str:
    return shape.draw()

# 21. Pattern matching (Python 3.10+)
def match_demo(value: Any) -> str:
    """Pattern matching demo"""
    match value:
        case int(n) if n > 0:
            return f"Positive integer: {n}"
        case int(n) if n < 0:
            return f"Negative integer: {n}"
        case str(s):
            return f"String: {s}"
        case [x, y]:
            return f"List with two elements: {x}, {y}"
        case {"name": name, "age": age}:
            return f"Person: {name}, {age}"
        case _:
            return "Unknown type"

# 22. Walrus operator (Python 3.8+)
def walrus_demo():
    """Walrus operator demo"""
    # Traditional way
    n = 10
    if n > 5:
        print(f"{n} is greater than 5")
    
    # Using walrus operator
    if (m := 20) > 15:
        print(f"{m} is greater than 15")
    
    # In list comprehensions
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    filtered = [y for x in numbers if (y := x * 2) > 10]
    
    return {
        "filtered": filtered
    }

# 23. Concurrency and multithreading
def thread_demo():
    """Multithreading demo"""
    def worker(thread_id: int, result_list: List):
        for i in range(3):
            time.sleep(0.1)
            result_list.append(f"Thread {thread_id}: Work {i}")
    
    threads = []
    results = []
    
    # Create threads
    for i in range(3):
        thread = threading.Thread(target=worker, args=(i, results))
        threads.append(thread)
        thread.start()
    
    # Wait for all threads to complete
    for thread in threads:
        thread.join()
    
    return results

# 24. Async programming
async def async_demo():
    """Async programming demo"""
    async def fetch_data(task_id: int):
        await asyncio.sleep(0.1)
        return f"Data from task {task_id}"
    
    # Create multiple async tasks
    tasks = [fetch_data(i) for i in range(3)]
    
    # Execute concurrently
    results = await asyncio.gather(*tasks)
    
    return results

# 25. Memory management and garbage collection
def memory_management_demo():
    """Memory management demo"""
    # Create many objects
    objects = [object() for _ in range(1000)]
    
    # Delete references
    del objects
    
    # Manually trigger garbage collection
    collected = gc.collect()
    
    return f"Garbage collected {collected} objects"

# Main function to test all features
@timer_decorator
def main():
    """Main function, test all Python syntax features"""
    print("=== Comprehensive Python Syntax Demo ===\n")
    
    # 1. Basic data types and enums
    print("1. Basic data types and enums:")
    color = Color.RED
    print(f"Color: {color.name} = {color.value}")
    print(f"Is warm color: {color.is_warm}")
    
    # 2. Data classes
    person = Person("Alice", 30, "alice@example.com")
    person.add_score(85.5)
    person.add_score(92.0)
    print(f"Person: {person}")
    print(f"Average score: {person.average_score}")
    
    # 3. Classes and inheritance
    rectangle = Rectangle(5, 3)
    circle = Circle(2)
    print(f"Rectangle area: {rectangle.area()}")
    print(f"Circle area: {circle.area()}")
    
    # 4. Singleton pattern
    db1 = Database()
    db2 = Database()
    print(f"Same database instance: {db1 is db2}")
    
    # 5. Generators and iterators
    print(f"Fibonacci sequence: {list(fibonacci_generator(10))}")
    fib_iter = Fibonacci(10)
    print(f"Fibonacci iterator: {list(fib_iter)}")
    
    # 6. String processing
    string_result = advanced_string_processing()
    print(f"String processing: {string_result}")
    
    # 7. Exception handling
    division_result = divide_with_exception_handling(10, 2)
    print(f"Division result: {division_result}")
    
    # 8. Comprehensions
    comp_result = comprehensions_demo()
    print(f"Comprehensions: {comp_result}")
    
    # 9. Binary tree
    tree = BinaryTree(5)
    tree.insert(3)
    tree.insert(7)
    tree.insert(1)
    tree.insert(9)
    print(f"Tree traversal: {tree.inorder_traversal()}")
    
    # 10. Property decorators
    temp = Temperature(25)
    print(f"Temperature: {temp.celsius}°C = {temp.fahrenheit}°F")
    
    # 11. Static methods and class methods
    print(f"Math utils - add: {MathUtils.add(5, 3)}")
    print(f"Math utils - circle area: {MathUtils.circle_area(2)}")
    
    # 12. Mixins
    user = User("Bob", "bob@example.com")
    user.log("User logged in")
    user_data = user.to_dict()
    print(f"User data: {user_data}")
    
    # 13. Generics
    container = Container("Hello")
    print(f"Generic container: {container.get_item()}")
    
    # 14. Protocols and type hints
    circle_shape = CircleShape()
    print(f"Drawing: {draw_shape(circle_shape)}")
    
    # 15. Pattern matching
    print(f"Pattern matching: {match_demo(42)}")
    print(f"Pattern matching: {match_demo('hello')}")
    
    # 16. Multithreading
    thread_results = thread_demo()
    print(f"Thread results: {thread_results}")
    
    # 17. Async programming
    async_results = asyncio.run(async_demo())
    print(f"Async results: {async_results}")
    
    # 18. Walrus operator
    walrus_result = walrus_demo()
    print(f"Walrus result: {walrus_result}")
    
    # 19. Memory management
    memory_result = memory_management_demo()
    print(f"Memory management: {memory_result}")
    
    print("\n=== All tests completed ===")

if __name__ == "__main__":
    main()