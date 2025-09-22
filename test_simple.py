#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Simple Python test file for Fluxus compiler
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
    
    @classmethod
    def from_birth_year(cls, name, birth_year):
        age = 2025 - birth_year
        return cls(name, age)
    
    @staticmethod
    def is_adult(age):
        return age >= 18

class Student(Person):
    def __init__(self, name, age, student_id):
        super().__init__(name, age)
        self.student_id = student_id
        self.courses = []
    
    def enroll(self, course):
        self.courses.append(course)
        return f"Enrolled in {course}"

# Exception handling
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Division by zero error")
except Exception as e:
    print(f"Error: {e}")
else:
    print("Calculation successful")
finally:
    print("Cleanup operation")

# File operations
with open("test_file.txt", "w") as f:
    f.write("Hello, file!\n")
    f.write("This is a test.\n")

with open("test_file.txt", "r") as f:
    content = f.read()
    print(f"File content:\n{content}")

# List comprehensions
squares = [x**2 for x in range(10)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]

# Generator expressions
square_gen = (x**2 for x in range(10))
for square in square_gen:
    print(f"Generator square: {square}")

# Decorators
def timer(func):
    """Timer decorator"""
    import time
    
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} execution time: {end - start:.5f} seconds")
        return result
    return wrapper

@timer
def fibonacci(n):
    """Calculate Fibonacci sequence"""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

# Context managers
class ManagedFile:
    def __init__(self, filename, mode):
        self.filename = filename
        self.mode = mode
    
    def __enter__(self):
        self.file = open(self.filename, self.mode)
        return self.file
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file.close()

with ManagedFile("test_file.txt", "r") as f:
    content = f.read()
    print(f"Using context manager to read: {content[:20]}...")

# Iterators and generators
class Countdown:
    """Countdown iterator"""
    def __init__(self, start):
        self.start = start
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.start <= 0:
            raise StopIteration
        self.start -= 1
        return self.start

for i in Countdown(5):
    print(f"Countdown: {i}")

def simple_generator():
    yield 1
    yield 2
    yield 3

for value in simple_generator():
    print(f"Generator value: {value}")

# Regular expressions
import re

text = "My phone number is 123-456-7890, email is example@example.com"
phone_pattern = r'\d{3}-\d{3}-\d{4}'
email_pattern = r'\w+@\w+\.\w+'

phone_match = re.search(phone_pattern, text)
email_match = re.search(email_pattern, text)

if phone_match:
    print(f"Found phone number: {phone_match.group()}")

if email_match:
    print(f"Found email: {email_match.group()}")

# JSON processing
import json

data = {
    "name": "Alice",
    "age": 30,
    "scores": [85, 90, 95],
    "active": True
}

json_str = json.dumps(data, indent=2)
print(f"JSON string:\n{json_str}")

parsed_data = json.loads(json_str)
print(f"Parsed data: {parsed_data}")

# Type hints
from typing import List, Dict, Tuple, Optional, Union

def process_items(items: List[str]) -> Dict[str, int]:
    """Process string list, return length of each string"""
    result = {}
    for item in items:
        result[item] = len(item)
    return result

def get_value(data: Dict[str, int], key: str) -> Optional[int]:
    """Get value from dictionary, may return None"""
    return data.get(key)

# Enums
from enum import Enum, auto

class Color(Enum):
    RED = auto()
    GREEN = auto()
    BLUE = auto()

print(f"Color enum: {Color.RED}")

# Data classes
from dataclasses import dataclass

@dataclass
class Product:
    name: str
    price: float
    quantity: int = 1
    
    def total_value(self) -> float:
        return self.price * self.quantity

product = Product("Laptop", 999.99, 2)
print(f"Product: {product.name}, Total value: {product.total_value()}")

# Async programming
import asyncio

async def say_after(delay, what_to_say):
    """Async function, output after delay"""
    await asyncio.sleep(delay)
    print(what_to_say)

async def main():
    """Main async function"""
    task1 = asyncio.create_task(say_after(1, "Hello"))
    task2 = asyncio.create_task(say_after(2, "World"))
    
    await task1
    await task2

# Properties and descriptors
class Temperature:
    """Temperature class using properties"""
    def __init__(self, celsius=0):
        self._celsius = celsius
    
    @property
    def celsius(self):
        return self._celsius
    
    @celsius.setter
    def celsius(self, value):
        if value < -273.15:
            raise ValueError("Temperature cannot be below absolute zero")
        self._celsius = value
    
    @property
    def fahrenheit(self):
        return self._celsius * 9/5 + 32

temp = Temperature(25)
print(f"Celsius: {temp.celsius}°C")
print(f"Fahrenheit: {temp.fahrenheit}°F")

# Metaclasses
class Meta(type):
    """Custom metaclass"""
    def __new__(cls, name, bases, dct):
        # Add new attribute
        dct['class_name'] = name
        # Add new method
        def hello(self):
            return f"Hello from {self.class_name}"
        dct['hello'] = hello
        return super().__new__(cls, name, bases, dct)

class MyClass(metaclass=Meta):
    pass

obj = MyClass()
print(f"Class name: {obj.class_name}")
print(f"Greeting: {obj.hello()}")

# Abstract base classes
from abc import ABC, abstractmethod

class Shape(ABC):
    """Abstract base class for shapes"""
    @abstractmethod
    def area(self):
        pass
    
    @abstractmethod
    def perimeter(self):
        pass

class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def area(self):
        return self.width * self.height
    
    def perimeter(self):
        return 2 * (self.width + self.height)

rect = Rectangle(5, 3)
print(f"Rectangle area: {rect.area()}")
print(f"Rectangle perimeter: {rect.perimeter()}")

# Closures and function factories
def make_multiplier(n):
    """Create a multiplier function"""
    def multiplier(x):
        return x * n
    return multiplier

times3 = make_multiplier(3)
times5 = make_multiplier(5)

print(f"3x: {times3(10)}")
print(f"5x: {times5(10)}")

# Memory management and weak references
import weakref

class BigObject:
    def __del__(self):
        print("BigObject destroyed")

obj = BigObject()
weak_ref = weakref.ref(obj)

print(f"Object alive: {weak_ref() is not None}")
del obj
print(f"Object alive: {weak_ref() is not None}")

# Serialization and pickle
import pickle

data = {"name": "Alice", "age": 30, "scores": [85, 90, 95]}

with open("data.pkl", "wb") as f:
    pickle.dump(data, f)

with open("data.pkl", "rb") as f:
    loaded_data = pickle.load(f)

print(f"Deserialized data: {loaded_data}")

# Context variables
import contextvars

user_var = contextvars.ContextVar('user', default='Anonymous')

def process_request():
    user = user_var.get()
    print(f"Processing request, user: {user}")

# Set context variable
token = user_var.set("Alice")
process_request()

# Reset context variable
user_var.reset(token)
process_request()

# Walrus operator
numbers = [1, 2, 3, 4, 5]
while (n := len(numbers)) > 0:
    popped = numbers.pop()
    print(f"Popped: {popped}, Remaining: {n-1}")

# Positional and keyword arguments
def example_func(a, b, /, c, d, *, e, f):
    """Demonstrate different parameter types:
    - a, b: positional-only parameters
    - c, d: positional or keyword parameters
    - e, f: keyword-only parameters
    """
    print(f"a={a}, b={b}, c={c}, d={d}, e={e}, f={f}")

example_func(1, 2, 3, d=4, e=5, f=6)

# Operator overloading
class Vector:
    """Vector class demonstrating operator overloading"""
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)
    
    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __str__(self):
        return f"Vector({self.x}, {self.y})"

v1 = Vector(1, 2)
v2 = Vector(3, 4)

print(f"v1 + v2 = {v1 + v2}")
print(f"v1 - v2 = {v1 - v2}")
print(f"v1 * 3 = {v1 * 3}")
print(f"v1 == v2 = {v1 == v2}")

# Singleton pattern
class Singleton:
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

s1 = Singleton()
s2 = Singleton()
print(f"s1 is s2: {s1 is s2}")

# Factory pattern
class Dog:
    def speak(self):
        return "Woof!"

class Cat:
    def speak(self):
        return "Meow!"

def animal_factory(animal_type):
    """Animal factory"""
    if animal_type == "dog":
        return Dog()
    elif animal_type == "cat":
        return Cat()
    else:
        raise ValueError(f"Unknown animal type: {animal_type}")

dog = animal_factory("dog")
cat = animal_factory("cat")
print(f"Dog says: {dog.speak()}")
print(f"Cat says: {cat.speak()}")

# Observer pattern
class Subject:
    """Subject (observable)"""
    def __init__(self):
        self._observers = []
        self._state = None
    
    def attach(self, observer):
        if observer not in self._observers:
            self._observers.append(observer)
    
    def detach(self, observer):
        try:
            self._observers.remove(observer)
        except ValueError:
            pass
    
    def notify(self):
        for observer in self._observers:
            observer.update(self)
    
    @property
    def state(self):
        return self._state
    
    @state.setter
    def state(self, value):
        self._state = value
        self.notify()

class Observer:
    """Observer"""
    def update(self, subject):
        print(f"Observer received update: {subject.state}")

subject = Subject()
observer1 = Observer()
observer2 = Observer()

subject.attach(observer1)
subject.attach(observer2)

subject.state = "New state"

# Strategy pattern
class Strategy:
    """Strategy base class"""
    def execute(self, a, b):
        pass

class AddStrategy(Strategy):
    def execute(self, a, b):
        return a + b

class SubtractStrategy(Strategy):
    def execute(self, a, b):
        return a - b

class MultiplyStrategy(Strategy):
    def execute(self, a, b):
        return a * b

class Context:
    """Context"""
    def __init__(self, strategy):
        self._strategy = strategy
    
    def set_strategy(self, strategy):
        self._strategy = strategy
    
    def execute_strategy(self, a, b):
        return self._strategy.execute(a, b)

context = Context(AddStrategy())
print(f"Addition: {context.execute_strategy(5, 3)}")

context.set_strategy(SubtractStrategy())
print(f"Subtraction: {context.execute_strategy(5, 3)}")

context.set_strategy(MultiplyStrategy())
print(f"Multiplication: {context.execute_strategy(5, 3)}")

# Decorator pattern
class Coffee:
    """Coffee base class"""
    def cost(self):
        return 5
    
    def description(self):
        return "Basic coffee"

class MilkDecorator:
    """Milk decorator"""
    def __init__(self, coffee):
        self._coffee = coffee
    
    def cost(self):
        return self._coffee.cost() + 2
    
    def description(self):
        return self._coffee.description() + ", milk"

class SugarDecorator:
    """Sugar decorator"""
    def __init__(self, coffee):
        self._coffee = coffee
    
    def cost(self):
        return self._coffee.cost() + 1
    
    def description(self):
        return self._coffee.description() + ", sugar"

coffee = Coffee()
print(f"{coffee.description()}: ${coffee.cost()}")

coffee_with_milk = MilkDecorator(coffee)
print(f"{coffee_with_milk.description()}: ${coffee_with_milk.cost()}")

coffee_with_milk_and_sugar = SugarDecorator(coffee_with_milk)
print(f"{coffee_with_milk_and_sugar.description()}: ${coffee_with_milk_and_sugar.cost()}")

# Context manager decorator
from contextlib import contextmanager

@contextmanager
def managed_resource(resource_name):
    """Context manager decorator example"""
    print(f"Acquiring resource: {resource_name}")
    try:
        yield resource_name
    finally:
        print(f"Releasing resource: {resource_name}")

with managed_resource("Database connection"):
    print("Using resource")

# Function annotations
def calculate(
    a: float,
    b: float,
    operation: str = "add"
) -> float:
    """Function with detailed annotations"""
    if operation == "add":
        return a + b
    elif operation == "subtract":
        return a - b
    elif operation == "multiply":
        return a * b
    elif operation == "divide":
        return a / b
    else:
        raise ValueError(f"Unknown operation: {operation}")

# Type aliases
from typing import List, Dict

Vector = List[float]
Matrix = List[Vector]
UserInfo = Dict[str, Union[str, int, List[int]]]

def dot_product(v1: Vector, v2: Vector) -> float:
    """Calculate vector dot product"""
    return sum(x * y for x, y in zip(v1, v2))

# Generics
from typing import TypeVar, Generic

T = TypeVar('T')

class Stack(Generic[T]):
    """Generic stack class"""
    def __init__(self):
        self._items = []
    
    def push(self, item: T) -> None:
        self._items.append(item)
    
    def pop(self) -> T:
        return self._items.pop()
    
    def is_empty(self) -> bool:
        return len(self._items) == 0

# Use generic stack
int_stack = Stack[int]()
int_stack.push(1)
int_stack.push(2)
print(f"Popped integer: {int_stack.pop()}")

str_stack = Stack[str]()
str_stack.push("Hello")
str_stack.push("World")
print(f"Popped string: {str_stack.pop()}")

# Protocols
from typing import Protocol

class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def draw(self) -> str:
        return "Drawing circle"

class Square:
    def draw(self) -> str:
        return "Drawing square"

def render_shape(shape: Drawable) -> None:
    print(shape.draw())

circle = Circle()
square = Square()

render_shape(circle)
render_shape(square)

# Callable objects
class Adder:
    def __init__(self, n):
        self.n = n
    
    def __call__(self, x):
        return self.n + x

add5 = Adder(5)
print(f"5 + 3 = {add5(3)}")

# Descriptor protocol
class ValidatedAttribute:
    """Validated attribute descriptor"""
    def __init__(self, name, type_, default=None):
        self.name = name
        self.type_ = type_
        self.default = default
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        value = obj.__dict__.get(self.name, self.default)
        if value is None:
            return None
        if not isinstance(value, self.type_):
            raise TypeError(f"{self.name} must be {self.type_}")
        return value
    
    def __set__(self, obj, value):
        if value is not None and not isinstance(value, self.type_):
            raise TypeError(f"{self.name} must be {self.type_}")
        obj.__dict__[self.name] = value

class Person:
    name = ValidatedAttribute("name", str)
    age = ValidatedAttribute("age", int, 0)
    height = ValidatedAttribute("height", float)

person = Person()
person.name = "Alice"
person.age = 30
person.height = 170.5

print(f"Name: {person.name}, Age: {person.age}, Height: {person.height}")

try:
    person.age = "thirty"  # This will raise TypeError
except TypeError as e:
    print(f"Error: {e}")

# Context manager protocol
class Timer:
    """Timer context manager"""
    def __init__(self, name):
        self.name = name
        self.start = None
        self.end = None
    
    def __enter__(self):
        import time
        self.start = time.time()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        import time
        self.end = time.time()
        elapsed = self.end - self.start
        print(f"{self.name} elapsed: {elapsed:.5f} seconds")
        return False  # Don't suppress exceptions

with Timer("Code block"):
    import time
    time.sleep(0.1)

# Iterator protocol
class Fibonacci:
    """Fibonacci sequence iterator"""
    def __init__(self, max_count):
        self.max_count = max_count
        self.count = 0
        self.a, self.b = 0, 1
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.count >= self.max_count:
            raise StopIteration
        self.count += 1
        result = self.a
        self.a, self.b = self.b, self.a + self.b
        return result

for num in Fibonacci(10):
    print(f"Fibonacci number: {num}")

# Iterable protocol
class Range:
    """Custom range class"""
    def __init__(self, start, end, step=1):
        self.start = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return RangeIterator(self.start, self.end, self.step)

class RangeIterator:
    """Range iterator"""
    def __init__(self, start, end, step):
        self.current = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.step > 0 and self.current >= self.end:
            raise StopIteration
        if self.step < 0 and self.current <= self.end:
            raise StopIteration
        
        result = self.current
        self.current += self.step
        return result

for i in Range(1, 5):
    print(f"Range number: {i}")

for i in Range(5, 0, -1):
    print(f"Reverse range number: {i}")

# Generator expressions and yield from
def chain(*iterables):
    """Chain multiple iterables"""
    for it in iterables:
        yield from it

for item in chain([1, 2], ['a', 'b'], [3, 4]):
    print(f"Chained item: {item}")

# Type checking
import typing

def check_type(value, expected_type):
    """Check if value matches expected type"""
    if not isinstance(value, expected_type):
        raise TypeError(f"Expected {expected_type}, got {type(value)}")
    return True

# Usage examples
check_type(42, int)
check_type("hello", str)

try:
    check_type(3.14, int)
except TypeError as e:
    print(f"Type error: {e}")

# Performance profiling
import timeit

def test_performance():
    """Performance test function"""
    total = 0
    for i in range(1000):
        total += i
    return total

# Measure execution time
execution_time = timeit.timeit(test_performance, number=1000)
print(f"Execution time: {execution_time:.5f} seconds")

# Memory profiling
import sys

def get_size(obj):
    """Get object size"""
    return sys.getsizeof(obj)

print(f"Integer size: {get_size(42)} bytes")
print(f"String size: {get_size('hello')} bytes")
print(f"List size: {get_size([1, 2, 3])} bytes")

# Code objects and dynamic execution
code_str = """
def dynamic_function(x, y):
    return x + y

result = dynamic_function(10, 20)
print(f"Dynamic execution result: {result}")
"""

# Compile and execute code
code_obj = compile(code_str, "<string>", "exec")
exec(code_obj)

# Function attributes
def example_function():
    """Example function"""
    pass

# Set function attributes
example_function.author = "Alice"
example_function.version = "1.0"

print(f"Function author: {example_function.author}")
print(f"Function version: {example_function.version}")

# Closures and variable scope
def outer_function(x):
    def inner_function(y):
        return x + y
    return inner_function

add5 = outer_function(5)
print(f"Closure result: {add5(10)}")

# Nonlocal and global keywords
x = 10  # Global variable

def modify_global():
    global x
    x = 20
    print(f"Modified global variable inside: {x}")

def outer():
    x = 5  # Outer function variable
    
    def inner():
        nonlocal x
        x = 10
        print(f"Modified outer variable inside: {x}")
    
    print(f"Outer variable before: {x}")
    inner()
    print(f"Outer variable after: {x}")

print(f"Global variable initial: {x}")
modify_global()
print(f"Global variable after: {x}")
outer()

# Tuple unpacking
a, b, c = (1, 2, 3)
print(f"Tuple unpacking: a={a}, b={b}, c={c}")

# Swap variables
a, b = b, a
print(f"After swap: a={a}, b={b}")

# Star unpacking
first, *middle, last = [1, 2, 3, 4, 5]
print(f"First: {first}, Middle: {middle}, Last: {last}")

# Dictionary unpacking
dict1 = {"a": 1, "b": 2}
dict2 = {"c": 3, "d": 4}
merged_dict = {**dict1, **dict2}
print(f"Merged dictionary: {merged_dict}")

# Set operations
set1 = {1, 2, 3}
set2 = {3, 4, 5}

print(f"Union: {set1 | set2}")
print(f"Intersection: {set1 & set2}")
print(f"Difference: {set1 - set2}")
print(f"Symmetric difference: {set1 ^ set2}")

# List methods
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
numbers.insert(0, 0)
numbers.extend([7, 8, 9])
numbers.remove(3)
popped = numbers.pop()
print(f"Popped element: {popped}")
print(f"List: {numbers}")
print(f"Index: {numbers.index(4)}")
print(f"Count: {numbers.count(2)}")
numbers.sort()
print(f"After sorting: {numbers}")
numbers.reverse()
print(f"After reversing: {numbers}")

# Dictionary methods
student = {"name": "Alice", "age": 25, "courses": ["Math", "Science"]}
print(f"Keys: {student.keys()}")
print(f"Values: {student.values()}")
print(f"Items: {student.items()}")
print(f"Get name: {student.get('name')}")
print(f"Get grades: {student.get('grades', 'N/A')}")
student.update({"age": 26, "grades": [85, 90, 95]})
print(f"After update: {student}")
removed = student.pop('courses')
print(f"Removed courses: {removed}")
print(f"Final dictionary: {student}")

# String methods
text = "  Hello, World!  "
print(f"Stripped: '{text.strip()}'")
print(f"Uppercase: '{text.upper()}'")
print(f"Lowercase: '{text.lower()}'")
print(f"Replace: '{text.replace('World', 'Python')}'")
print(f"Split: '{text.split(',')}'")
print(f"Join: {'-'.join(['a', 'b', 'c'])}")
print(f"Find: '{text.find('World')}'")
print(f"Count: '{text.count('l')}'")
print(f"Starts with Hello: {text.strip().startswith('Hello')}")
print(f"Ends with World: {text.strip().endswith('World')}")

# String formatting
name = "Alice"
age = 30
height = 1.65

# f-strings (Python 3.6+)
print(f"Name: {name}, Age: {age}, Height: {height:.2f}m")

# str.format()
print("Name: {}, Age: {}, Height: {:.2f}m".format(name, age, height))

# % formatting
print("Name: %s, Age: %d, Height: %.2fm" % (name, age, height))

# File and directory operations
import os

# Get current working directory
print(f"Current working directory: {os.getcwd()}")

# List directory contents
print(f"Directory contents: {os.listdir('.')}")

# Create directory
os.makedirs("test_dir", exist_ok=True)

# Check if path exists
print(f"test_dir exists: {os.path.exists('test_dir')}")

# Check if directory
print(f"test_dir is directory: {os.path.isdir('test_dir')}")

# Check if file
print(f"test_dir is file: {os.path.isfile('test_dir')}")

# Get file size
with open("test_file.txt", "w") as f:
    f.write("Hello, World!")
print(f"File size: {os.path.getsize('test_file.txt')} bytes")

# Delete file
os.remove("test_file.txt")

# Delete directory
os.rmdir("test_dir")

# Path operations
from pathlib import Path

# Create Path objects
current_dir = Path(".")
home_dir = Path.home()

print(f"Current directory: {current_dir.resolve()}")
print(f"Home directory: {home_dir}")

# Create path
new_dir = current_dir / "example_dir"
new_dir.mkdir(exist_ok=True)

# Create file
file_path = new_dir / "example.txt"
file_path.write_text("Hello, Path!")

# Read file
print(f"File content: {file_path.read_text()}")

# Iterate directory
print("Directory iteration:")
for item in new_dir.iterdir():
    print(f"  {item.name}: {'directory' if item.is_dir() else 'file'}")

# Cleanup
file_path.unlink()
new_dir.rmdir()

# Environment variables
import os

# Get environment variable
print(f"PATH: {os.environ.get('PATH', 'not set')}")

# Set environment variable
os.environ['MY_VAR'] = 'my_value'
print(f"MY_VAR: {os.environ.get('MY_VAR')}")

# Delete environment variable
if 'MY_VAR' in os.environ:
    del os.environ['MY_VAR']
print(f"MY_VAR after deletion: {os.environ.get('MY_VAR', 'not set')}")

# Command line arguments
import sys

# Get command line arguments
print(f"Script name: {sys.argv[0]}")
print(f"Arguments: {sys.argv[1:]}")

# Command line argument parsing
import argparse

# Create parser
parser = argparse.ArgumentParser(description='Example script')
parser.add_argument('--name', type=str, default='World', help='Name')
parser.add_argument('--count', type=int, default=1, help='Count')
parser.add_argument('--verbose', action='store_true', help='Verbose output')

# Simulate parsing command line arguments
# In actual usage, these arguments come from command line
args = parser.parse_args(['--name', 'Alice', '--count', '3', '--verbose'])

print(f"Name: {args.name}")
print(f"Count: {args.count}")
print(f"Verbose: {args.verbose}")

print("Python all syntax features example completed!")