#!/usr/bin/env python3
"""
Complex Python test for advanced language features and edge cases.
This tests the compiler's ability to handle sophisticated Python constructs.
"""

import asyncio
import concurrent.futures
import threading
import multiprocessing
import weakref
import gc
import contextlib
from dataclasses import dataclass, field
from typing import Generic, TypeVar, Union, Optional, Callable, Any
from abc import ABC, abstractmethod
import sys

# Test dataclasses
@dataclass
class Person:
    name: str
    age: int
    skills: list = field(default_factory=list)
    
    def __post_init__(self):
        if self.age < 0:
            raise ValueError("Age cannot be negative")

# Test abstract base classes
class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass
    
    @abstractmethod
    def perimeter(self) -> float:
        pass

class Rectangle(Shape):
    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height
    
    def area(self) -> float:
        return self.width * self.height
    
    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

class Circle(Shape):
    def __init__(self, radius: float):
        self.radius = radius
    
    def area(self) -> float:
        return 3.14159 * self.radius ** 2
    
    def perimeter(self) -> float:
        return 2 * 3.14159 * self.radius

# Test generics
T = TypeVar('T')

class Container(Generic[T]):
    def __init__(self):
        self._items: list[T] = []
    
    def add(self, item: T) -> None:
        self._items.append(item)
    
    def get(self, index: int) -> T:
        return self._items[index]
    
    def size(self) -> int:
        return len(self._items)

# Test decorators
def timer_decorator(func):
    """Decorator to measure function execution time"""
    import time
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} took {end - start:.4f} seconds")
        return result
    return wrapper

def memoize(func):
    """Memoization decorator"""
    cache = {}
    def wrapper(*args):
        if args in cache:
            return cache[args]
        result = func(*args)
        cache[args] = result
        return result
    return wrapper

# Test context managers
@contextlib.contextmanager
def custom_context():
    print("Entering context")
    try:
        yield "context_value"
    finally:
        print("Exiting context")

class CustomContextManager:
    def __init__(self, name):
        self.name = name
    
    def __enter__(self):
        print(f"Entering {self.name}")
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        print(f"Exiting {self.name}")
        return False

# Test metaclasses
class SingletonMeta(type):
    _instances = {}
    
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(*args, **kwargs)
        return cls._instances[cls]

class Singleton(metaclass=SingletonMeta):
    def __init__(self):
        self.value = 42

# Test descriptors
class LoggedAttribute:
    def __init__(self, initial_value=None):
        self.value = initial_value
        self.name = None
    
    def __set_name__(self, owner, name):
        self.name = name
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        print(f"Getting {self.name}: {self.value}")
        return self.value
    
    def __set__(self, obj, value):
        print(f"Setting {self.name}: {self.value} -> {value}")
        self.value = value

class LoggedClass:
    attr = LoggedAttribute(10)

# Test property decorators
class Temperature:
    def __init__(self):
        self._celsius = 0
    
    @property
    def celsius(self):
        return self._celsius
    
    @celsius.setter
    def celsius(self, value):
        if value < -273.15:
            raise ValueError("Temperature below absolute zero is not possible")
        self._celsius = value
    
    @property
    def fahrenheit(self):
        return self._celsius * 9/5 + 32
    
    @fahrenheit.setter
    def fahrenheit(self, value):
        self.celsius = (value - 32) * 5/9

# Test generators and iterators
def fibonacci_generator(n):
    """Generate fibonacci numbers up to n"""
    a, b = 0, 1
    count = 0
    while count < n:
        yield a
        a, b = b, a + b
        count += 1

class RangeIterator:
    def __init__(self, start, end, step=1):
        self.current = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.current >= self.end:
            raise StopIteration
        value = self.current
        self.current += self.step
        return value

# Test async/await (if supported)
async def async_function(delay: float) -> str:
    await asyncio.sleep(delay)
    return f"Async result after {delay} seconds"

async def async_generator(n: int):
    for i in range(n):
        await asyncio.sleep(0.01)
        yield i

# Test exception handling and custom exceptions
class CustomError(Exception):
    def __init__(self, message, code=None):
        super().__init__(message)
        self.code = code

def test_exception_handling():
    """Test various exception handling scenarios"""
    results = {}
    
    # Test basic exception handling
    try:
        x = 1 / 0
    except ZeroDivisionError as e:
        results['zero_division'] = str(e)
    
    # Test custom exception
    try:
        raise CustomError("Custom error message", code=123)
    except CustomError as e:
        results['custom_error'] = {'message': str(e), 'code': e.code}
    
    # Test multiple exceptions
    try:
        # This will raise TypeError
        "string" + 42
    except (TypeError, ValueError) as e:
        results['multiple_catch'] = type(e).__name__
    
    # Test finally block
    finally_executed = False
    try:
        pass
    finally:
        finally_executed = True
    results['finally_executed'] = finally_executed
    
    # Test else block
    else_executed = False
    try:
        pass
    except Exception:
        pass
    else:
        else_executed = True
    results['else_executed'] = else_executed
    
    return results

# Test advanced data structures
def test_advanced_data_structures():
    """Test advanced Python data structures"""
    results = {}
    
    # Test sets
    set1 = {1, 2, 3, 4, 5}
    set2 = {4, 5, 6, 7, 8}
    results['set_union'] = list(set1 | set2)
    results['set_intersection'] = list(set1 & set2)
    results['set_difference'] = list(set1 - set2)
    
    # Test frozenset
    fs = frozenset([1, 2, 3])
    results['frozenset_hashable'] = hash(fs) is not None
    
    # Test bytes and bytearray
    b = b"hello"
    ba = bytearray(b"world")
    ba.extend(b" python")
    results['bytes_decode'] = b.decode('utf-8')
    results['bytearray_result'] = ba.decode('utf-8')
    
    # Test memoryview
    mv = memoryview(ba)
    results['memoryview_slice'] = bytes(mv[0:5]).decode('utf-8')
    
    return results

# Test comprehensions
def test_comprehensions():
    """Test various Python comprehensions"""
    data = range(10)
    
    # List comprehension
    squares = [x**2 for x in data if x % 2 == 0]
    
    # Dict comprehension
    square_dict = {x: x**2 for x in data if x % 2 == 0}
    
    # Set comprehension
    square_set = {x**2 for x in data if x % 2 == 0}
    
    # Generator comprehension
    square_gen = (x**2 for x in data if x % 2 == 0)
    
    # Nested comprehension
    matrix = [[i*j for j in range(3)] for i in range(3)]
    
    return {
        'list_comp': squares,
        'dict_comp': square_dict,
        'set_comp': list(square_set),
        'gen_comp': list(square_gen),
        'nested_comp': matrix
    }

# Test weak references
def test_weak_references():
    """Test weak reference functionality"""
    results = {}
    
    class TestObject:
        def __init__(self, value):
            self.value = value
    
    obj = TestObject(42)
    weak_ref = weakref.ref(obj)
    
    results['weak_ref_alive'] = weak_ref() is not None
    results['weak_ref_value'] = weak_ref().value if weak_ref() else None
    
    # Test weak reference callback
    callback_called = False
    def callback(ref):
        nonlocal callback_called
        callback_called = True
    
    obj2 = TestObject(24)
    weak_ref2 = weakref.ref(obj2, callback)
    del obj2
    gc.collect()  # Force garbage collection
    
    results['callback_called'] = callback_called
    
    return results

# Test lambda functions and functional programming
def test_functional_programming():
    """Test functional programming features"""
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    
    # Lambda functions
    square = lambda x: x**2
    is_even = lambda x: x % 2 == 0
    
    # Map, filter, reduce
    from functools import reduce
    
    squared = list(map(square, numbers))
    evens = list(filter(is_even, numbers))
    sum_all = reduce(lambda x, y: x + y, numbers)
    
    # Higher-order functions
    def apply_twice(func, x):
        return func(func(x))
    
    def make_multiplier(n):
        return lambda x: x * n
    
    double = make_multiplier(2)
    quadruple = apply_twice(double, 5)
    
    return {
        'squared': squared,
        'evens': evens,
        'sum_all': sum_all,
        'quadruple': quadruple,
        'higher_order': apply_twice(lambda x: x + 1, 5)
    }

# Test operator overloading
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)
    
    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)
    
    def __repr__(self):
        return f"Vector({self.x}, {self.y})"
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __len__(self):
        return int((self.x**2 + self.y**2)**0.5)

def test_operator_overloading():
    """Test operator overloading"""
    v1 = Vector(1, 2)
    v2 = Vector(3, 4)
    
    v3 = v1 + v2
    v4 = v2 - v1
    v5 = v1 * 3
    
    return {
        'add_result': (v3.x, v3.y),
        'sub_result': (v4.x, v4.y),
        'mul_result': (v5.x, v5.y),
        'equality': v1 == Vector(1, 2),
        'repr': repr(v1)
    }

def main():
    """Main test function"""
    print("Testing Advanced Python Features in Compiled Code")
    print("=" * 60)
    
    test_functions = [
        ("Exception Handling", test_exception_handling),
        ("Advanced Data Structures", test_advanced_data_structures),
        ("Comprehensions", test_comprehensions),
        ("Weak References", test_weak_references),
        ("Functional Programming", test_functional_programming),
        ("Operator Overloading", test_operator_overloading),
    ]
    
    # Test dataclasses
    person = Person("Alice", 30, ["Python", "Go"])
    print(f"Dataclass test: {person.name} is {person.age} years old")
    
    # Test abstract classes
    shapes = [Rectangle(5, 3), Circle(2)]
    for shape in shapes:
        print(f"Shape area: {shape.area():.2f}, perimeter: {shape.perimeter():.2f}")
    
    # Test generics
    int_container = Container[int]()
    int_container.add(42)
    int_container.add(24)
    print(f"Container size: {int_container.size()}")
    
    # Test decorators
    @timer_decorator
    @memoize
    def fibonacci(n):
        if n <= 1:
            return n
        return fibonacci(n-1) + fibonacci(n-2)
    
    result = fibonacci(10)
    print(f"Fibonacci(10) = {result}")
    
    # Test context managers
    with custom_context() as value:
        print(f"Context value: {value}")
    
    with CustomContextManager("test") as cm:
        print(f"Custom context manager: {cm.name}")
    
    # Test singleton
    s1 = Singleton()
    s2 = Singleton()
    print(f"Singleton test: {s1 is s2}")
    
    # Test descriptors
    logged = LoggedClass()
    logged.attr = 20
    value = logged.attr
    
    # Test properties
    temp = Temperature()
    temp.celsius = 25
    print(f"Temperature: {temp.celsius}°C = {temp.fahrenheit:.1f}°F")
    
    # Test generators
    fib_gen = fibonacci_generator(5)
    fib_numbers = list(fib_gen)
    print(f"Fibonacci sequence: {fib_numbers}")
    
    # Test custom iterator
    custom_range = RangeIterator(0, 5, 2)
    range_values = list(custom_range)
    print(f"Custom range: {range_values}")
    
    # Run all test functions
    all_results = {}
    successful = 0
    
    for test_name, test_func in test_functions:
        print(f"\nTesting {test_name}...")
        try:
            result = test_func()
            all_results[test_name] = result
            print(f"✓ {test_name} completed successfully")
            successful += 1
        except Exception as e:
            print(f"✗ {test_name} failed: {e}")
            all_results[test_name] = {"error": str(e)}
    
    print("\n" + "=" * 60)
    print("Advanced Features Test Summary:")
    print(f"Total tests: {len(test_functions)}")
    print(f"Successful: {successful}")
    print(f"Failed: {len(test_functions) - successful}")
    
    # Save results
    import json
    with open('python_advanced_test_results.json', 'w') as f:
        json.dump(all_results, f, indent=2, default=str)
    
    print("\nDetailed results saved to 'python_advanced_test_results.json'")
    
    return successful == len(test_functions)

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)