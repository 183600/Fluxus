#!/usr/bin/env python3
"""
Comprehensive Python test suite covering core language features.
This file tests all major Python constructs to ensure proper compilation.
"""

import os
import sys
import json
import math
import time
import random
import datetime
import collections
import itertools
import functools
from typing import List, Dict, Any, Optional, Union, Tuple

# Test basic data types
def test_basic_types():
    """Test basic Python data types"""
    # Integers
    int_var = 42
    negative_int = -17
    big_int = 123456789012345
    
    # Floats
    float_var = 3.14159
    scientific = 1.23e-4
    negative_float = -2.718
    
    # Strings
    string_var = "Hello, Python!"
    multiline = """This is a
    multiline string"""
    raw_string = r"Raw string with \n no escapes"
    
    # Booleans
    bool_true = True
    bool_false = False
    
    # None
    none_var = None
    
    print(f"Integer: {int_var}")
    print(f"Float: {float_var:.3f}")
    print(f"String: {string_var}")
    print(f"Boolean: {bool_true}")
    print(f"None: {none_var}")

def test_collections():
    """Test Python collection types"""
    # Lists
    numbers = [1, 2, 3, 4, 5]
    mixed_list = [1, "hello", 3.14, True, None]
    nested_list = [[1, 2], [3, 4], [5, 6]]
    
    # List operations
    numbers.append(6)
    numbers.extend([7, 8])
    numbers.insert(0, 0)
    
    print(f"Numbers list: {numbers}")
    print(f"List slice [2:5]: {numbers[2:5]}")
    print(f"List comprehension: {[x * 2 for x in numbers[:5]]}")
    
    # Tuples
    coordinates = (10, 20)
    empty_tuple = ()
    single_tuple = (42,)
    
    print(f"Coordinates: {coordinates}")
    print(f"Tuple unpacking: x={coordinates[0]}, y={coordinates[1]}")
    
    # Dictionaries
    person = {
        "name": "Alice",
        "age": 30,
        "city": "New York",
        "hobbies": ["reading", "swimming"]
    }
    
    person["email"] = "alice@example.com"
    print(f"Person: {person}")
    print(f"Keys: {list(person.keys())}")
    print(f"Values: {list(person.values())}")
    
    # Sets
    unique_numbers = {1, 2, 3, 4, 5, 5, 4, 3}  # Duplicates removed
    set_from_list = set([1, 2, 2, 3, 3, 3])
    
    print(f"Unique numbers: {unique_numbers}")
    print(f"Set operations: {unique_numbers | {6, 7}}")

def test_functions():
    """Test function definitions and calls"""
    
    # Simple function
    def greet(name: str) -> str:
        return f"Hello, {name}!"
    
    # Function with default parameters
    def power(base: float, exponent: float = 2) -> float:
        return base ** exponent
    
    # Function with variable arguments
    def sum_all(*args: float) -> float:
        return sum(args)
    
    # Function with keyword arguments
    def create_person(**kwargs: Any) -> Dict[str, Any]:
        return kwargs
    
    # Lambda function
    square = lambda x: x * x
    
    # Higher-order functions
    def apply_operation(func, value):
        return func(value)
    
    print(f"Greeting: {greet('World')}")
    print(f"Power: {power(3, 4)}")
    print(f"Sum all: {sum_all(1, 2, 3, 4, 5)}")
    print(f"Create person: {create_person(name='Bob', age=25)}")
    print(f"Lambda square: {square(8)}")
    print(f"Apply operation: {apply_operation(lambda x: x * 3, 10)}")

def test_classes():
    """Test class definitions and object-oriented programming"""
    
    class Animal:
        species_count = 0
        
        def __init__(self, name: str, species: str):
            self.name = name
            self.species = species
            Animal.species_count += 1
        
        def speak(self) -> str:
            return f"{self.name} makes a sound"
        
        def __str__(self) -> str:
            return f"{self.name} the {self.species}"
        
        @classmethod
        def get_species_count(cls) -> int:
            return cls.species_count
        
        @staticmethod
        def is_mammal(species: str) -> bool:
            mammals = ["dog", "cat", "horse", "human"]
            return species.lower() in mammals
    
    class Dog(Animal):
        def __init__(self, name: str, breed: str):
            super().__init__(name, "dog")
            self.breed = breed
        
        def speak(self) -> str:
            return f"{self.name} barks: Woof!"
        
        def fetch(self) -> str:
            return f"{self.name} fetches the ball"
    
    # Create objects
    animal = Animal("Generic", "unknown")
    dog = Dog("Buddy", "Golden Retriever")
    
    print(f"Animal: {animal}")
    print(f"Animal speak: {animal.speak()}")
    print(f"Dog: {dog}")
    print(f"Dog speak: {dog.speak()}")
    print(f"Dog fetch: {dog.fetch()}")
    print(f"Species count: {Animal.get_species_count()}")
    print(f"Is dog mammal: {Animal.is_mammal('dog')}")

def test_control_flow():
    """Test control flow statements"""
    
    # If-elif-else
    age = 25
    if age < 18:
        status = "minor"
    elif age < 65:
        status = "adult"
    else:
        status = "senior"
    print(f"Age {age} -> Status: {status}")
    
    # For loops
    print("For loop with range:")
    for i in range(5):
        print(f"  {i}: {i * i}")
    
    print("For loop with list:")
    fruits = ["apple", "banana", "cherry"]
    for fruit in fruits:
        print(f"  Fruit: {fruit}")
    
    print("For loop with enumerate:")
    for i, fruit in enumerate(fruits):
        print(f"  {i}: {fruit}")
    
    # While loop
    print("While loop:")
    count = 0
    while count < 3:
        print(f"  Count: {count}")
        count += 1
    
    # List comprehensions
    squares = [x**2 for x in range(10)]
    evens = [x for x in range(20) if x % 2 == 0]
    
    print(f"Squares: {squares}")
    print(f"Evens: {evens}")
    
    # Dictionary comprehension
    square_dict = {x: x**2 for x in range(5)}
    print(f"Square dict: {square_dict}")

def test_exception_handling():
    """Test exception handling"""
    
    def divide_numbers(a: float, b: float) -> float:
        try:
            result = a / b
            return result
        except ZeroDivisionError:
            print(f"Error: Cannot divide {a} by zero")
            return float('inf')
        except TypeError as e:
            print(f"Type error: {e}")
            return 0.0
        finally:
            print(f"Division operation attempted: {a} / {b}")
    
    # Test normal division
    result1 = divide_numbers(10, 2)
    print(f"10 / 2 = {result1}")
    
    # Test division by zero
    result2 = divide_numbers(10, 0)
    print(f"10 / 0 = {result2}")
    
    # Custom exception
    class CustomError(Exception):
        def __init__(self, message: str):
            self.message = message
            super().__init__(self.message)
    
    def validate_age(age: int):
        if age < 0:
            raise CustomError("Age cannot be negative")
        if age > 150:
            raise CustomError("Age seems unrealistic")
        return True
    
    try:
        validate_age(-5)
    except CustomError as e:
        print(f"Custom error caught: {e.message}")

def test_file_operations():
    """Test file I/O operations"""
    filename = "test_python_file.txt"
    content = "Hello from Python file operations!\nLine 2\nLine 3"
    
    try:
        # Write to file
        with open(filename, 'w') as f:
            f.write(content)
        print(f"Successfully wrote to {filename}")
        
        # Read from file
        with open(filename, 'r') as f:
            read_content = f.read()
        print(f"Read from file:\n{read_content}")
        
        # Read lines
        with open(filename, 'r') as f:
            lines = f.readlines()
        print(f"Lines count: {len(lines)}")
        
        # Append to file
        with open(filename, 'a') as f:
            f.write("\nAppended line")
        
        # Read again
        with open(filename, 'r') as f:
            final_content = f.read()
        print(f"Final content:\n{final_content}")
        
    finally:
        # Clean up
        if os.path.exists(filename):
            os.remove(filename)
            print(f"Cleaned up {filename}")

def test_modules_and_imports():
    """Test standard library modules"""
    
    # Math module
    print(f"Math.pi: {math.pi:.6f}")
    print(f"Math.e: {math.e:.6f}")
    print(f"Math.sqrt(16): {math.sqrt(16)}")
    print(f"Math.factorial(5): {math.factorial(5)}")
    print(f"Math.gcd(48, 18): {math.gcd(48, 18)}")
    
    # Random module
    random.seed(42)  # For reproducible results
    print(f"Random int 1-10: {random.randint(1, 10)}")
    print(f"Random float: {random.random():.3f}")
    print(f"Random choice: {random.choice(['a', 'b', 'c', 'd'])}")
    
    sample_list = [1, 2, 3, 4, 5]
    random.shuffle(sample_list)
    print(f"Shuffled list: {sample_list}")
    
    # Datetime module
    now = datetime.datetime.now()
    print(f"Current time: {now.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Current year: {now.year}")
    print(f"Current month: {now.month}")
    
    # JSON module
    data = {
        "name": "Test Data",
        "numbers": [1, 2, 3, 4, 5],
        "nested": {"key": "value"}
    }
    json_string = json.dumps(data, indent=2)
    print(f"JSON string:\n{json_string}")
    
    parsed_data = json.loads(json_string)
    print(f"Parsed back: {parsed_data['name']}")

def test_itertools_collections():
    """Test itertools and collections modules"""
    
    # Collections
    counter = collections.Counter("hello world")
    print(f"Character count: {dict(counter)}")
    print(f"Most common: {counter.most_common(3)}")
    
    default_dict = collections.defaultdict(list)
    default_dict['fruits'].append('apple')
    default_dict['fruits'].append('banana')
    print(f"Default dict: {dict(default_dict)}")
    
    deque = collections.deque([1, 2, 3])
    deque.appendleft(0)
    deque.append(4)
    print(f"Deque: {list(deque)}")
    
    # Itertools
    print(f"Chain: {list(itertools.chain([1, 2], [3, 4]))}")
    print(f"Combinations: {list(itertools.combinations('ABCD', 2))}")
    print(f"Permutations: {list(itertools.permutations('ABC', 2))}")
    print(f"Product: {list(itertools.product('AB', '12'))}")
    
    # Take first 5 from infinite iterator
    count_iter = itertools.count(start=10, step=2)
    first_five = list(itertools.islice(count_iter, 5))
    print(f"Count iterator (first 5): {first_five}")

def test_decorators():
    """Test decorators and functools"""
    
    def timer_decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            start_time = time.time()
            result = func(*args, **kwargs)
            end_time = time.time()
            print(f"{func.__name__} took {end_time - start_time:.4f} seconds")
            return result
        return wrapper
    
    @timer_decorator
    def slow_function():
        time.sleep(0.01)  # Simulate slow operation
        return "Done"
    
    result = slow_function()
    print(f"Function result: {result}")
    
    # Functools.lru_cache
    @functools.lru_cache(maxsize=128)
    def fibonacci(n):
        if n < 2:
            return n
        return fibonacci(n-1) + fibonacci(n-2)
    
    print(f"Fibonacci(10): {fibonacci(10)}")
    print(f"Cache info: {fibonacci.cache_info()}")
    
    # Functools.reduce
    numbers = [1, 2, 3, 4, 5]
    product = functools.reduce(lambda x, y: x * y, numbers)
    print(f"Product of {numbers}: {product}")

def test_generators():
    """Test generators and yield"""
    
    def simple_generator():
        yield 1
        yield 2
        yield 3
    
    def fibonacci_generator(n):
        a, b = 0, 1
        for _ in range(n):
            yield a
            a, b = b, a + b
    
    def squares_generator(n):
        for i in range(n):
            yield i ** 2
    
    print("Simple generator:")
    for value in simple_generator():
        print(f"  {value}")
    
    print("Fibonacci generator (first 10):")
    fib_gen = fibonacci_generator(10)
    print(f"  {list(fib_gen)}")
    
    print("Squares generator (first 5):")
    squares = list(squares_generator(5))
    print(f"  {squares}")
    
    # Generator expression
    even_squares = (x**2 for x in range(10) if x % 2 == 0)
    print(f"Even squares: {list(even_squares)}")

def test_context_managers():
    """Test context managers and with statements"""
    
    class CustomContextManager:
        def __init__(self, name):
            self.name = name
        
        def __enter__(self):
            print(f"Entering {self.name}")
            return self
        
        def __exit__(self, exc_type, exc_val, exc_tb):
            print(f"Exiting {self.name}")
            return False
        
        def do_something(self):
            print(f"Doing something in {self.name}")
    
    print("Using custom context manager:")
    with CustomContextManager("TestContext") as cm:
        cm.do_something()
    
    # File context manager (already tested in file operations)
    print("Context manager ensures proper cleanup even with exceptions")

def test_type_hints_and_annotations():
    """Test type hints and annotations"""
    
    def process_data(
        items: List[int], 
        multiplier: float = 1.0,
        return_dict: bool = False
    ) -> Union[List[float], Dict[int, float]]:
        """Process a list of integers with type hints"""
        processed = [item * multiplier for item in items]
        
        if return_dict:
            return {i: val for i, val in enumerate(processed)}
        return processed
    
    # Test with list return
    result_list = process_data([1, 2, 3, 4], 2.5)
    print(f"Processed list: {result_list}")
    
    # Test with dict return
    result_dict = process_data([1, 2, 3], 3.0, return_dict=True)
    print(f"Processed dict: {result_dict}")
    
    # Optional and Union types
    def find_item(items: List[str], target: str) -> Optional[int]:
        try:
            return items.index(target)
        except ValueError:
            return None
    
    items = ["apple", "banana", "cherry"]
    index = find_item(items, "banana")
    print(f"Index of 'banana': {index}")
    
    missing_index = find_item(items, "orange")
    print(f"Index of 'orange': {missing_index}")

def test_advanced_features():
    """Test advanced Python features"""
    
    # Multiple inheritance
    class Flyable:
        def fly(self):
            return "Flying high!"
    
    class Swimmable:
        def swim(self):
            return "Swimming fast!"
    
    class Duck(Flyable, Swimmable):
        def __init__(self, name):
            self.name = name
        
        def quack(self):
            return f"{self.name} says quack!"
    
    duck = Duck("Donald")
    print(f"Duck quack: {duck.quack()}")
    print(f"Duck fly: {duck.fly()}")
    print(f"Duck swim: {duck.swim()}")
    
    # Property decorator
    class Circle:
        def __init__(self, radius):
            self._radius = radius
        
        @property
        def radius(self):
            return self._radius
        
        @radius.setter
        def radius(self, value):
            if value < 0:
                raise ValueError("Radius cannot be negative")
            self._radius = value
        
        @property
        def area(self):
            return math.pi * self._radius ** 2
        
        @property
        def circumference(self):
            return 2 * math.pi * self._radius
    
    circle = Circle(5)
    print(f"Circle radius: {circle.radius}")
    print(f"Circle area: {circle.area:.2f}")
    print(f"Circle circumference: {circle.circumference:.2f}")
    
    # Magic methods
    class Vector:
        def __init__(self, x, y):
            self.x = x
            self.y = y
        
        def __add__(self, other):
            return Vector(self.x + other.x, self.y + other.y)
        
        def __mul__(self, scalar):
            return Vector(self.x * scalar, self.y * scalar)
        
        def __str__(self):
            return f"Vector({self.x}, {self.y})"
        
        def __repr__(self):
            return self.__str__()
        
        def __len__(self):
            return int(math.sqrt(self.x**2 + self.y**2))
    
    v1 = Vector(3, 4)
    v2 = Vector(1, 2)
    v3 = v1 + v2
    v4 = v1 * 2
    
    print(f"Vector v1: {v1}")
    print(f"Vector v2: {v2}")
    print(f"v1 + v2: {v3}")
    print(f"v1 * 2: {v4}")
    print(f"Length of v1: {len(v1)}")

def main():
    """Main function to run all tests"""
    print("=== Python Comprehensive Test Suite ===")
    
    print("\n1. Basic Types Test:")
    test_basic_types()
    
    print("\n2. Collections Test:")
    test_collections()
    
    print("\n3. Functions Test:")
    test_functions()
    
    print("\n4. Classes Test:")
    test_classes()
    
    print("\n5. Control Flow Test:")
    test_control_flow()
    
    print("\n6. Exception Handling Test:")
    test_exception_handling()
    
    print("\n7. File Operations Test:")
    test_file_operations()
    
    print("\n8. Modules and Imports Test:")
    test_modules_and_imports()
    
    print("\n9. Itertools and Collections Test:")
    test_itertools_collections()
    
    print("\n10. Decorators Test:")
    test_decorators()
    
    print("\n11. Generators Test:")
    test_generators()
    
    print("\n12. Context Managers Test:")
    test_context_managers()
    
    print("\n13. Type Hints Test:")
    test_type_hints_and_annotations()
    
    print("\n14. Advanced Features Test:")
    test_advanced_features()
    
    print("\n=== All tests completed ===")

if __name__ == "__main__":
    main()