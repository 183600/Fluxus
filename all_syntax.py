#!/usr/bin/env python3
"""
Comprehensive Python syntax demonstration file.
Contains all major Python syntax features.
"""

# 1. Variables and Basic Types
integer_var = 42
float_var = 3.14159
string_var = "Hello, World!"
boolean_var = True
none_var = None

# 2. Collections
list_var = [1, 2, 3, "four", 5.0]
tuple_var = (1, 2, 3, "immutable")
set_var = {1, 2, 3, 4, 5}
dict_var = {"key1": "value1", "key2": 42, "key3": [1, 2, 3]}

# 3. Control Flow
# If statements
if integer_var > 0:
    print("Positive")
elif integer_var < 0:
    print("Negative")
else:
    print("Zero")

# For loops
for i in range(5):
    print(f"For loop iteration {i}")

for index, value in enumerate(list_var):
    print(f"Index {index}: {value}")

# While loops
counter = 0
while counter < 3:
    print(f"While loop iteration {counter}")
    counter += 1

# Break and continue
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(f"Odd number: {i}")

# 4. Functions
def simple_function():
    return "Hello from function"

def function_with_params(a, b, c=10):
    return a + b + c

def function_with_types(a: int, b: str) -> str:
    return f"{a}: {b}"

def function_with_kwargs(*args, **kwargs):
    print(f"Args: {args}")
    print(f"Kwargs: {kwargs}")
    return len(args) + len(kwargs)

# Lambda functions
lambda_func = lambda x, y: x * y

# 5. Classes and Objects
class BaseClass:
    def __init__(self, value):
        self.value = value

    def method(self):
        return f"Base method: {self.value}"

class DerivedClass(BaseClass):
    def __init__(self, value, extra):
        super().__init__(value)
        self.extra = extra

    def method(self):
        return f"Derived method: {self.value}, {self.extra}"

    @classmethod
    def class_method(cls):
        return "Class method called"

    @staticmethod
    def static_method():
        return "Static method called"

# 6. Exception Handling
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Division by zero")
except Exception as e:
    print(f"Other error: {e}")
else:
    print("No error occurred")
finally:
    print("Finally block always executes")

# 7. File Operations
try:
    with open("temp_file.txt", "w") as f:
        f.write("Hello, file!")

    with open("temp_file.txt", "r") as f:
        content = f.read()
        print(f"File content: {content}")
except IOError as e:
    print(f"File error: {e}")

# 8. Imports and Modules
import math
from datetime import datetime, timedelta
import os as system_os
from collections import defaultdict

# Using imports
sqrt_result = math.sqrt(16)
current_time = datetime.now()
current_dir = system_os.getcwd()

# 9. List comprehensions and Generator expressions
squares = [x**2 for x in range(10)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]
gen_expr = (x**2 for x in range(5))

# 10. Context managers (with statement)
class ContextManager:
    def __enter__(self):
        print("Entering context")
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Exiting context")
        return False

with ContextManager():
    print("Inside context")

# 11. Decorators
def decorator_function(original_function):
    def wrapper(*args, **kwargs):
        print("Decorator before function call")
        result = original_function(*args, **kwargs)
        print("Decorator after function call")
        return result
    return wrapper

@decorator_function
def decorated_function(x, y):
    return x + y

# 12. Advanced data structures
nested_list = [[1, 2], [3, 4], [5, 6]]
nested_dict = {"outer": {"inner": "value"}}
matrix = [[(i + j) for j in range(3)] for i in range(3)]

# 13. String operations
formatted_string = f"Value: {integer_var}, Float: {float_var:.2f}"
raw_string = r"This is a raw string with \n newlines"
multi_line_string = """This is a
multi-line string"""

# 14. Bitwise operations
bitwise_and = 5 & 3
bitwise_or = 5 | 3
bitwise_xor = 5 ^ 3
bitwise_not = ~5
bitwise_shift_left = 5 << 2
bitwise_shift_right = 5 >> 1

# 15. Type annotations and advanced typing
from typing import List, Dict, Tuple, Optional, Union, Any

def typed_function(
    numbers: List[int],
    mapping: Dict[str, Any],
    optional_param: Optional[str] = None
) -> Tuple[int, str]:
    return (len(numbers), optional_param or "default")

# 16. Generators and iterators
def fibonacci_generator(n):
    a, b = 0, 1
    for _ in range(n):
        yield a
        a, b = b, a + b

# 17. Async/Await (Python 3.5+)
import asyncio

async def async_function():
    await asyncio.sleep(0.1)
    return "Async result"

# 18. Properties and descriptors
class PropertyExample:
    def __init__(self):
        self._value = 0

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, new_value):
        if new_value >= 0:
            self._value = new_value
        else:
            raise ValueError("Value must be non-negative")

# 19. Metaclasses
class MetaClass(type):
    def __new__(cls, name, bases, dct):
        dct['meta_attribute'] = 'meta_value'
        return super().__new__(cls, name, bases, dct)

class ClassWithMeta(metaclass=MetaClass):
    pass

# 20. Operator overloading
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

    def __str__(self):
        return f"Vector({self.x}, {self.y})"

# 21. Abstract base classes
from abc import ABC, abstractmethod

class AbstractShape(ABC):
    @abstractmethod
    def area(self):
        pass

    @abstractmethod
    def perimeter(self):
        pass

class Circle(AbstractShape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

    def perimeter(self):
        return 2 * math.pi * self.radius

# 22. Multiple inheritance
class A:
    def method_a(self):
        return "A"

class B:
    def method_b(self):
        return "B"

class C(A, B):
    def method_c(self):
        return "C"

# 23. Advanced exception handling
class CustomError(Exception):
    def __init__(self, message, code):
        super().__init__(message)
        self.code = code

# 24. Magic methods
class MagicMethods:
    def __len__(self):
        return 42

    def __getitem__(self, key):
        return f"Item at {key}"

    def __setitem__(self, key, value):
        print(f"Setting {key} to {value}")

    def __contains__(self, item):
        return True

    def __call__(self, *args, **kwargs):
        return f"Called with {args}, {kwargs}"

# 25. Main execution block
if __name__ == "__main__":
    print("=== Testing all Python syntax ===")

    # Test basic types
    print(f"Integer: {integer_var}")
    print(f"Float: {float_var}")
    print(f"String: {string_var}")
    print(f"Boolean: {boolean_var}")

    # Test collections
    print(f"List: {list_var}")
    print(f"Tuple: {tuple_var}")
    print(f"Set: {set_var}")
    print(f"Dict: {dict_var}")

    # Test functions
    print(f"Simple function: {simple_function()}")
    print(f"Function with params: {function_with_params(1, 2)}")
    print(f"Function with types: {function_with_types(42, 'test')}")
    print(f"Function with kwargs: {function_with_kwargs(1, 2, 3, a=4, b=5)}")
    print(f"Lambda function: {lambda_func(3, 4)}")

    # Test classes
    base_obj = BaseClass("base")
    derived_obj = DerivedClass("derived", "extra")
    print(f"Base class: {base_obj.method()}")
    print(f"Derived class: {derived_obj.method()}")
    print(f"Class method: {DerivedClass.class_method()}")
    print(f"Static method: {DerivedClass.static_method()}")

    # Test imports
    print(f"Square root: {sqrt_result}")
    print(f"Current time: {current_time}")
    print(f"Current directory: {current_dir}")

    # Test list comprehensions
    print(f"Squares: {squares}")
    print(f"Even squares: {even_squares}")

    # Test generators
    fib_gen = fibonacci_generator(10)
    print(f"Fibonacci: {list(fib_gen)}")

    # Test operator overloading
    v1 = Vector(1, 2)
    v2 = Vector(3, 4)
    print(f"Vector addition: {v1 + v2}")
    print(f"Vector subtraction: {v1 - v2}")
    print(f"Vector multiplication: {v1 * 2}")

    # Test abstract classes
    circle = Circle(5)
    print(f"Circle area: {circle.area()}")
    print(f"Circle perimeter: {circle.perimeter()}")

    # Test multiple inheritance
    c_obj = C()
    print(f"Multiple inheritance: {c_obj.method_a()}, {c_obj.method_b()}, {c_obj.method_c()}")

    # Test magic methods
    magic_obj = MagicMethods()
    print(f"Length: {len(magic_obj)}")
    print(f"Get item: {magic_obj[0]}")
    print(f"Contains: {'test' in magic_obj}")
    print(f"Call: {magic_obj(1, 2, a=3)}")

    # Test properties
    prop_obj = PropertyExample()
    prop_obj.value = 10
    print(f"Property value: {prop_obj.value}")

    # Test metaclasses
    meta_obj = ClassWithMeta()
    print(f"Meta attribute: {meta_obj.meta_attribute}")

    # Test decorators
    print(f"Decorated function: {decorated_function(5, 3)}")

    # Test formatted strings
    print(f"Formatted: {formatted_string}")
    print(f"Raw: {raw_string}")
    print(f"Multi-line: {multi_line_string}")

    # Test bitwise operations
    print(f"Bitwise AND: {bitwise_and}")
    print(f"Bitwise OR: {bitwise_or}")
    print(f"Bitwise XOR: {bitwise_xor}")
    print(f"Bitwise NOT: {bitwise_not}")
    print(f"Bitwise shift left: {bitwise_shift_left}")
    print(f"Bitwise shift right: {bitwise_shift_right}")

    # Test typed functions
    print(f"Typed function: {typed_function([1, 2, 3], {'key': 'value'})}")

    # Test nested data structures
    print(f"Nested list: {nested_list}")
    print(f"Nested dict: {nested_dict}")
    print(f"Matrix: {matrix}")

    print("=== All syntax tests completed ===")