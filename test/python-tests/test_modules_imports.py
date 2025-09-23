# Test module imports and creation

# Create a simple module (in this file, but normally would be separate)
def module_function():
    return "Function from test module"

class ModuleClass:
    def __init__(self, name):
        self.name = name

    def greet(self):
        return f"Hello from {self.name}!"

MODULE_CONSTANT = "I am a constant"

# Import built-in modules
import math
import random
import datetime
import json

# Test math module
print("Math module tests:")
print(f"Pi: {math.pi}")
print(f"Square root of 16: {math.sqrt(16)}")
print(f"Power of 2^3: {math.pow(2, 3)}")
print(f"Factorial of 5: {math.factorial(5)}")

# Test random module
print("\nRandom module tests:")
print(f"Random number between 1-100: {random.randint(1, 100)}")
print(f"Random choice from list: {random.choice(['apple', 'banana', 'cherry'])}")
random_numbers = [random.random() for _ in range(3)]
print(f"Random floats: {random_numbers}")

# Test datetime module
print("\nDatetime module tests:")
current_time = datetime.datetime.now()
print(f"Current date and time: {current_time}")
print(f"Current date: {current_time.date()}")
print(f"Current time: {current_time.time()}")
print(f"Formatted date: {current_time.strftime('%Y-%m-%d %H:%M:%S')}")

# Test json module
print("\nJSON module tests:")
data = {
    "name": "Alice",
    "age": 30,
    "hobbies": ["reading", "swimming", "coding"],
    "active": True
}

json_string = json.dumps(data)
print(f"Data as JSON: {json_string}")

parsed_data = json.loads(json_string)
print(f"Parsed name: {parsed_data['name']}")
print(f"Parsed hobbies: {parsed_data['hobbies']}")

# Test import variations
from math import sqrt, pi
from random import choice

print(f"\nUsing specific imports:")
print(f"Square root of 25: {sqrt(25)}")
print(f"Pi value: {pi}")
print(f"Random choice: {choice(['red', 'green', 'blue'])}")

# Test module-level variables and functions
print(f"\nModule constant: {MODULE_CONSTANT}")
print(f"Module function: {module_function()}")
print(f"Module class: {ModuleClass('Test').greet()}")