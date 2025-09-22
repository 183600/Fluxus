# Comprehensive Python test for Fluxus compiler

# Basic operations
x = 10
y = 20
print("=== Basic Arithmetic ===")
print("Sum:", x + y)
print("Difference:", y - x)
print("Product:", x * y)
print("Quotient:", y / x)
print("Remainder:", y % x)
print("Power:", x ** 2)

# String operations
print("\n=== String Operations ===")
name = "Alice"
greeting = "Hello"
message = f"{greeting}, {name}!"
print(message)
print("Uppercase:", message.upper())
print("Length:", len(message))

# List operations
print("\n=== List Operations ===")
numbers = [1, 2, 3, 4, 5]
print("Original list:", numbers)
numbers.append(6)
print("After append:", numbers)
print("First element:", numbers[0])
print("Last element:", numbers[-1])
print("Slice:", numbers[1:4])

# Dictionary operations
print("\n=== Dictionary Operations ===")
person = {"name": "Bob", "age": 30, "city": "New York"}
print("Person:", person)
print("Name:", person["name"])
print("Keys:", list(person.keys()))
print("Values:", list(person.values()))

# Control flow
print("\n=== Control Flow ===")
for i in range(3):
    print(f"Loop iteration {i}")

count = 0
while count < 3:
    print(f"While loop count: {count}")
    count += 1

# Functions
def add(a, b):
    """Add two numbers"""
    return a + b

def multiply(a, b=2):
    """Multiply with default parameter"""
    return a * b

print("\n=== Functions ===")
print("add(5, 3):", add(5, 3))
print("multiply(4):", multiply(4))
print("multiply(4, 3):", multiply(4, 3))

# Classes
class Animal:
    def __init__(self, name):
        self.name = name
    
    def speak(self):
        return f"{self.name} makes a sound"

class Dog(Animal):
    def speak(self):
        return f"{self.name} barks"

print("\n=== Classes ===")
animal = Animal("Generic Animal")
dog = Dog("Buddy")
print(animal.speak())
print(dog.speak())

# Exception handling
print("\n=== Exception Handling ===")
try:
    result = 10 / 0
except ZeroDivisionError as e:
    print(f"Caught exception: {e}")
else:
    print("No exception occurred")
finally:
    print("Finally block executed")

# List comprehensions
print("\n=== List Comprehensions ===")
squares = [x**2 for x in range(5)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]
print("Squares:", squares)
print("Even squares:", even_squares)

# File operations
print("\n=== File Operations ===")
with open("test_output.txt", "w") as f:
    f.write("Hello from Fluxus!\n")
    f.write("This is a test file.\n")

with open("test_output.txt", "r") as f:
    content = f.read()
    print("File content:")
    print(content)

# Lambda functions
print("\n=== Lambda Functions ===")
square = lambda x: x**2
numbers = [1, 2, 3, 4, 5]
squared = list(map(square, numbers))
print("Original numbers:", numbers)
print("Squared numbers:", squared)

# Generator
print("\n=== Generator ===")
def count_up_to(n):
    i = 1
    while i <= n:
        yield i
        i += 1

print("Counting to 5:")
for num in count_up_to(5):
    print(num)

print("\n=== Test Completed ===")