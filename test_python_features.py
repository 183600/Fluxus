# Test more Python features

# Basic arithmetic
a = 10
b = 20
sum_result = a + b
product = a * b

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

# List operations
numbers = [1, 2, 3, 4, 5]
doubled = [x * 2 for x in numbers]

# Dictionary
person = {"name": "Alice", "age": 30}
name = person["name"]

# Simple class
class Calculator:
    def add(self, x, y):
        return x + y
    
    def multiply(self, x, y):
        return x * y

calc = Calculator()
result1 = calc.add(5, 3)
result2 = calc.multiply(4, 7)

# Print results
print(f"Sum: {sum_result}")
print(f"Product: {product}")
print(f"Factorial(5): {factorial(5)}")
print(f"Doubled numbers: {doubled}")
print(f"Person name: {name}")
print(f"Calc add: {result1}")
print(f"Calc multiply: {result2}")