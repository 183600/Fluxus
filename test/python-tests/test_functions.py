# Test function definitions and calls
def greet(name):
    return f"Hello, {name}!"

def add_numbers(a, b):
    return a + b

def multiply_numbers(a, b):
    return a * b

def calculate_average(numbers):
    return sum(numbers) / len(numbers)

# Test function calls
result1 = greet("World")
result2 = add_numbers(5, 3)
result3 = multiply_numbers(4, 7)
result4 = calculate_average([1, 2, 3, 4, 5])

print("Greeting:", result1)
print("Addition:", result2)
print("Multiplication:", result3)
print("Average:", result4)

# Test lambda functions
double = lambda x: x * 2
square = lambda x: x ** 2

print("Double of 10:", double(10))
print("Square of 4:", square(4))