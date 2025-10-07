#!/usr/bin/env python3
"""
Simple Python syntax demonstration for compiler testing
"""

def add_numbers(a, b):
    """Add two numbers"""
    return a + b

def factorial(n):
    """Calculate factorial"""
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

class Calculator:
    """Simple calculator class"""
    
    def __init__(self, name):
        self.name = name
        self.history = []
    
    def add(self, a, b):
        result = a + b
        self.history.append(f"add({a}, {b}) = {result}")
        return result
    
    def multiply(self, a, b):
        result = a * b
        self.history.append(f"multiply({a}, {b}) = {result}")
        return result
    
    def get_history(self):
        return self.history

def fibonacci_generator(n):
    """Generate fibonacci sequence"""
    a, b = 0, 1
    for i in range(n):
        yield a
        a, b = b, a + b

def list_comprehension_demo():
    """Demonstrate list comprehensions"""
    # Simple list comprehension
    squares = [x**2 for x in range(10)]
    
    # List comprehension with condition
    even_squares = [x**2 for x in range(10) if x % 2 == 0]
    
    # Nested list comprehension
    matrix = [[i*j for j in range(1, 4)] for i in range(1, 4)]
    
    return squares, even_squares, matrix

def dict_comprehension_demo():
    """Demonstrate dictionary comprehensions"""
    # Simple dict comprehension
    square_dict = {x: x**2 for x in range(5)}
    
    return square_dict

def exception_handling_demo(a, b):
    """Demonstrate exception handling"""
    try:
        result = a / b
        print(f"Division successful: {a} / {b} = {result}")
        return result
    except ZeroDivisionError:
        print("Error: Cannot divide by zero")
        return None
    except Exception as e:
        print(f"Error: {e}")
        return None

def main():
    """Main function"""
    print("=== Python Syntax Demo ===")
    
    # Basic function calls
    print(f"add_numbers(5, 3) = {add_numbers(5, 3)}")
    print(f"factorial(5) = {factorial(5)}")
    
    # Class usage
    calc = Calculator("MyCalc")
    print(f"calc.add(10, 5) = {calc.add(10, 5)}")
    print(f"calc.multiply(4, 7) = {calc.multiply(4, 7)}")
    print(f"Calculator history: {calc.get_history()}")
    
    # Generator
    fib_list = list(fibonacci_generator(10))
    print(f"Fibonacci(10): {fib_list}")
    
    # List comprehensions
    squares, even_squares, matrix = list_comprehension_demo()
    print(f"Squares: {squares}")
    print(f"Even squares: {even_squares}")
    print(f"Matrix: {matrix}")
    
    # Dictionary comprehension
    square_dict = dict_comprehension_demo()
    print(f"Square dict: {square_dict}")
    
    # Exception handling
    print("Exception handling:")
    exception_handling_demo(10, 2)
    exception_handling_demo(10, 0)
    
    print("\n=== Demo completed ===")

if __name__ == "__main__":
    main()