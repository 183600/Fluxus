#!/usr/bin/env python3

# Test Python compilation with various features
def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def greet(name):
    return f"Hello, {name}!"

def main():
    # Variables and basic arithmetic
    x = 10
    y = 20
    result = x + y
    print(f"Result: {result}")
    
    # Function calls
    fib_result = fibonacci(8)
    print(f"Fibonacci(8): {fib_result}")
    
    # String operations
    greeting = greet("World")
    print(greeting)
    
    # Lists
    numbers = [1, 2, 3, 4, 5]
    print(f"Numbers: {numbers}")
    
    # Loops
    for i in range(5):
        print(f"Loop iteration: {i}")

if __name__ == "__main__":
    main()