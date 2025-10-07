#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Another Python test file for directory scanning
"""

import math
import sys

def fibonacci(n):
    """Calculate Fibonacci sequence"""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

def factorial(n):
    """Calculate factorial"""
    if n < 0:
        raise ValueError("Negative factorial not defined")
    if n == 0 or n == 1:
        return 1
    return n * factorial(n - 1)

def main():
    """Main function"""
    print("Advanced test file executed successfully!")

    # Test mathematical functions
    fib_result = fibonacci(10)
    fact_result = factorial(5)

    print(f"Fibonacci(10) = {fib_result}")
    print(f"Factorial(5) = {fact_result}")
    print(f"Square root of 16 = {math.sqrt(16)}")

    # Test with lists and comprehensions
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    squares = [x**2 for x in numbers]
    even_numbers = [x for x in numbers if x % 2 == 0]

    print(f"Numbers: {numbers}")
    print(f"Squares: {squares}")
    print(f"Even numbers: {even_numbers}")

    # Test with dictionary operations
    word_counts = {"hello": 3, "world": 2, "python": 5}
    for word, count in word_counts.items():
        print(f"'{word}' appears {count} times")

    print("All advanced functionality works!")

if __name__ == "__main__":
    main()