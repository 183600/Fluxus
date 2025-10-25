#!/usr/bin/env python3
"""
Advanced Calculator with scientific functions and memory operations
"""

import math
import operator
from typing import Union, Callable, Dict, Any

class ScientificCalculator:
    """A comprehensive calculator with basic and scientific operations."""
    
    def __init__(self):
        self.memory: float = 0.0
        self.history: list = []
        self.operations: Dict[str, Callable] = {
            '+': operator.add,
            '-': operator.sub,
            '*': operator.mul,
            '/': operator.truediv,
            '^': operator.pow,
            '%': operator.mod,
        }
    
    def add_to_history(self, operation: str, result: float) -> None:
        """Add operation to history."""
        self.history.append(f"{operation} = {result}")
    
    def basic_operation(self, a: float, op: str, b: float) -> float:
        """Perform basic arithmetic operations."""
        if op == '/' and b == 0:
            raise ValueError("Division by zero is not allowed")
        
        if op not in self.operations:
            raise ValueError(f"Unknown operation: {op}")
        
        result = self.operations[op](a, b)
        self.add_to_history(f"{a} {op} {b}", result)
        return result
    
    def square_root(self, x: float) -> float:
        """Calculate square root."""
        if x < 0:
            raise ValueError("Cannot calculate square root of negative number")
        result = math.sqrt(x)
        self.add_to_history(f"√{x}", result)
        return result
    
    def power(self, base: float, exponent: float) -> float:
        """Calculate power."""
        result = math.pow(base, exponent)
        self.add_to_history(f"{base}^{exponent}", result)
        return result
    
    def logarithm(self, x: float, base: float = math.e) -> float:
        """Calculate logarithm."""
        if x <= 0:
            raise ValueError("Logarithm undefined for non-positive numbers")
        if base <= 0 or base == 1:
            raise ValueError("Invalid logarithm base")
        
        if base == math.e:
            result = math.log(x)
            self.add_to_history(f"ln({x})", result)
        else:
            result = math.log(x, base)
            self.add_to_history(f"log_{base}({x})", result)
        return result
    
    def trigonometric(self, x: float, func: str) -> float:
        """Calculate trigonometric functions."""
        trig_functions = {
            'sin': math.sin,
            'cos': math.cos,
            'tan': math.tan,
            'asin': math.asin,
            'acos': math.acos,
            'atan': math.atan,
        }
        
        if func not in trig_functions:
            raise ValueError(f"Unknown trigonometric function: {func}")
        
        # Check domain for inverse functions
        if func in ['asin', 'acos'] and (x < -1 or x > 1):
            raise ValueError(f"{func} domain error: x must be in [-1, 1]")
        
        result = trig_functions[func](x)
        self.add_to_history(f"{func}({x})", result)
        return result
    
    def factorial(self, n: int) -> int:
        """Calculate factorial."""
        if n < 0:
            raise ValueError("Factorial is not defined for negative numbers")
        if n > 170:  # Prevent overflow
            raise ValueError("Number too large for factorial calculation")
        
        result = math.factorial(n)
        self.add_to_history(f"{n}!", result)
        return result
    
    def combination(self, n: int, r: int) -> int:
        """Calculate combinations (nCr)."""
        if n < 0 or r < 0:
            raise ValueError("n and r must be non-negative")
        if r > n:
            raise ValueError("r cannot be greater than n")
        
        result = math.comb(n, r)
        self.add_to_history(f"C({n},{r})", result)
        return result
    
    def permutation(self, n: int, r: int) -> int:
        """Calculate permutations (nPr)."""
        if n < 0 or r < 0:
            raise ValueError("n and r must be non-negative")
        if r > n:
            raise ValueError("r cannot be greater than n")
        
        result = math.perm(n, r)
        self.add_to_history(f"P({n},{r})", result)
        return result
    
    def store_memory(self, value: float) -> None:
        """Store value in memory."""
        self.memory = value
        print(f"Stored {value} in memory")
    
    def recall_memory(self) -> float:
        """Recall value from memory."""
        return self.memory
    
    def clear_memory(self) -> None:
        """Clear memory."""
        self.memory = 0.0
        print("Memory cleared")
    
    def add_to_memory(self, value: float) -> None:
        """Add value to memory."""
        self.memory += value
        print(f"Added {value} to memory. New memory value: {self.memory}")
    
    def get_history(self) -> list:
        """Get calculation history."""
        return self.history.copy()
    
    def clear_history(self) -> None:
        """Clear calculation history."""
        self.history.clear()
        print("History cleared")
    
    def degrees_to_radians(self, degrees: float) -> float:
        """Convert degrees to radians."""
        result = math.radians(degrees)
        self.add_to_history(f"{degrees}° to radians", result)
        return result
    
    def radians_to_degrees(self, radians: float) -> float:
        """Convert radians to degrees."""
        result = math.degrees(radians)
        self.add_to_history(f"{radians} rad to degrees", result)
        return result


def demonstrate_calculator():
    """Demonstrate calculator functionality."""
    calc = ScientificCalculator()
    
    print("=== Scientific Calculator Demo ===")
    
    # Basic operations
    print("\n--- Basic Operations ---")
    print(f"5 + 3 = {calc.basic_operation(5, '+', 3)}")
    print(f"10 - 4 = {calc.basic_operation(10, '-', 4)}")
    print(f"7 * 8 = {calc.basic_operation(7, '*', 8)}")
    print(f"15 / 3 = {calc.basic_operation(15, '/', 3)}")
    print(f"2 ^ 10 = {calc.basic_operation(2, '^', 10)}")
    
    # Scientific operations
    print("\n--- Scientific Operations ---")
    print(f"√16 = {calc.square_root(16)}")
    print(f"2^8 = {calc.power(2, 8)}")
    print(f"ln(10) = {calc.logarithm(10):.4f}")
    print(f"log₁₀(100) = {calc.logarithm(100, 10)}")
    
    # Trigonometric functions
    print("\n--- Trigonometric Functions ---")
    angle_rad = calc.degrees_to_radians(45)
    print(f"45° = {angle_rad:.4f} radians")
    print(f"sin(45°) = {calc.trigonometric(angle_rad, 'sin'):.4f}")
    print(f"cos(45°) = {calc.trigonometric(angle_rad, 'cos'):.4f}")
    print(f"tan(45°) = {calc.trigonometric(angle_rad, 'tan'):.4f}")
    
    # Combinatorics
    print("\n--- Combinatorics ---")
    print(f"5! = {calc.factorial(5)}")
    print(f"C(10,3) = {calc.combination(10, 3)}")
    print(f"P(10,3) = {calc.permutation(10, 3)}")
    
    # Memory operations
    print("\n--- Memory Operations ---")
    calc.store_memory(42)
    calc.add_to_memory(8)
    print(f"Memory recall: {calc.recall_memory()}")
    
    # History
    print("\n--- Calculation History ---")
    history = calc.get_history()
    for i, calculation in enumerate(history[-5:], 1):  # Show last 5
        print(f"{i}. {calculation}")
    
    # Error handling demonstration
    print("\n--- Error Handling ---")
    try:
        calc.basic_operation(10, '/', 0)
    except ValueError as e:
        print(f"Error caught: {e}")
    
    try:
        calc.square_root(-4)
    except ValueError as e:
        print(f"Error caught: {e}")


if __name__ == "__main__":
    demonstrate_calculator()