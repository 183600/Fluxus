#!/usr/bin/env python3
"""
Advanced Mathematical Operations and Scientific Computing in Python
"""

import math
import cmath
import numpy as np
import random
import time
from typing import List, Tuple, Union, Optional, Callable
from dataclasses import dataclass
from functools import lru_cache
import matplotlib.pyplot as plt
from scipy import optimize, integrate, stats
import sympy as sp


class Matrix:
    """Matrix operations implementation."""
    
    def __init__(self, data: List[List[float]]):
        self.data = data
        self.rows = len(data)
        self.cols = len(data[0]) if data else 0
        
        # Validate matrix dimensions
        for row in data:
            if len(row) != self.cols:
                raise ValueError("All rows must have the same number of columns")
    
    def __getitem__(self, key):
        return self.data[key]
    
    def __setitem__(self, key, value):
        self.data[key] = value
    
    def __add__(self, other: 'Matrix') -> 'Matrix':
        if self.rows != other.rows or self.cols != other.cols:
            raise ValueError("Matrices must have the same dimensions for addition")
        
        result = []
        for i in range(self.rows):
            row = []
            for j in range(self.cols):
                row.append(self.data[i][j] + other.data[i][j])
            result.append(row)
        
        return Matrix(result)
    
    def __sub__(self, other: 'Matrix') -> 'Matrix':
        if self.rows != other.rows or self.cols != other.cols:
            raise ValueError("Matrices must have the same dimensions for subtraction")
        
        result = []
        for i in range(self.rows):
            row = []
            for j in range(self.cols):
                row.append(self.data[i][j] - other.data[i][j])
            result.append(row)
        
        return Matrix(result)
    
    def __mul__(self, other: Union['Matrix', float]) -> 'Matrix':
        if isinstance(other, (int, float)):
            # Scalar multiplication
            result = []
            for i in range(self.rows):
                row = []
                for j in range(self.cols):
                    row.append(self.data[i][j] * other)
                result.append(row)
            return Matrix(result)
        
        elif isinstance(other, Matrix):
            # Matrix multiplication
            if self.cols != other.rows:
                raise ValueError("Number of columns in first matrix must equal number of rows in second matrix")
            
            result = []
            for i in range(self.rows):
                row = []
                for j in range(other.cols):
                    sum_val = 0
                    for k in range(self.cols):
                        sum_val += self.data[i][k] * other.data[k][j]
                    row.append(sum_val)
                result.append(row)
            
            return Matrix(result)
        
        else:
            raise TypeError("Can only multiply matrix by scalar or another matrix")
    
    def transpose(self) -> 'Matrix':
        result = []
        for j in range(self.cols):
            row = []
            for i in range(self.rows):
                row.append(self.data[i][j])
            result.append(row)
        
        return Matrix(result)
    
    def determinant(self) -> float:
        if self.rows != self.cols:
            raise ValueError("Determinant can only be calculated for square matrices")
        
        if self.rows == 1:
            return self.data[0][0]
        
        if self.rows == 2:
            return self.data[0][0] * self.data[1][1] - self.data[0][1] * self.data[1][0]
        
        det = 0
        for j in range(self.cols):
            minor = self.get_minor(0, j)
            sign = (-1) ** j
            det += sign * self.data[0][j] * minor.determinant()
        
        return det
    
    def get_minor(self, skip_row: int, skip_col: int) -> 'Matrix':
        result = []
        for i in range(self.rows):
            if i == skip_row:
                continue
            row = []
            for j in range(self.cols):
                if j == skip_col:
                    continue
                row.append(self.data[i][j])
            result.append(row)
        
        return Matrix(result)
    
    def inverse(self) -> 'Matrix':
        det = self.determinant()
        if abs(det) < 1e-10:
            raise ValueError("Matrix is not invertible (determinant is zero)")
        
        if self.rows == 2:
            return Matrix([
                [self.data[1][1] / det, -self.data[0][1] / det],
                [-self.data[1][0] / det, self.data[0][0] / det]
            ])
        
        # For larger matrices, use adjugate method
        adjugate = []
        for i in range(self.rows):
            row = []
            for j in range(self.cols):
                minor = self.get_minor(i, j)
                cofactor = ((-1) ** (i + j)) * minor.determinant()
                row.append(cofactor)
            adjugate.append(row)
        
        adjugate_matrix = Matrix(adjugate).transpose()
        return adjugate_matrix * (1 / det)
    
    def __str__(self) -> str:
        result = "[\n"
        for row in self.data:
            result += "  " + str([f"{x:8.2f}" for x in row]) + "\n"
        result += "]"
        return result


class Complex:
    """Complex number operations."""
    
    def __init__(self, real: float, imag: float = 0):
        self.real = real
        self.imag = imag
    
    def __add__(self, other: 'Complex') -> 'Complex':
        return Complex(self.real + other.real, self.imag + other.imag)
    
    def __sub__(self, other: 'Complex') -> 'Complex':
        return Complex(self.real - other.real, self.imag - other.imag)
    
    def __mul__(self, other: Union['Complex', float]) -> 'Complex':
        if isinstance(other, Complex):
            real = self.real * other.real - self.imag * other.imag
            imag = self.real * other.imag + self.imag * other.real
            return Complex(real, imag)
        else:
            return Complex(self.real * other, self.imag * other)
    
    def __truediv__(self, other: 'Complex') -> 'Complex':
        denominator = other.real**2 + other.imag**2
        real = (self.real * other.real + self.imag * other.imag) / denominator
        imag = (self.imag * other.real - self.real * other.imag) / denominator
        return Complex(real, imag)
    
    def magnitude(self) -> float:
        return math.sqrt(self.real**2 + self.imag**2)
    
    def phase(self) -> float:
        return math.atan2(self.imag, self.real)
    
    def conjugate(self) -> 'Complex':
        return Complex(self.real, -self.imag)
    
    def __str__(self) -> str:
        if self.imag >= 0:
            return f"{self.real:.2f} + {self.imag:.2f}i"
        else:
            return f"{self.real:.2f} - {abs(self.imag):.2f}i"


class Statistics:
    """Advanced statistical operations."""
    
    def __init__(self, data: List[float]):
        self.data = sorted(data)
        self.n = len(data)
    
    def mean(self) -> float:
        return sum(self.data) / self.n if self.n > 0 else 0
    
    def median(self) -> float:
        if self.n == 0:
            return 0
        
        if self.n % 2 == 0:
            return (self.data[self.n // 2 - 1] + self.data[self.n // 2]) / 2
        else:
            return self.data[self.n // 2]
    
    def mode(self) -> List[float]:
        if self.n == 0:
            return []
        
        freq = {}
        for value in self.data:
            freq[value] = freq.get(value, 0) + 1
        
        max_freq = max(freq.values())
        return [value for value, count in freq.items() if count == max_freq]
    
    def variance(self) -> float:
        if self.n <= 1:
            return 0
        
        mean_val = self.mean()
        return sum((x - mean_val)**2 for x in self.data) / (self.n - 1)
    
    def std_deviation(self) -> float:
        return math.sqrt(self.variance())
    
    def quartiles(self) -> Tuple[float, float, float]:
        if self.n == 0:
            return (0, 0, 0)
        
        q1_idx = self.n // 4
        q2_idx = self.n // 2
        q3_idx = 3 * self.n // 4
        
        return (self.data[q1_idx], self.data[q2_idx], self.data[q3_idx])
    
    def iqr(self) -> float:
        q1, _, q3 = self.quartiles()
        return q3 - q1
    
    def outliers(self) -> List[float]:
        q1, _, q3 = self.quartiles()
        iqr_val = self.iqr()
        lower_bound = q1 - 1.5 * iqr_val
        upper_bound = q3 + 1.5 * iqr_val
        
        return [x for x in self.data if x < lower_bound or x > upper_bound]
    
    def correlation(self, other_data: List[float]) -> float:
        if len(other_data) != self.n or self.n <= 1:
            return 0
        
        mean1 = self.mean()
        mean2 = sum(other_data) / len(other_data)
        
        numerator = sum((self.data[i] - mean1) * (other_data[i] - mean2) for i in range(self.n))
        denominator = math.sqrt(
            sum((x - mean1)**2 for x in self.data) * 
            sum((x - mean2)**2 for x in other_data)
        )
        
        return numerator / denominator if denominator != 0 else 0


class NumericalMethods:
    """Numerical computation methods."""
    
    @staticmethod
    def newton_raphson(f: Callable[[float], float], 
                      df: Callable[[float], float], 
                      x0: float, 
                      tolerance: float = 1e-10, 
                      max_iterations: int = 100) -> float:
        """Newton-Raphson method for finding roots."""
        x = x0
        for _ in range(max_iterations):
            fx = f(x)
            if abs(fx) < tolerance:
                return x
            
            dfx = df(x)
            if abs(dfx) < 1e-15:
                raise ValueError("Derivative is too close to zero")
            
            x = x - fx / dfx
        
        raise ValueError("Newton-Raphson method did not converge")
    
    @staticmethod
    def bisection_method(f: Callable[[float], float], 
                        a: float, 
                        b: float, 
                        tolerance: float = 1e-10, 
                        max_iterations: int = 100) -> float:
        """Bisection method for finding roots."""
        if f(a) * f(b) > 0:
            raise ValueError("Function must have different signs at endpoints")
        
        for _ in range(max_iterations):
            c = (a + b) / 2
            fc = f(c)
            
            if abs(fc) < tolerance or abs(b - a) < tolerance:
                return c
            
            if f(a) * fc < 0:
                b = c
            else:
                a = c
        
        raise ValueError("Bisection method did not converge")
    
    @staticmethod
    def trapezoidal_rule(f: Callable[[float], float], 
                        a: float, 
                        b: float, 
                        n: int = 1000) -> float:
        """Numerical integration using trapezoidal rule."""
        h = (b - a) / n
        result = (f(a) + f(b)) / 2
        
        for i in range(1, n):
            x = a + i * h
            result += f(x)
        
        return result * h
    
    @staticmethod
    def simpson_rule(f: Callable[[float], float], 
                    a: float, 
                    b: float, 
                    n: int = 1000) -> float:
        """Numerical integration using Simpson's rule."""
        if n % 2 != 0:
            n += 1  # Make n even
        
        h = (b - a) / n
        result = f(a) + f(b)
        
        for i in range(1, n):
            x = a + i * h
            if i % 2 == 0:
                result += 2 * f(x)
            else:
                result += 4 * f(x)
        
        return result * h / 3
    
    @staticmethod
    def gradient_descent(f: Callable[[List[float]], float],
                        grad_f: Callable[[List[float]], List[float]],
                        x0: List[float],
                        learning_rate: float = 0.01,
                        tolerance: float = 1e-6,
                        max_iterations: int = 10000) -> List[float]:
        """Gradient descent optimization."""
        x = x0.copy()
        
        for _ in range(max_iterations):
            grad = grad_f(x)
            
            # Check convergence
            if sum(g**2 for g in grad)**0.5 < tolerance:
                break
            
            # Update parameters
            for i in range(len(x)):
                x[i] -= learning_rate * grad[i]
        
        return x


class Polynomial:
    """Polynomial operations."""
    
    def __init__(self, coefficients: List[float]):
        # Remove leading zeros
        while len(coefficients) > 1 and coefficients[0] == 0:
            coefficients.pop(0)
        self.coefficients = coefficients
        self.degree = len(coefficients) - 1
    
    def evaluate(self, x: float) -> float:
        result = 0
        power = self.degree
        for coeff in self.coefficients:
            result += coeff * (x ** power)
            power -= 1
        return result
    
    def derivative(self) -> 'Polynomial':
        if self.degree == 0:
            return Polynomial([0])
        
        new_coeffs = []
        power = self.degree
        for coeff in self.coefficients[:-1]:
            new_coeffs.append(coeff * power)
            power -= 1
        
        return Polynomial(new_coeffs)
    
    def integral(self, constant: float = 0) -> 'Polynomial':
        new_coeffs = []
        power = self.degree + 1
        
        for coeff in self.coefficients:
            new_coeffs.append(coeff / power)
            power -= 1
        
        new_coeffs.append(constant)
        return Polynomial(new_coeffs)
    
    def __add__(self, other: 'Polynomial') -> 'Polynomial':
        # Pad shorter polynomial with zeros
        max_degree = max(self.degree, other.degree)
        
        coeffs1 = [0] * (max_degree - self.degree) + self.coefficients
        coeffs2 = [0] * (max_degree - other.degree) + other.coefficients
        
        result_coeffs = [c1 + c2 for c1, c2 in zip(coeffs1, coeffs2)]
        return Polynomial(result_coeffs)
    
    def __mul__(self, other: Union['Polynomial', float]) -> 'Polynomial':
        if isinstance(other, (int, float)):
            return Polynomial([coeff * other for coeff in self.coefficients])
        
        elif isinstance(other, Polynomial):
            result_degree = self.degree + other.degree
            result_coeffs = [0] * (result_degree + 1)
            
            for i, c1 in enumerate(self.coefficients):
                for j, c2 in enumerate(other.coefficients):
                    result_coeffs[i + j] += c1 * c2
            
            return Polynomial(result_coeffs)
    
    def __str__(self) -> str:
        if not self.coefficients:
            return "0"
        
        terms = []
        power = self.degree
        
        for coeff in self.coefficients:
            if coeff == 0:
                power -= 1
                continue
            
            if power == 0:
                terms.append(f"{coeff:g}")
            elif power == 1:
                if coeff == 1:
                    terms.append("x")
                elif coeff == -1:
                    terms.append("-x")
                else:
                    terms.append(f"{coeff:g}x")
            else:
                if coeff == 1:
                    terms.append(f"x^{power}")
                elif coeff == -1:
                    terms.append(f"-x^{power}")
                else:
                    terms.append(f"{coeff:g}x^{power}")
            
            power -= 1
        
        if not terms:
            return "0"
        
        result = terms[0]
        for term in terms[1:]:
            if term.startswith('-'):
                result += f" - {term[1:]}"
            else:
                result += f" + {term}"
        
        return result


def fibonacci_sequence(n: int) -> List[int]:
    """Generate Fibonacci sequence up to n terms."""
    if n <= 0:
        return []
    elif n == 1:
        return [0]
    elif n == 2:
        return [0, 1]
    
    fib = [0, 1]
    for i in range(2, n):
        fib.append(fib[i-1] + fib[i-2])
    
    return fib


@lru_cache(maxsize=None)
def fibonacci_recursive(n: int) -> int:
    """Calculate nth Fibonacci number recursively with memoization."""
    if n <= 1:
        return n
    return fibonacci_recursive(n-1) + fibonacci_recursive(n-2)


def prime_factorization(n: int) -> List[int]:
    """Find prime factorization of a number."""
    factors = []
    d = 2
    
    while d * d <= n:
        while n % d == 0:
            factors.append(d)
            n //= d
        d += 1
    
    if n > 1:
        factors.append(n)
    
    return factors


def sieve_of_eratosthenes(limit: int) -> List[int]:
    """Generate all prime numbers up to limit using Sieve of Eratosthenes."""
    if limit < 2:
        return []
    
    sieve = [True] * (limit + 1)
    sieve[0] = sieve[1] = False
    
    for i in range(2, int(math.sqrt(limit)) + 1):
        if sieve[i]:
            for j in range(i * i, limit + 1, i):
                sieve[j] = False
    
    return [i for i in range(2, limit + 1) if sieve[i]]


def gcd(a: int, b: int) -> int:
    """Calculate Greatest Common Divisor using Euclidean algorithm."""
    while b:
        a, b = b, a % b
    return a


def lcm(a: int, b: int) -> int:
    """Calculate Least Common Multiple."""
    return abs(a * b) // gcd(a, b)


def benchmark_mathematical_operations():
    """Benchmark various mathematical operations."""
    print("\n=== Mathematical Operations Benchmark ===")
    
    # Matrix operations benchmark
    print("\nMatrix Operations:")
    sizes = [50, 100, 200]
    
    for size in sizes:
        # Generate random matrices
        data1 = [[random.uniform(-10, 10) for _ in range(size)] for _ in range(size)]
        data2 = [[random.uniform(-10, 10) for _ in range(size)] for _ in range(size)]
        
        m1 = Matrix(data1)
        m2 = Matrix(data2)
        
        # Benchmark matrix multiplication
        start_time = time.time()
        result = m1 * m2
        end_time = time.time()
        
        print(f"  {size}x{size} matrix multiplication: {end_time - start_time:.4f}s")
    
    # Numerical methods benchmark
    print("\nNumerical Methods:")
    
    # Test function: x^3 - 2x - 5 = 0
    test_func = lambda x: x**3 - 2*x - 5
    test_func_derivative = lambda x: 3*x**2 - 2
    
    methods = [
        ("Newton-Raphson", lambda: NumericalMethods.newton_raphson(test_func, test_func_derivative, 2.0)),
        ("Bisection", lambda: NumericalMethods.bisection_method(test_func, 2.0, 3.0))
    ]
    
    for name, method in methods:
        start_time = time.time()
        root = method()
        end_time = time.time()
        
        print(f"  {name}: root = {root:.6f}, time = {end_time - start_time:.6f}s")
    
    # Integration benchmark
    print("\nNumerical Integration:")
    
    # Test function: sin(x) from 0 to π (should be 2)
    test_integral = lambda x: math.sin(x)
    
    integration_methods = [
        ("Trapezoidal", lambda: NumericalMethods.trapezoidal_rule(test_integral, 0, math.pi, 10000)),
        ("Simpson", lambda: NumericalMethods.simpson_rule(test_integral, 0, math.pi, 10000))
    ]
    
    for name, method in integration_methods:
        start_time = time.time()
        result = method()
        end_time = time.time()
        
        error = abs(result - 2.0)
        print(f"  {name}: result = {result:.6f}, error = {error:.6f}, time = {end_time - start_time:.6f}s")


def main():
    """Main function demonstrating mathematical operations."""
    print("=== Advanced Mathematical Operations Demo ===")
    
    # 1. Matrix Operations
    print("\n1. Matrix Operations:")
    
    m1 = Matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    m2 = Matrix([[9, 8, 7], [6, 5, 4], [3, 2, 1]])
    
    print(f"Matrix 1:{m1}")
    print(f"Matrix 2:{m2}")
    
    print(f"Addition:{m1 + m2}")
    print(f"Multiplication:{m1 * m2}")
    print(f"Transpose of Matrix 1:{m1.transpose()}")
    
    # Invertible matrix example
    m3 = Matrix([[2, 1], [1, 1]])
    print(f"Matrix 3:{m3}")
    print(f"Determinant: {m3.determinant():.2f}")
    print(f"Inverse:{m3.inverse()}")
    
    # 2. Complex Numbers
    print("\n2. Complex Number Operations:")
    c1 = Complex(3, 4)
    c2 = Complex(1, -2)
    
    print(f"c1 = {c1}")
    print(f"c2 = {c2}")
    print(f"c1 + c2 = {c1 + c2}")
    print(f"c1 * c2 = {c1 * c2}")
    print(f"c1 / c2 = {c1 / c2}")
    print(f"|c1| = {c1.magnitude():.2f}")
    print(f"arg(c1) = {c1.phase():.2f} radians")
    
    # 3. Statistics
    print("\n3. Statistical Analysis:")
    data = [2.3, 1.5, 3.7, 2.1, 4.2, 1.8, 3.9, 2.7, 3.1, 2.9, 4.5, 1.2, 3.3]
    stats = Statistics(data)
    
    print(f"Data: {data}")
    print(f"Mean: {stats.mean():.2f}")
    print(f"Median: {stats.median():.2f}")
    print(f"Mode: {stats.mode()}")
    print(f"Standard Deviation: {stats.std_deviation():.2f}")
    print(f"Quartiles (Q1, Q2, Q3): {stats.quartiles()}")
    print(f"IQR: {stats.iqr():.2f}")
    print(f"Outliers: {stats.outliers()}")
    
    # 4. Numerical Methods
    print("\n4. Numerical Methods:")
    
    # Find root of x^2 - 2 = 0 (should be √2)
    f = lambda x: x**2 - 2
    df = lambda x: 2*x
    
    root = NumericalMethods.newton_raphson(f, df, 1.0)
    print(f"Root of x² - 2 = 0: {root:.10f} (√2 ≈ {math.sqrt(2):.10f})")
    
    # Numerical integration of x^2 from 0 to 2 (should be 8/3)
    integrand = lambda x: x**2
    trapezoidal_result = NumericalMethods.trapezoidal_rule(integrand, 0, 2, 10000)
    simpson_result = NumericalMethods.simpson_rule(integrand, 0, 2, 10000)
    exact_result = 8/3
    
    print(f"∫₀² x² dx:")
    print(f"  Trapezoidal: {trapezoidal_result:.6f}")
    print(f"  Simpson's: {simpson_result:.6f}")
    print(f"  Exact: {exact_result:.6f}")
    
    # 5. Polynomial Operations
    print("\n5. Polynomial Operations:")
    
    # p(x) = 2x³ - 3x² + 4x - 1
    p1 = Polynomial([2, -3, 4, -1])
    # q(x) = x² + 2x + 1
    p2 = Polynomial([1, 2, 1])
    
    print(f"p(x) = {p1}")
    print(f"q(x) = {p2}")
    print(f"p(x) + q(x) = {p1 + p2}")
    print(f"p(x) * q(x) = {p1 * p2}")
    print(f"p'(x) = {p1.derivative()}")
    print(f"∫p(x)dx = {p1.integral()}")
    print(f"p(2) = {p1.evaluate(2)}")
    
    # 6. Number Theory
    print("\n6. Number Theory:")
    
    # Fibonacci sequence
    fib_seq = fibonacci_sequence(15)
    print(f"Fibonacci sequence (15 terms): {fib_seq}")
    print(f"21st Fibonacci number: {fibonacci_recursive(21)}")
    
    # Prime numbers
    primes = sieve_of_eratosthenes(50)
    print(f"Prime numbers up to 50: {primes}")
    
    # Prime factorization
    number = 315
    factors = prime_factorization(number)
    print(f"Prime factorization of {number}: {factors}")
    
    # GCD and LCM
    a, b = 48, 18
    print(f"GCD({a}, {b}) = {gcd(a, b)}")
    print(f"LCM({a}, {b}) = {lcm(a, b)}")
    
    # 7. Benchmark
    benchmark_mathematical_operations()
    
    print("\n=== Demo completed ===")


if __name__ == "__main__":
    main()