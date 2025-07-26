import math
import statistics
import json
from datetime import datetime, timedelta
from typing import List, Dict, Optional, Tuple

class AdvancedCalculator:
    """Advanced calculator with memory and scientific functions."""
    
    def __init__(self):
        self.memory = 0.0
        self.history = []
    
    def add(self, a: float, b: float) -> float:
        """Add two numbers."""
        result = a + b
        self._update_memory_and_history('add', [a, b], result)
        return result
    
    def subtract(self, a: float, b: float) -> float:
        """Subtract two numbers."""
        result = a - b
        self._update_memory_and_history('subtract', [a, b], result)
        return result
    
    def multiply(self, a: float, b: float) -> float:
        """Multiply two numbers."""
        result = a * b
        self._update_memory_and_history('multiply', [a, b], result)
        return result
    
    def divide(self, a: float, b: float) -> float:
        """Divide two numbers."""
        if b == 0:
            raise ValueError("Division by zero is not allowed")
        result = a / b
        self._update_memory_and_history('divide', [a, b], result)
        return result
    
    def power(self, base: float, exponent: float) -> float:
        """Raise base to the power of exponent."""
        result = base ** exponent
        self._update_memory_and_history('power', [base, exponent], result)
        return result
    
    def sqrt(self, x: float) -> float:
        """Calculate square root."""
        if x < 0:
            raise ValueError("Square root of negative number is not allowed")
        result = math.sqrt(x)
        self._update_memory_and_history('sqrt', [x], result)
        return result
    
    def log(self, x: float, base: float = math.e) -> float:
        """Calculate logarithm."""
        if x <= 0:
            raise ValueError("Logarithm of non-positive number is not allowed")
        if base <= 0 or base == 1:
            raise ValueError("Invalid logarithm base")
        
        if base == math.e:
            result = math.log(x)
        else:
            result = math.log(x) / math.log(base)
        
        self._update_memory_and_history('log', [x, base], result)
        return result
    
    def sin(self, x: float, degrees: bool = False) -> float:
        """Calculate sine."""
        if degrees:
            x = math.radians(x)
        result = math.sin(x)
        self._update_memory_and_history('sin', [x], result)
        return result
    
    def cos(self, x: float, degrees: bool = False) -> float:
        """Calculate cosine."""
        if degrees:
            x = math.radians(x)
        result = math.cos(x)
        self._update_memory_and_history('cos', [x], result)
        return result
    
    def tan(self, x: float, degrees: bool = False) -> float:
        """Calculate tangent."""
        if degrees:
            x = math.radians(x)
        result = math.tan(x)
        self._update_memory_and_history('tan', [x], result)
        return result
    
    def factorial(self, n: int) -> int:
        """Calculate factorial."""
        if n < 0:
            raise ValueError("Factorial of negative number is not defined")
        if n > 170:  # Prevent overflow
            raise ValueError("Number too large for factorial calculation")
        result = math.factorial(n)
        self._update_memory_and_history('factorial', [n], result)
        return result
    
    def fibonacci(self, n: int) -> int:
        """Calculate nth Fibonacci number."""
        if n < 0:
            raise ValueError("Fibonacci index must be non-negative")
        if n <= 1:
            return n
        
        a, b = 0, 1
        for _ in range(2, n + 1):
            a, b = b, a + b
        
        self._update_memory_and_history('fibonacci', [n], b)
        return b
    
    def gcd(self, a: int, b: int) -> int:
        """Calculate greatest common divisor."""
        result = math.gcd(a, b)
        self._update_memory_and_history('gcd', [a, b], result)
        return result
    
    def lcm(self, a: int, b: int) -> int:
        """Calculate least common multiple."""
        result = abs(a * b) // math.gcd(a, b) if a != 0 and b != 0 else 0
        self._update_memory_and_history('lcm', [a, b], result)
        return result
    
    def is_prime(self, n: int) -> bool:
        """Check if number is prime."""
        if n < 2:
            return False
        if n == 2:
            return True
        if n % 2 == 0:
            return False
        
        for i in range(3, int(math.sqrt(n)) + 1, 2):
            if n % i == 0:
                return False
        return True
    
    def get_memory(self) -> float:
        """Get current memory value."""
        return self.memory
    
    def clear_memory(self) -> None:
        """Clear memory."""
        self.memory = 0.0
    
    def get_history(self) -> List[Dict]:
        """Get calculation history."""
        return self.history.copy()
    
    def clear_history(self) -> None:
        """Clear calculation history."""
        self.history.clear()
    
    def _update_memory_and_history(self, operation: str, operands: List, result: float) -> None:
        """Update memory and history."""
        self.memory = result
        self.history.append({
            'timestamp': datetime.now().isoformat(),
            'operation': operation,
            'operands': operands,
            'result': result
        })

class StatisticalAnalyzer:
    """Statistical analysis tools."""
    
    def __init__(self, data: List[float]):
        self.data = data.copy()
    
    def mean(self) -> float:
        """Calculate arithmetic mean."""
        return statistics.mean(self.data) if self.data else 0.0
    
    def median(self) -> float:
        """Calculate median."""
        return statistics.median(self.data) if self.data else 0.0
    
    def mode(self) -> List[float]:
        """Calculate mode(s)."""
        try:
            return [statistics.mode(self.data)]
        except statistics.StatisticsError:
            # Multiple modes or no mode
            from collections import Counter
            counts = Counter(self.data)
            max_count = max(counts.values())
            return [value for value, count in counts.items() if count == max_count]
    
    def std_dev(self, population: bool = False) -> float:
        """Calculate standard deviation."""
        if len(self.data) < 2:
            return 0.0
        return statistics.pstdev(self.data) if population else statistics.stdev(self.data)
    
    def variance(self, population: bool = False) -> float:
        """Calculate variance."""
        if len(self.data) < 2:
            return 0.0
        return statistics.pvariance(self.data) if population else statistics.variance(self.data)
    
    def range(self) -> float:
        """Calculate range."""
        return max(self.data) - min(self.data) if self.data else 0.0
    
    def quartiles(self) -> Tuple[float, float, float]:
        """Calculate quartiles (Q1, Q2, Q3)."""
        if not self.data:
            return (0.0, 0.0, 0.0)
        
        sorted_data = sorted(self.data)
        n = len(sorted_data)
        
        q1 = statistics.median(sorted_data[:n//2])
        q2 = statistics.median(sorted_data)
        q3 = statistics.median(sorted_data[(n+1)//2:])
        
        return (q1, q2, q3)
    
    def percentile(self, p: float) -> float:
        """Calculate percentile."""
        if not (0 <= p <= 100):
            raise ValueError("Percentile must be between 0 and 100")
        
        if not self.data:
            return 0.0
        
        sorted_data = sorted(self.data)
        index = (p / 100) * (len(sorted_data) - 1)
        
        if index.is_integer():
            return sorted_data[int(index)]
        else:
            lower = sorted_data[int(index)]
            upper = sorted_data[int(index) + 1]
            return lower + (index - int(index)) * (upper - lower)
    
    def outliers(self, method: str = 'iqr') -> List[float]:
        """Detect outliers using IQR method."""
        if method != 'iqr':
            raise ValueError("Only 'iqr' method is currently supported")
        
        q1, q2, q3 = self.quartiles()
        iqr = q3 - q1
        lower_bound = q1 - 1.5 * iqr
        upper_bound = q3 + 1.5 * iqr
        
        return [x for x in self.data if x < lower_bound or x > upper_bound]
    
    def correlation(self, other_data: List[float]) -> float:
        """Calculate correlation coefficient with another dataset."""
        if len(self.data) != len(other_data):
            raise ValueError("Datasets must have the same length")
        
        return statistics.correlation(self.data, other_data)
    
    def summary(self) -> Dict:
        """Generate comprehensive statistical summary."""
        if not self.data:
            return {"error": "No data available"}
        
        q1, q2, q3 = self.quartiles()
        
        return {
            'count': len(self.data),
            'mean': self.mean(),
            'median': self.median(),
            'mode': self.mode(),
            'std_dev': self.std_dev(),
            'variance': self.variance(),
            'min': min(self.data),
            'max': max(self.data),
            'range': self.range(),
            'q1': q1,
            'q2': q2,
            'q3': q3,
            'iqr': q3 - q1,
            'outliers': self.outliers()
        }

def monte_carlo_pi(iterations: int = 1000000) -> float:
    """Estimate π using Monte Carlo method."""
    import random
    inside_circle = 0
    
    for _ in range(iterations):
        x = random.uniform(-1, 1)
        y = random.uniform(-1, 1)
        if x*x + y*y <= 1:
            inside_circle += 1
    
    return 4.0 * inside_circle / iterations

def numerical_integration(func, a: float, b: float, n: int = 1000) -> float:
    """Numerical integration using trapezoidal rule."""
    h = (b - a) / n
    result = (func(a) + func(b)) / 2.0
    
    for i in range(1, n):
        x = a + i * h
        result += func(x)
    
    return result * h

def newton_raphson(func, derivative, x0: float, tolerance: float = 1e-10, max_iterations: int = 100) -> Tuple[float, int]:
    """Find root using Newton-Raphson method."""
    x = x0
    for i in range(max_iterations):
        fx = func(x)
        if abs(fx) < tolerance:
            return x, i
        
        dfx = derivative(x)
        if abs(dfx) < 1e-15:
            raise ValueError("Derivative is zero")
        
        x = x - fx / dfx
    
    raise ValueError("Maximum iterations reached")

def generate_random_data(size: int, distribution: str = 'uniform', **kwargs) -> List[float]:
    """Generate random data with specified distribution."""
    import random
    
    if distribution == 'uniform':
        min_val = kwargs.get('min', 0)
        max_val = kwargs.get('max', 1)
        return [random.uniform(min_val, max_val) for _ in range(size)]
    
    elif distribution == 'normal':
        mu = kwargs.get('mu', 0)
        sigma = kwargs.get('sigma', 1)
        return [random.normalvariate(mu, sigma) for _ in range(size)]
    
    elif distribution == 'exponential':
        lambd = kwargs.get('lambd', 1)
        return [random.expovariate(lambd) for _ in range(size)]
    
    else:
        raise ValueError(f"Unsupported distribution: {distribution}")

def main():
    print("Advanced Scientific Calculator and Statistical Analyzer")
    print("=" * 55)
    
    # Calculator demo
    print("\n1. Calculator Operations:")
    calc = AdvancedCalculator()
    
    print(f"10 + 5 = {calc.add(10, 5)}")
    print(f"Memory: {calc.get_memory()}")
    
    print(f"20 - 8 = {calc.subtract(20, 8)}")
    print(f"Memory: {calc.get_memory()}")
    
    print(f"7 * 6 = {calc.multiply(7, 6)}")
    print(f"Memory: {calc.get_memory()}")
    
    try:
        print(f"100 / 4 = {calc.divide(100, 4)}")
        print(f"Memory: {calc.get_memory()}")
    except ValueError as e:
        print(f"Error: {e}")
    
    print(f"2^8 = {calc.power(2, 8)}")
    print(f"√144 = {calc.sqrt(144)}")
    print(f"sin(30°) = {calc.sin(30, degrees=True):.6f}")
    print(f"cos(60°) = {calc.cos(60, degrees=True):.6f}")
    print(f"ln(e) = {calc.log(math.e):.6f}")
    print(f"log₁₀(100) = {calc.log(100, 10):.6f}")
    
    print(f"5! = {calc.factorial(5)}")
    print(f"Fibonacci(10) = {calc.fibonacci(10)}")
    print(f"GCD(48, 18) = {calc.gcd(48, 18)}")
    print(f"LCM(12, 8) = {calc.lcm(12, 8)}")
    print(f"Is 17 prime? {calc.is_prime(17)}")
    print(f"Is 18 prime? {calc.is_prime(18)}")
    
    # Statistical analysis
    print("\n2. Statistical Analysis:")
    data = [23, 45, 56, 78, 32, 45, 67, 89, 23, 45, 67, 89, 12, 34, 56, 78, 90, 12, 34, 56]
    analyzer = StatisticalAnalyzer(data)
    
    print(f"Data: {data}")
    summary = analyzer.summary()
    
    print(f"Count: {summary['count']}")
    print(f"Mean: {summary['mean']:.2f}")
    print(f"Median: {summary['median']:.2f}")
    print(f"Mode: {summary['mode']}")
    print(f"Standard Deviation: {summary['std_dev']:.2f}")
    print(f"Variance: {summary['variance']:.2f}")
    print(f"Range: {summary['range']:.2f}")
    print(f"Q1: {summary['q1']:.2f}, Q2: {summary['q2']:.2f}, Q3: {summary['q3']:.2f}")
    print(f"IQR: {summary['iqr']:.2f}")
    print(f"Outliers: {summary['outliers']}")
    
    # Advanced mathematical computations
    print("\n3. Advanced Mathematical Computations:")
    
    # Monte Carlo π estimation
    pi_estimate = monte_carlo_pi(100000)
    print(f"π ≈ {pi_estimate:.6f} (Monte Carlo with 100k samples)")
    print(f"Error: {abs(math.pi - pi_estimate):.6f}")
    
    # Numerical integration
    # Integrate x^2 from 0 to 1 (should be 1/3)
    integral = numerical_integration(lambda x: x**2, 0, 1, 1000)
    print(f"∫₀¹ x² dx ≈ {integral:.6f} (exact: {1/3:.6f})")
    
    # Newton-Raphson root finding
    # Find root of x^2 - 2 = 0 (should be √2)
    f = lambda x: x**2 - 2
    df = lambda x: 2*x
    
    try:
        root, iterations = newton_raphson(f, df, 1.0)
        print(f"Root of x² - 2 = 0: {root:.10f} (found in {iterations} iterations)")
        print(f"Verification: f({root:.10f}) = {f(root):.2e}")
    except ValueError as e:
        print(f"Error in root finding: {e}")
    
    # Random data analysis
    print("\n4. Random Data Generation and Analysis:")
    
    # Uniform distribution
    uniform_data = generate_random_data(1000, 'uniform', min=0, max=100)
    uniform_analyzer = StatisticalAnalyzer(uniform_data)
    print(f"Uniform data (0-100): Mean={uniform_analyzer.mean():.2f}, StdDev={uniform_analyzer.std_dev():.2f}")
    
    # Normal distribution
    normal_data = generate_random_data(1000, 'normal', mu=50, sigma=15)
    normal_analyzer = StatisticalAnalyzer(normal_data)
    print(f"Normal data (μ=50, σ=15): Mean={normal_analyzer.mean():.2f}, StdDev={normal_analyzer.std_dev():.2f}")
    
    # Exponential distribution
    exp_data = generate_random_data(1000, 'exponential', lambd=0.1)
    exp_analyzer = StatisticalAnalyzer(exp_data)
    print(f"Exponential data (λ=0.1): Mean={exp_analyzer.mean():.2f}, StdDev={exp_analyzer.std_dev():.2f}")
    
    # Correlation analysis
    x_data = list(range(1, 21))
    y_data = [2*x + random.uniform(-5, 5) for x in x_data]  # Linear relationship with noise
    
    import random
    correlation = StatisticalAnalyzer(x_data).correlation(y_data)
    print(f"Correlation between x and y (linear relationship): {correlation:.3f}")
    
    # Mathematical constants and special functions
    print("\n5. Mathematical Constants and Special Functions:")
    print(f"π = {math.pi:.10f}")
    print(f"e = {math.e:.10f}")
    print(f"Golden ratio φ = {(1 + math.sqrt(5))/2:.10f}")
    print(f"√2 = {math.sqrt(2):.10f}")
    print(f"√3 = {math.sqrt(3):.10f}")
    
    x = 2.5
    print(f"\nFor x = {x}:")
    print(f"sin(x) = {math.sin(x):.6f}")
    print(f"cos(x) = {math.cos(x):.6f}")
    print(f"tan(x) = {math.tan(x):.6f}")
    print(f"sinh(x) = {math.sinh(x):.6f}")
    print(f"cosh(x) = {math.cosh(x):.6f}")
    print(f"tanh(x) = {math.tanh(x):.6f}")
    print(f"ln(x) = {math.log(x):.6f}")
    print(f"log₁₀(x) = {math.log10(x):.6f}")
    print(f"e^x = {math.exp(x):.6f}")
    print(f"Γ(x) = {math.gamma(x):.6f}")
    
    # Display calculation history
    print("\n6. Calculation History:")
    history = calc.get_history()
    print(f"Total calculations performed: {len(history)}")
    
    if history:
        print("Last 5 calculations:")
        for entry in history[-5:]:
            timestamp = entry['timestamp']
            operation = entry['operation']
            operands = entry['operands']
            result = entry['result']
            print(f"  {timestamp}: {operation}({operands}) = {result}")

if __name__ == "__main__":
    main()