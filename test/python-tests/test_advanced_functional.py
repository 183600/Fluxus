#!/usr/bin/env python3
"""
Advanced Python Functional Programming Patterns
Including advanced itertools, functools, operator usage, and functional patterns
"""

import itertools
import functools
import operator
import collections
from typing import Any, Callable, Iterable, List, Dict, Tuple, Optional, TypeVar, Generic
from dataclasses import dataclass
import heapq
import random
import time

T = TypeVar('T')
U = TypeVar('U')

# Advanced Itertools Patterns

def test_advanced_combinatorics():
    """Test advanced combinatorial functions"""
    print("=== Advanced Combinatorics ===")
    
    # Cartesian product with repeat
    colors = ['red', 'blue']
    sizes = ['S', 'M', 'L']
    materials = ['cotton', 'silk']
    
    # All combinations of products
    all_combinations = list(itertools.product(colors, sizes, materials))
    print(f"All product combinations: {len(all_combinations)}")
    for combo in all_combinations[:5]:  # Show first 5
        print(f"  {combo}")
    
    # Permutations with different lengths
    items = ['A', 'B', 'C']
    print(f"\nPermutations of {items}:")
    for r in range(1, len(items) + 1):
        perms = list(itertools.permutations(items, r))
        print(f"  Length {r}: {perms}")
    
    # Combinations with replacement
    numbers = [1, 2, 3]
    combos_with_replacement = list(itertools.combinations_with_replacement(numbers, 2))
    print(f"\nCombinations with replacement: {combos_with_replacement}")

def test_infinite_iterators():
    """Test infinite iterator patterns"""
    print("\n=== Infinite Iterators ===")
    
    # Cycle through a sequence
    cycle_iter = itertools.cycle(['A', 'B', 'C'])
    print("Cycling through ['A', 'B', 'C']:")
    for i, item in enumerate(cycle_iter):
        if i >= 10:
            break
        print(f"  {item}")
    
    # Count with step
    counter = itertools.count(10, 2.5)
    print(f"\nCounting from 10 by 2.5:")
    for i, num in enumerate(counter):
        if i >= 5:
            break
        print(f"  {num}")
    
    # Accumulate with custom function
    data = [1, 2, 3, 4, 5]
    accumulated = list(itertools.accumulate(data, operator.mul))
    print(f"\nAccumulated multiplication: {accumulated}")
    
    # Accumulate with strings
    words = ["Hello", " ", "World", "!"]
    concatenated = list(itertools.accumulate(words))
    print(f"Accumulated strings: {concatenated}")

def test_iterator_aggregation():
    """Test iterator aggregation functions"""
    print("\n=== Iterator Aggregation ===")
    
    # Chain multiple iterables
    list1 = [1, 2, 3]
    list2 = ['a', 'b', 'c']
    list3 = [True, False]
    
    chained = list(itertools.chain(list1, list2, list3))
    print(f"Chained: {chained}")
    
    # Chain from iterable
    nested_lists = [[1, 2], [3, 4], [5, 6]]
    chained_from_iterable = list(itertools.chain.from_iterable(nested_lists))
    print(f"Chained from iterable: {chained_from_iterable}")
    
    # Compress with boolean selector
    data = ['A', 'B', 'C', 'D', 'E']
    selectors = [1, 0, 1, 0, 1]
    compressed = list(itertools.compress(data, selectors))
    print(f"Compressed: {compressed}")
    
    # Dropwhile and takewhile
    numbers = [1, 3, 5, 2, 4, 6, 8]
    dropwhile_result = list(itertools.dropwhile(lambda x: x < 5, numbers))
    takewhile_result = list(itertools.takewhile(lambda x: x < 5, numbers))
    print(f"Dropwhile (<5): {dropwhile_result}")
    print(f"Takewhile (<5): {takewhile_result}")
    
    # Filterfalse
    mixed_numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    evens = list(filter(lambda x: x % 2 == 0, mixed_numbers))
    odds = list(itertools.filterfalse(lambda x: x % 2 == 0, mixed_numbers))
    print(f"Evens: {evens}")
    print(f"Odds: {odds}")

def test_iterator_grouping():
    """Test iterator grouping and partitioning"""
    print("\n=== Iterator Grouping ===")
    
    # Group by function
    words = ['apple', 'banana', 'cherry', 'apricot', 'blueberry', 'cranberry']
    grouped = {}
    for key, group in itertools.groupby(sorted(words), key=lambda x: x[0]):
        grouped[key] = list(group)
    print(f"Grouped by first letter: {grouped}")
    
    # Partition into n groups
    def partition(iterable, n):
        """Partition iterable into n equal parts"""
        items = list(iterable)
        size = len(items) // n
        if len(items) % n != 0:
            size += 1
        
        for i in range(0, len(items), size):
            yield items[i:i + size]
    
    numbers = list(range(20))
    partitions = list(partition(numbers, 4))
    print(f"Partitioned into 4: {partitions}")
    
    # Sliding window
    def sliding_window(iterable, size):
        """Create sliding window of given size"""
        it = iter(iterable)
        window = collections.deque(itertools.islice(it, size), maxlen=size)
        if len(window) == size:
            yield tuple(window)
        
        for item in it:
            window.append(item)
            yield tuple(window)
    
    data = [1, 2, 3, 4, 5, 6, 7]
    windows = list(sliding_window(data, 3))
    print(f"Sliding windows (size 3): {windows}")

# Advanced Functional Patterns

def test_function_composition():
    """Test function composition patterns"""
    print("\n=== Function Composition ===")
    
    def compose(*functions):
        """Compose multiple functions"""
        def composed_function(x):
            result = x
            for func in reversed(functions):
                result = func(result)
            return result
        return composed_function
    
    def double(x):
        return x * 2
    
    def add_one(x):
        return x + 1
    
    def square(x):
        return x ** 2
    
    # Compose functions
    double_add_one_square = compose(square, add_one, double)
    result = double_add_one_square(3)  # ((3 * 2) + 1) ^ 2 = 49
    print(f"Composed function result: {result}")
    
    # Pipeline pattern
    def pipeline(data, *functions):
        """Process data through a pipeline of functions"""
        result = data
        for func in functions:
            result = func(result)
        return result
    
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    result = pipeline(
        numbers,
        lambda x: filter(lambda n: n % 2 == 0, x),  # Keep evens
        lambda x: map(lambda n: n * 2, x),         # Double them
        lambda x: filter(lambda n: n > 5, x),      # Keep > 5
        list                                       # Convert to list
    )
    print(f"Pipeline result: {result}")

def test_partial_application():
    """Test partial function application"""
    print("\n=== Partial Function Application ===")
    
    def power(base, exponent):
        return base ** exponent
    
    # Create specialized functions
    square = functools.partial(power, exponent=2)
    cube = functools.partial(power, exponent=3)
    
    print(f"Square of 5: {square(5)}")
    print(f"Cube of 3: {cube(3)}")
    
    # Partial with multiple arguments
    def calculate_price(base_price, tax_rate, discount):
        return base_price * (1 + tax_rate) * (1 - discount)
    
    # Create specialized pricing functions
    us_pricing = functools.partial(calculate_price, tax_rate=0.08)  # 8% tax
    eu_pricing = functools.partial(calculate_price, tax_rate=0.20)  # 20% tax
    
    print(f"US price (base=100, discount=0.1): {us_pricing(100, discount=0.1)}")
    print(f"EU price (base=100, discount=0.1): {eu_pricing(100, discount=0.1)}")

def test_function_memoization():
    """Test function memoization patterns"""
    print("\n=== Function Memoization ===")
    
    # Manual memoization
    def memoize(func):
        cache = {}
        
        @functools.wraps(func)
        def wrapper(*args):
            if args in cache:
                print(f"Cache hit for {args}")
                return cache[args]
            
            print(f"Computing result for {args}")
            result = func(*args)
            cache[args] = result
            return result
        
        wrapper.cache_clear = lambda: cache.clear()
        wrapper.cache_info = lambda: f"Cache size: {len(cache)}"
        
        return wrapper
    
    @memoize
    def expensive_fibonacci(n):
        """Expensive Fibonacci calculation"""
        if n <= 1:
            return n
        return expensive_fibonacci(n-1) + expensive_fibonacci(n-2)
    
    print("Computing Fibonacci numbers:")
    print(f"Fibonacci(5): {expensive_fibonacci(5)}")
    print(f"Fibonacci(7): {expensive_fibonacci(7)}")
    print(f"Fibonacci(5) again: {expensive_fibonacci(5)}")
    print(f"Cache info: {expensive_fibonacci.cache_info()}")
    
    # Using functools.lru_cache
    @functools.lru_cache(maxsize=32)
    def cached_power(base, exponent):
        """Cached power function"""
        print(f"Computing {base}^{exponent}")
        return base ** exponent
    
    print(f"\nCached power results:")
    print(f"2^10: {cached_power(2, 10)}")
    print(f"3^5: {cached_power(3, 5)}")
    print(f"2^10 again: {cached_power(2, 10)}")
    print(f"Cache stats: {cached_power.cache_info()}")

def test_monad_patterns():
    """Test monad-like patterns in Python"""
    print("\n=== Monad-like Patterns ===")
    
    # Maybe monad pattern
    class Maybe:
        def __init__(self, value=None):
            self.value = value
        
        def bind(self, func):
            if self.value is None:
                return Maybe(None)
            try:
                result = func(self.value)
                return Maybe(result)
            except:
                return Maybe(None)
        
        def __str__(self):
            return f"Maybe({self.value})"
    
    def safe_divide(x, y):
        if y == 0:
            return None
        return x / y
    
    def safe_sqrt(x):
        if x < 0:
            return None
        return x ** 0.5
    
    # Chain operations safely
    result = Maybe(16).bind(safe_sqrt).bind(lambda x: safe_divide(x, 2))
    print(f"Safe computation result: {result}")
    
    # Error monad pattern
    class Result:
        def __init__(self, value=None, error=None):
            self.value = value
            self.error = error
        
        def is_ok(self):
            return self.error is None
        
        def bind(self, func):
            if self.error:
                return self
            try:
                result = func(self.value)
                if isinstance(result, Result):
                    return result
                return Result(result)
            except Exception as e:
                return Result(error=str(e))
        
        def __str__(self):
            if self.error:
                return f"Error({self.error})"
            return f"Ok({self.value})"
    
    def divide_result(x, y):
        try:
            return Result(x / y)
        except ZeroDivisionError:
            return Result(error="Division by zero")
    
    def sqrt_result(x):
        try:
            if x < 0:
                return Result(error="Cannot sqrt negative number")
            return Result(x ** 0.5)
        except:
            return Result(error="Square root error")
    
    # Chain operations with error handling
    result = Result(16).bind(lambda x: sqrt_result(x)).bind(lambda x: divide_result(x, 2))
    print(f"Result monad: {result}")
    
    # Error case
    error_result = Result(-4).bind(lambda x: sqrt_result(x)).bind(lambda x: divide_result(x, 2))
    print(f"Error case: {error_result}")

# Advanced Data Processing

@dataclass
class Person:
    name: str
    age: int
    city: str
    salary: float

def test_advanced_data_processing():
    """Test advanced data processing with functional patterns"""
    print("\n=== Advanced Data Processing ===")
    
    # Generate sample data
    people = [
        Person("Alice", 25, "New York", 50000),
        Person("Bob", 30, "San Francisco", 75000),
        Person("Charlie", 35, "New York", 60000),
        Person("Diana", 28, "Chicago", 55000),
        Person("Eve", 32, "San Francisco", 80000),
        Person("Frank", 29, "Chicago", 58000),
    ]
    
    # Group by city using itertools.groupby
    people_by_city = {}
    for city, group in itertools.groupby(sorted(people, key=lambda p: p.city), key=lambda p: p.city):
        people_by_city[city] = list(group)
    
    print("People grouped by city:")
    for city, city_people in people_by_city.items():
        avg_salary = sum(p.salary for p in city_people) / len(city_people)
        print(f"  {city}: {len(city_people)} people, avg salary: ${avg_salary:,.0f}")
    
    # Filter and transform pipeline
    high_earners = list(filter(lambda p: p.salary > 60000, people))
    names_and_salaries = list(map(lambda p: (p.name, p.salary), high_earners))
    
    print(f"\nHigh earners (>$60k): {names_and_salaries}")
    
    # Use operator module for cleaner code
    from operator import attrgetter, itemgetter
    
    # Sort by multiple criteria
    sorted_people = sorted(people, key=attrgetter('city', 'salary'), reverse=True)
    print("\nPeople sorted by city and salary (descending):")
    for person in sorted_people:
        print(f"  {person.name} ({person.city}): ${person.salary:,.0f}")
    
    # Calculate statistics using functional approach
    salaries = list(map(attrgetter('salary'), people))
    total_salary = functools.reduce(operator.add, salaries)
    max_salary = max(salaries)
    min_salary = min(salaries)
    
    print(f"\nSalary statistics:")
    print(f"  Total: ${total_salary:,.0f}")
    print(f"  Average: ${total_salary / len(salaries):,.0f}")
    print(f"  Max: ${max_salary:,.0f}")
    print(f"  Min: ${min_salary:,.0f}")

# Advanced Functional Utilities

def curry(func):
    """Simple curry implementation"""
    @functools.wraps(func)
    def curried(*args, **kwargs):
        if len(args) + len(kwargs) >= func.__code__.co_argcount:
            return func(*args, **kwargs)
        return functools.partial(curried, *args, **kwargs)
    return curried

def test_advanced_functional_utilities():
    """Test advanced functional programming utilities"""
    print("\n=== Advanced Functional Utilities ===")
    
    # Define helper functions
    def square(x):
        return x ** 2
    
    def add_one(x):
        return x + 1
    
    def double(x):
        return x * 2
    
    # Currying
    @curry
    def add_three_numbers(a, b, c):
        return a + b + c
    
    add_five = add_three_numbers(5)
    add_five_and_three = add_five(3)
    result = add_five_and_three(2)  # 5 + 3 + 2 = 10
    print(f"Curried function result: {result}")
    
    # Function composition with reduce
    def compose_reduce(*functions):
        return functools.reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)
    
    composed = compose_reduce(square, add_one, double)
    result = composed(3)  # square(add_one(double(3))) = square(add_one(6)) = square(7) = 49
    print(f"Composed with reduce: {result}")
    
    # Advanced filtering with multiple conditions
    data = list(range(20))
    
    # Filter with multiple conditions using all/any
    complex_filter = lambda x: all([
        x % 2 == 0,      # Even
        x % 3 == 0,      # Divisible by 3
        x > 10           # Greater than 10
    ])
    
    filtered = list(filter(complex_filter, data))
    print(f"Complex filter result: {filtered}")
    
    # Partition function
    def partition(predicate, iterable):
        """Partition iterable into two lists based on predicate"""
        trues, falses = [], []
        for item in iterable:
            if predicate(item):
                trues.append(item)
            else:
                falses.append(item)
        return trues, falses
    
    evens, odds = partition(lambda x: x % 2 == 0, range(10))
    print(f"Partitioned evens: {evens}")
    print(f"Partitioned odds: {odds}")

def main():
    """Main test function"""
    print("=== Advanced Python Functional Programming Patterns ===")
    
    test_advanced_combinatorics()
    test_infinite_iterators()
    test_iterator_aggregation()
    test_iterator_grouping()
    test_function_composition()
    test_partial_application()
    test_function_memoization()
    test_monad_patterns()
    test_advanced_data_processing()
    test_advanced_functional_utilities()
    
    print("\n=== All functional programming tests completed successfully! ===")

if __name__ == "__main__":
    main()
