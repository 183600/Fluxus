# Test basic standard library modules

# math module
print("=== math module ===")
import math

print(f"math.pi: {math.pi}")
print(f"math.e: {math.e}")
print(f"math.sqrt(16): {math.sqrt(16)}")
print(f"math.pow(2, 3): {math.pow(2, 3)}")
print(f"math.ceil(3.2): {math.ceil(3.2)}")
print(f"math.floor(3.8): {math.floor(3.8)}")
print(f"math.sin(math.pi/2): {math.sin(math.pi/2)}")
print(f"math.cos(0): {math.cos(0)}")
print(f"math.log(math.e): {math.log(math.e)}")
print(f"math.log10(100): {math.log10(100)}")
print(f"math.factorial(5): {math.factorial(5)}")

# random module
print("\n=== random module ===")
import random

random.seed(42)  # For reproducible results
print(f"random.random(): {random.random()}")
print(f"random.randint(1, 10): {random.randint(1, 10)}")
print(f"random.uniform(1.0, 10.0): {random.uniform(1.0, 10.0)}")

choices = ['apple', 'banana', 'cherry']
print(f"random.choice(choices): {random.choice(choices)}")

numbers = [1, 2, 3, 4, 5]
print(f"random.sample(numbers, 3): {random.sample(numbers, 3)}")

deck = [1, 2, 3, 4, 5]
random.shuffle(deck)
print(f"After shuffle: {deck}")

# os module
print("\n=== os module ===")
import os

print(f"os.name: {os.name}")
print(f"Current PID: {os.getpid()}")
print(f"os.cpu_count(): {os.cpu_count()}")

# os.path
print("\n=== os.path module ===")
import os.path

path = "/home/user/file.txt"
print(f"os.path.basename(path): {os.path.basename(path)}")
print(f"os.path.dirname(path): {os.path.dirname(path)}")
print(f"os.path.split(path): {os.path.split(path)}")
print(f"os.path.splitext('file.txt'): {os.path.splitext('file.txt')}")
print(f"os.path.join('dir', 'file.txt'): {os.path.join('dir', 'file.txt')}")

# sys module
print("\n=== sys module ===")
import sys

print(f"sys.version_info.major: {sys.version_info.major}")
print(f"sys.version_info.minor: {sys.version_info.minor}")
print(f"sys.platform: {sys.platform}")
print(f"sys.maxsize: {sys.maxsize}")

# itertools module
print("\n=== itertools module ===")
import itertools

# count
counter = itertools.count(start=10, step=2)
print(f"First 5 from count(10, 2): {[next(counter) for _ in range(5)]}")

# cycle
colors = itertools.cycle(['red', 'green', 'blue'])
print(f"First 7 from cycle: {[next(colors) for _ in range(7)]}")

# repeat
repeater = itertools.repeat('hello', 3)
print(f"repeat('hello', 3): {list(repeater)}")

# chain
result = list(itertools.chain([1, 2], [3, 4], [5, 6]))
print(f"chain([1,2], [3,4], [5,6]): {result}")

# combinations
combs = list(itertools.combinations([1, 2, 3, 4], 2))
print(f"combinations([1,2,3,4], 2): {combs}")

# permutations
perms = list(itertools.permutations([1, 2, 3], 2))
print(f"permutations([1,2,3], 2): {perms}")

# product
prod = list(itertools.product([1, 2], ['a', 'b']))
print(f"product([1,2], ['a','b']): {prod}")

# functools module
print("\n=== functools module ===")
import functools

# reduce
numbers = [1, 2, 3, 4, 5]
total = functools.reduce(lambda x, y: x + y, numbers)
print(f"reduce(add, [1,2,3,4,5]): {total}")

product = functools.reduce(lambda x, y: x * y, numbers)
print(f"reduce(mul, [1,2,3,4,5]): {product}")

# partial
def multiply(x, y):
    return x * y

double = functools.partial(multiply, 2)
print(f"double(5) using partial: {double(5)}")

# lru_cache
@functools.lru_cache(maxsize=128)
def fibonacci(n):
    if n < 2:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print(f"fibonacci(10): {fibonacci(10)}")
print(f"Cache info: {fibonacci.cache_info()}")

# operator module
print("\n=== operator module ===")
import operator

print(f"operator.add(2, 3): {operator.add(2, 3)}")
print(f"operator.mul(4, 5): {operator.mul(4, 5)}")
print(f"operator.sub(10, 3): {operator.sub(10, 3)}")
print(f"operator.truediv(10, 3): {operator.truediv(10, 3)}")

data = [('Alice', 25), ('Bob', 30), ('Charlie', 20)]
sorted_by_age = sorted(data, key=operator.itemgetter(1))
print(f"Sorted by age: {sorted_by_age}")

# statistics module
print("\n=== statistics module ===")
import statistics

data = [1, 2, 3, 4, 5, 6, 7, 8, 9]
print(f"mean: {statistics.mean(data)}")
print(f"median: {statistics.median(data)}")
print(f"mode: {statistics.mode([1, 1, 2, 3, 3, 3, 4])}")
print(f"stdev: {statistics.stdev(data)}")
print(f"variance: {statistics.variance(data)}")

# time module
print("\n=== time module ===")
import time

current = time.time()
print(f"Current timestamp: {int(current)}")

local_time = time.localtime()
print(f"Local time: {time.strftime('%Y-%m-%d %H:%M:%S', local_time)}")

# string module
print("\n=== string module ===")
import string

print(f"ascii_lowercase: {string.ascii_lowercase}")
print(f"ascii_uppercase: {string.ascii_uppercase}")
print(f"digits: {string.digits}")
print(f"punctuation: {string.punctuation}")
print(f"whitespace: {repr(string.whitespace)}")

# Template strings
template = string.Template("Hello, $name! You are $age years old.")
result = template.substitute(name="Alice", age=25)
print(f"Template result: {result}")

# decimal module
print("\n=== decimal module ===")
from decimal import Decimal

d1 = Decimal('0.1')
d2 = Decimal('0.2')
print(f"Decimal('0.1') + Decimal('0.2'): {d1 + d2}")
print(f"Float 0.1 + 0.2: {0.1 + 0.2}")

# fractions module
print("\n=== fractions module ===")
from fractions import Fraction

f1 = Fraction(1, 3)
f2 = Fraction(1, 6)
print(f"Fraction(1, 3) + Fraction(1, 6): {f1 + f2}")
print(f"Fraction(1, 2) * 4: {Fraction(1, 2) * 4}")

print("\n=== All standard library tests completed ===")
