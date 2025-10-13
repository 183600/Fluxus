# Test comprehensive builtin functions

# Type conversion functions
print("=== Type Conversion ===")
print(f"int('123'): {int('123')}")
print(f"int(45.67): {int(45.67)}")
print(f"int('1010', 2): {int('1010', 2)}")  # Binary
print(f"int('FF', 16): {int('FF', 16)}")    # Hexadecimal

print(f"float('3.14'): {float('3.14')}")
print(f"float(5): {float(5)}")

print(f"str(123): {str(123)}")
print(f"str(3.14): {str(3.14)}")

print(f"bool(0): {bool(0)}")
print(f"bool(1): {bool(1)}")
print(f"bool(''): {bool('')}")
print(f"bool('hello'): {bool('hello')}")
print(f"bool([]): {bool([])}")
print(f"bool([1]): {bool([1])}")

# Math functions
print("\n=== Math Functions ===")
print(f"abs(-5): {abs(-5)}")
print(f"abs(-3.14): {abs(-3.14)}")

print(f"round(3.14159): {round(3.14159)}")
print(f"round(3.14159, 2): {round(3.14159, 2)}")
print(f"round(3.5): {round(3.5)}")

print(f"pow(2, 3): {pow(2, 3)}")
print(f"pow(2, 3, 5): {pow(2, 3, 5)}")  # (2^3) % 5

# Sequence functions
print("\n=== Sequence Functions ===")
numbers = [1, 2, 3, 4, 5]
print(f"len(numbers): {len(numbers)}")
print(f"min(numbers): {min(numbers)}")
print(f"max(numbers): {max(numbers)}")
print(f"sum(numbers): {sum(numbers)}")

print(f"min('hello'): {min('hello')}")
print(f"max('hello'): {max('hello')}")

# sorted and reversed
print("\n=== sorted and reversed ===")
unsorted = [3, 1, 4, 1, 5, 9, 2, 6]
print(f"sorted(unsorted): {sorted(unsorted)}")
print(f"sorted(unsorted, reverse=True): {sorted(unsorted, reverse=True)}")

text = "hello"
print(f"list(reversed(text)): {list(reversed(text))}")
print(f"list(reversed([1,2,3])): {list(reversed([1, 2, 3]))}")

# range
print("\n=== range ===")
print(f"list(range(5)): {list(range(5))}")
print(f"list(range(2, 8)): {list(range(2, 8))}")
print(f"list(range(0, 10, 2)): {list(range(0, 10, 2))}")
print(f"list(range(10, 0, -1)): {list(range(10, 0, -1))}")

# enumerate
print("\n=== enumerate ===")
fruits = ['apple', 'banana', 'cherry']
for i, fruit in enumerate(fruits):
    print(f"  {i}: {fruit}")

for i, fruit in enumerate(fruits, start=1):
    print(f"  {i}: {fruit}")

# zip
print("\n=== zip ===")
names = ['Alice', 'Bob', 'Charlie']
ages = [25, 30, 35]
cities = ['NYC', 'LA', 'SF']

for name, age, city in zip(names, ages, cities):
    print(f"  {name}, {age}, {city}")

zipped = list(zip(names, ages))
print(f"zipped: {zipped}")

# map
print("\n=== map ===")
numbers = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x**2, numbers))
print(f"squared: {squared}")

words = ['hello', 'world', 'python']
upper = list(map(str.upper, words))
print(f"upper: {upper}")

# filter
print("\n=== filter ===")
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
even = list(filter(lambda x: x % 2 == 0, numbers))
print(f"even: {even}")

words = ['apple', 'banana', 'cherry', 'avocado']
a_words = list(filter(lambda w: w.startswith('a'), words))
print(f"words starting with 'a': {a_words}")

# all and any
print("\n=== all and any ===")
print(f"all([True, True, True]): {all([True, True, True])}")
print(f"all([True, False, True]): {all([True, False, True])}")
print(f"all([]): {all([])}")  # Empty is True

print(f"any([False, False, False]): {any([False, False, False])}")
print(f"any([False, True, False]): {any([False, True, False])}")
print(f"any([]): {any([])}")  # Empty is False

numbers = [2, 4, 6, 8]
print(f"all even: {all(x % 2 == 0 for x in numbers)}")

# type and isinstance
print("\n=== type and isinstance ===")
x = 5
print(f"type(5): {type(x)}")
print(f"isinstance(5, int): {isinstance(x, int)}")
print(f"isinstance(5, (int, float)): {isinstance(x, (int, float))}")

s = "hello"
print(f"type('hello'): {type(s)}")
print(f"isinstance('hello', str): {isinstance(s, str)}")

# callable
print("\n=== callable ===")
def my_func():
    pass

print(f"callable(my_func): {callable(my_func)}")
print(f"callable(5): {callable(5)}")
print(f"callable(lambda x: x): {callable(lambda x: x)}")

# id
print("\n=== id ===")
a = [1, 2, 3]
b = a
c = [1, 2, 3]
print(f"id(a) == id(b): {id(a) == id(b)}")
print(f"id(a) == id(c): {id(a) == id(c)}")

# divmod
print("\n=== divmod ===")
quotient, remainder = divmod(17, 5)
print(f"divmod(17, 5): {quotient}, {remainder}")

# chr and ord
print("\n=== chr and ord ===")
print(f"ord('A'): {ord('A')}")
print(f"chr(65): {chr(65)}")
print(f"ord('a'): {ord('a')}")
print(f"chr(97): {chr(97)}")

# hex, oct, bin
print("\n=== hex, oct, bin ===")
print(f"hex(255): {hex(255)}")
print(f"oct(8): {oct(8)}")
print(f"bin(10): {bin(10)}")

# list, tuple, dict, set constructors
print("\n=== Collection Constructors ===")
print(f"list('hello'): {list('hello')}")
print(f"tuple([1,2,3]): {tuple([1, 2, 3])}")
print(f"set([1,2,2,3,3]): {set([1, 2, 2, 3, 3])}")
print(f"dict([('a',1), ('b',2)]): {dict([('a', 1), ('b', 2)])}")

# zip with * (unzip)
print("\n=== Unzipping ===")
pairs = [(1, 'a'), (2, 'b'), (3, 'c')]
numbers, letters = zip(*pairs)
print(f"numbers: {numbers}")
print(f"letters: {letters}")

# getattr, setattr, hasattr
print("\n=== Attribute Functions ===")
class MyClass:
    x = 10

obj = MyClass()
print(f"hasattr(obj, 'x'): {hasattr(obj, 'x')}")
print(f"getattr(obj, 'x'): {getattr(obj, 'x')}")
setattr(obj, 'y', 20)
print(f"getattr(obj, 'y'): {getattr(obj, 'y')}")
print(f"getattr(obj, 'z', 'default'): {getattr(obj, 'z', 'default')}")

print("\n=== All builtin function tests completed ===")
