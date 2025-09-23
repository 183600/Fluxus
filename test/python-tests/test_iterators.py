# Test iterators and iteration protocols
import itertools

# Basic iterator
class CountDown:
    def __init__(self, start):
        self.current = start

    def __iter__(self):
        return self

    def __next__(self):
        if self.current < 0:
            raise StopIteration
        value = self.current
        self.current -= 1
        return value

# Test custom iterator
def test_custom_iterator():
    print("Custom countdown iterator:")
    countdown = CountDown(5)
    for num in countdown:
        print(num)

# List iteration
def test_list_iteration():
    fruits = ["apple", "banana", "cherry"]
    print("\nList iteration:")
    for fruit in fruits:
        print(fruit)

    # Using enumerate
    print("\nWith enumerate:")
    for index, fruit in enumerate(fruits):
        print(f"{index}: {fruit}")

    # Using reversed
    print("\nReversed list:")
    for fruit in reversed(fruits):
        print(fruit)

# Dictionary iteration
def test_dict_iteration():
    student = {"name": "Alice", "age": 20, "grade": "A"}

    print("\nDictionary iteration:")
    print("Keys:")
    for key in student.keys():
        print(key)

    print("\nValues:")
    for value in student.values():
        print(value)

    print("\nItems:")
    for key, value in student.items():
        print(f"{key}: {value}")

# String iteration
def test_string_iteration():
    text = "Hello"
    print("\nString iteration:")
    for char in text:
        print(char)

    # String methods that return iterators
    print("\nSplit string:")
    for word in text.split():
        print(word)

# Range iterator
def test_range():
    print("\nRange iteration:")
    for i in range(5):
        print(i)

    print("\nRange with step:")
    for i in range(0, 10, 2):
        print(i)

# File iteration
def test_file_iteration():
    filename = "temp_test_file.txt"

    # Create a test file
    with open(filename, 'w') as f:
        f.write("Line 1\n")
        f.write("Line 2\n")
        f.write("Line 3\n")

    print("\nFile iteration:")
    with open(filename, 'r') as f:
        for line in f:
            print(f"Line: {line.strip()}")

    # Clean up
    import os
    os.remove(filename)

# itertools examples
def test_itertools():
    print("\n=== itertools.count ===")
    counter = itertools.count(10, 2)
    for _ in range(5):
        print(next(counter))

    print("\n=== itertools.cycle ===")
    cycle_chars = itertools.cycle('ABC')
    for _ in range(6):
        print(next(cycle_chars))

    print("\n=== itertools.repeat ===")
    repeater = itertools.repeat('hello', 3)
    for item in repeater:
        print(item)

    print("\n=== itertools.chain ===")
    result = list(itertools.chain([1, 2, 3], ['a', 'b', 'c'], [4, 5, 6]))
    print("Chained:", result)

    print("\n=== itertools.accumulate ===")
    numbers = [1, 2, 3, 4, 5]
    accumulated = list(itertools.accumulate(numbers))
    print("Accumulated:", accumulated)

    print("\n=== itertools.permutations ===")
    perms = list(itertools.permutations([1, 2, 3], 2))
    print("Permutations:", perms)

    print("\n=== itertools.combinations ===")
    combos = list(itertools.combinations([1, 2, 3, 4], 2))
    print("Combinations:", combos)

    print("\n=== itertools.product ===")
    products = list(itertools.product([1, 2], ['a', 'b']))
    print("Products:", products)

# Generator functions
def countdown_generator(n):
    while n >= 0:
        yield n
        n -= 1

def test_generators():
    print("\nGenerator function:")
    for num in countdown_generator(3):
        print(num)

    # Generator expression
    print("\nGenerator expression:")
    squares = (x**2 for x in range(5))
    for square in squares:
        print(square)

# Iteration with zip
def test_zip():
    list1 = [1, 2, 3]
    list2 = ['a', 'b', 'c']
    list3 = [10, 20, 30]

    print("\nZip iteration:")
    for item in zip(list1, list2, list3):
        print(item)

    print("\nZip longest:")
    from itertools import zip_longest
    for item in zip_longest(list1, list2, [100], fillvalue='MISSING'):
        print(item)

# Run all tests
print("=== Custom Iterator ===")
test_custom_iterator()

test_list_iteration()
test_dict_iteration()
test_string_iteration()
test_range()
test_file_iteration()
test_itertools()
test_generators()
test_zip()