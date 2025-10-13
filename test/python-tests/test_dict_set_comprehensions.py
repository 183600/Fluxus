# Test dictionary and set comprehensions

# Dictionary comprehension - basic
print("=== Basic Dictionary Comprehension ===")
squares = {x: x**2 for x in range(6)}
print(f"Squares: {squares}")

# Dictionary comprehension with condition
print("\n=== Dictionary Comprehension with Condition ===")
even_squares = {x: x**2 for x in range(10) if x % 2 == 0}
print(f"Even squares: {even_squares}")

# Dictionary comprehension from two lists
print("\n=== Dictionary from Two Lists ===")
keys = ['a', 'b', 'c', 'd']
values = [1, 2, 3, 4]
dict_from_lists = {k: v for k, v in zip(keys, values)}
print(f"Dict from lists: {dict_from_lists}")

# Dictionary comprehension with string manipulation
print("\n=== Dictionary with String Manipulation ===")
words = ['hello', 'world', 'python']
word_lengths = {word: len(word) for word in words}
print(f"Word lengths: {word_lengths}")

# Dictionary comprehension - swap keys and values
print("\n=== Swap Keys and Values ===")
original = {'a': 1, 'b': 2, 'c': 3}
swapped = {v: k for k, v in original.items()}
print(f"Original: {original}")
print(f"Swapped: {swapped}")

# Dictionary comprehension with nested logic
print("\n=== Dictionary with Nested Logic ===")
categorized = {
    x: "even" if x % 2 == 0 else "odd"
    for x in range(10)
}
print(f"Categorized: {categorized}")

# Dictionary comprehension from string
print("\n=== Character Frequency ===")
text = "hello world"
char_count = {char: text.count(char) for char in text if char != ' '}
print(f"Character frequency: {char_count}")

# Set comprehension - basic
print("\n=== Basic Set Comprehension ===")
squares_set = {x**2 for x in range(6)}
print(f"Squares set: {squares_set}")

# Set comprehension with condition
print("\n=== Set Comprehension with Condition ===")
even_numbers = {x for x in range(20) if x % 2 == 0}
print(f"Even numbers: {even_numbers}")

# Set comprehension - unique values
print("\n=== Set Comprehension - Unique Values ===")
numbers = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
unique = {x for x in numbers}
print(f"Original list: {numbers}")
print(f"Unique set: {unique}")

# Set comprehension with string
print("\n=== Unique Characters ===")
text = "hello world"
unique_chars = {char for char in text if char != ' '}
print(f"Unique characters: {unique_chars}")

# Set comprehension with transformation
print("\n=== Set with Transformation ===")
words = ['Hello', 'WORLD', 'Python', 'PROGRAMMING']
lowercase_set = {word.lower() for word in words}
print(f"Lowercase set: {lowercase_set}")

# Nested dictionary comprehension
print("\n=== Nested Dictionary Comprehension ===")
matrix_dict = {
    i: {j: i * j for j in range(1, 4)}
    for i in range(1, 4)
}
print("Matrix dictionary:")
for i, row in matrix_dict.items():
    print(f"  {i}: {row}")

# Dictionary comprehension with enumerate
print("\n=== Dictionary with Enumerate ===")
items = ['apple', 'banana', 'cherry']
indexed = {i: item for i, item in enumerate(items)}
print(f"Indexed items: {indexed}")

# Set operations with comprehensions
print("\n=== Set Operations ===")
set1 = {x for x in range(10) if x % 2 == 0}
set2 = {x for x in range(10) if x % 3 == 0}
print(f"Even numbers: {set1}")
print(f"Multiples of 3: {set2}")
print(f"Union: {set1 | set2}")
print(f"Intersection: {set1 & set2}")
print(f"Difference: {set1 - set2}")
print(f"Symmetric difference: {set1 ^ set2}")

# Dictionary comprehension with conditional value
print("\n=== Conditional Values in Dict ===")
numbers = range(10)
classified = {
    x: "positive" if x > 0 else "zero" if x == 0 else "negative"
    for x in range(-5, 6)
}
print(f"Classified: {classified}")

# Set comprehension with multiple conditions
print("\n=== Set with Multiple Conditions ===")
filtered = {
    x for x in range(50)
    if x % 2 == 0
    if x % 3 == 0
}
print(f"Numbers divisible by 2 and 3: {filtered}")

# Dictionary comprehension from nested structure
print("\n=== Dictionary from Nested Structure ===")
data = [
    ('Alice', 25),
    ('Bob', 30),
    ('Charlie', 35)
]
people = {name: age for name, age in data}
print(f"People: {people}")

# Filtering dictionary items
print("\n=== Filter Dictionary Items ===")
scores = {'Alice': 85, 'Bob': 92, 'Charlie': 78, 'David': 95}
high_scores = {name: score for name, score in scores.items() if score >= 90}
print(f"High scores (>= 90): {high_scores}")

# Set comprehension with function call
print("\n=== Set with Function Call ===")
words = ['hello', 'world', 'python', 'programming']
lengths = {len(word) for word in words}
print(f"Unique word lengths: {lengths}")

# Complex dictionary comprehension
print("\n=== Complex Dictionary Comprehension ===")
students = ['Alice', 'Bob', 'Charlie', 'David', 'Eve']
grades = [85, 92, 78, 95, 88]
status = {
    name: {'grade': grade, 'pass': grade >= 80}
    for name, grade in zip(students, grades)
}
print("Student status:")
for name, info in status.items():
    print(f"  {name}: {info}")

print("\n=== All dict/set comprehension tests completed ===")
