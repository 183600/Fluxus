#!/usr/bin/env python3
"""
Python data structures test
"""

print("=== Testing Python Data Structures ===")

# Lists
print("=== Lists ===")
my_list = [1, 2, 3, 4, 5]
print("Original list: ", my_list)

# List operations
my_list.append(6)
print("After append: ", my_list)

my_list.insert(0, 0)
print("After insert: ", my_list)

my_list.remove(3)
print("After remove: ", my_list)

popped = my_list.pop()
print("Popped element: ", popped)
print("After pop: ", my_list)

# List slicing
print("First three elements: ", my_list[0:3])
print("Last element: ", my_list[-1])

# List methods
print("Length: ", len(my_list))
print("Sum: ", sum(my_list))
print("Max: ", max(my_list))
print("Min: ", min(my_list))

# String list
string_list = ["apple", "banana", "cherry"]
print("String list: ", string_list)
print("Joined: ", " ".join(string_list))

# Dictionaries
print("\n=== Dictionaries ===")
my_dict = {"name": "Alice", "age": 30, "city": "New York"}
print("Original dict: ", my_dict)

# Dictionary operations
my_dict["job"] = "Engineer"
print("After adding job: ", my_dict)

my_dict["age"] = 31
print("After updating age: ", my_dict)

removed_value = my_dict.pop("city")
print("Removed city: ", removed_value)
print("After pop: ", my_dict)

# Dictionary methods
print("Keys: ", my_dict.keys())
print("Values: ", my_dict.values())
print("Items: ", my_dict.items())

print("Has name key: ", "name" in my_dict)
print("Get name: ", my_dict.get("name"))
print("Get missing key: ", my_dict.get("missing", "default"))

# Sets
print("\n=== Sets ===")
my_set = {1, 2, 3, 4, 5}
print("Original set: ", my_set)

# Set operations
my_set.add(6)
print("After add: ", my_set)

my_set.remove(2)
print("After remove: ", my_set)

other_set = {4, 5, 6, 7, 8}
print("Other set: ", other_set)

print("Union: ", my_set.union(other_set))
print("Intersection: ", my_set.intersection(other_set))
print("Difference: ", my_set.difference(other_set))

print("Is subset: ", {1, 3}.issubset(my_set))
print("Is superset: ", my_set.issuperset({1, 3}))

# Tuples
print("\n=== Tuples ===")
my_tuple = (1, 2, 3, 4, 5)
print("Original tuple: ", my_tuple)

# Tuple operations
print("Length: ", len(my_tuple))
print("First element: ", my_tuple[0])
print("Last element: ", my_tuple[-1])
print("Slice: ", my_tuple[1:4])

# Tuple unpacking
a, b, c, d, e = my_tuple
print("Unpacked: ", a, b, c, d, e)

# Nested structures
print("\n=== Nested Structures ===")
nested_list = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
print("Nested list: ", nested_list)
print("Element [1][1]: ", nested_list[1][1])

nested_dict = {"person": {"name": "Bob", "age": 25}, "scores": [85, 90, 78]}
print("Nested dict: ", nested_dict)
print("Person name: ", nested_dict["person"]["name"])

# List of dictionaries
list_of_dicts = [
    {"name": "Alice", "score": 85},
    {"name": "Bob", "score": 90},
    {"name": "Charlie", "score": 78}
]
print("List of dicts: ", list_of_dicts)

# Complex operations
print("\n=== Complex Operations ===")
# List comprehension (basic)
squares = []
for x in range(1, 6):
    squares.append(x * x)
print("Squares: ", squares)

# Dictionary operations
word_count = {}
words = ["hello", "world", "hello", "python", "world", "hello"]
for word in words:
    if word in word_count:
        word_count[word] += 1
    else:
        word_count[word] = 1
print("Word count: ", word_count)

# Set operations for unique values
numbers = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
unique_numbers = set(numbers)
print("Unique numbers: ", unique_numbers)

print("=== Data Structures Test Complete ===")