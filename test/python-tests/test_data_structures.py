# Test various data structures

# Lists
print("=== Lists ===")
fruits = ["apple", "banana", "cherry", "date"]
print(f"Original list: {fruits}")

# List operations
fruits.append("elderberry")
fruits.insert(1, "blueberry")
fruits.remove("cherry")
popped = fruits.pop()
print(f"After operations: {fruits}")
print(f"Popped item: {popped}")

# List slicing
print(f"First three: {fruits[:3]}")
print(f"Last two: {fruits[-2:]}")

# List methods
numbers = [3, 1, 4, 1, 5, 9, 2, 6]
numbers.sort()
print(f"Sorted numbers: {numbers}")
numbers.reverse()
print(f"Reversed numbers: {numbers}")
print(f"Count of 1: {numbers.count(1)}")
print(f"Index of 9: {numbers.index(9)}")

# Tuples
print("\n=== Tuples ===")
coordinates = (10, 20)
point = (3, 4, 5)
print(f"Coordinates: {coordinates}")
print(f"Point: {point}")

# Tuple unpacking
x, y = coordinates
print(f"Unpacked: x={x}, y={y}")

# Sets
print("\n=== Sets ===")
set1 = {1, 2, 3, 4, 5}
set2 = {4, 5, 6, 7, 8}
print(f"Set 1: {set1}")
print(f"Set 2: {set2}")

# Set operations
print(f"Union: {set1 | set2}")
print(f"Intersection: {set1 & set2}")
print(f"Difference: {set1 - set2}")
print(f"Symmetric difference: {set1 ^ set2}")

# Set methods
set1.add(6)
set1.remove(2)
print(f"Modified set 1: {set1}")
print(f"Is 3 in set1? {3 in set1}")
print(f"Set length: {len(set1)}")

# Dictionaries (extended)
print("\n=== Dictionaries Extended ===")
student = {
    "name": "Alice",
    "age": 20,
    "grades": [85, 90, 78, 92],
    "courses": {
        "math": "A",
        "science": "B+",
        "history": "A-"
    }
}

print(f"Student: {student}")
print(f"Keys: {list(student.keys())}")
print(f"Values: {list(student.values())}")

# Nested dictionary access
print(f"Math grade: {student['courses']['math']}")

# Dictionary methods
student.update({"major": "Computer Science"})
age = student.pop("age")
print(f"After update and pop: {student}")
print(f"Popped age: {age}")

# Default dictionary
grades = student.get("grades", [])
print(f"Grades: {grades}")
missing = student.get("missing_key", "default_value")
print(f"Missing key with default: {missing}")

# Complex nested structure
complex_data = {
    "users": [
        {"id": 1, "name": "Alice", "active": True},
        {"id": 2, "name": "Bob", "active": False},
        {"id": 3, "name": "Charlie", "active": True}
    ],
    "metadata": {
        "total_users": 3,
        "active_users": 2,
        "created_date": "2024-01-01"
    }
}

print(f"\nComplex data: {complex_data}")
print(f"Active users: {[u['name'] for u in complex_data['users'] if u['active']]}")