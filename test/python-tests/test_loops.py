# Test various loop structures
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# For loop
print("For loop:")
for num in numbers:
    print(f"Number: {num}")

# While loop
print("\nWhile loop:")
count = 1
while count <= 5:
    print(f"Count: {count}")
    count += 1

# Range-based for loop
print("\nRange-based for loop:")
for i in range(5):
    print(f"Range value: {i}")

# Loop with break and continue
print("\nLoop with break and continue:")
for i in range(10):
    if i == 7:
        break  # Exit loop when i reaches 7
    if i % 2 == 0:
        continue  # Skip even numbers
    print(f"Odd number: {i}")

# Nested loops
print("\nNested loops:")
for i in range(3):
    for j in range(3):
        print(f"i={i}, j={j}")

# For loop with enumerate
print("\nFor loop with enumerate:")
fruits = ["apple", "banana", "cherry"]
for index, fruit in enumerate(fruits):
    print(f"Index {index}: {fruit}")

# For loop with zip
print("\nFor loop with zip:")
names = ["Alice", "Bob", "Charlie"]
ages = [25, 30, 35]
for name, age in zip(names, ages):
    print(f"{name} is {age} years old")