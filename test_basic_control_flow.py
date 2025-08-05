#!/usr/bin/env python3
# Test 3: Basic language features - Control flow
print("=== Testing Control Flow ===")

# If/elif/else statements
print("If/elif/else statements:")
age = 25
if age < 13:
    print("Child")
elif age < 18:
    print("Teenager")
elif age < 65:
    print("Adult")
else:
    print("Senior")

# While loop
print("\nWhile loop:")
count = 0
while count < 5:
    print(f"Count: {count}")
    count += 1

# For loop with range
print("\nFor loop with range:")
for i in range(5):
    print(f"Range iteration: {i}")

# For loop with break and continue
print("\nFor loop with break and continue:")
for i in range(10):
    if i == 3:
        continue  # Skip 3
    if i == 7:
        break     # Stop at 7
    print(f"Break/continue test: {i}")

# Nested loops
print("\nNested loops:")
for i in range(3):
    for j in range(2):
        print(f"Nested: i={i}, j={j}")

print("=== Control Flow Test Complete ===\n")