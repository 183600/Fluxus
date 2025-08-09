# Simple Python test to verify basic functionality

# Basic variables
x = 42
y = 3.14
s = "Hello"
b = True
n = None

print("=== Basic Variables ===")
print(f"x = {x}")
print(f"y = {y}")
print(f"s = {s}")
print(f"b = {b}")
print(f"n = {n}")

# Basic operations
print("\n=== Basic Operations ===")
print(f"5 + 3 = {5 + 3}")
print(f"5 - 3 = {5 - 3}")
print(f"5 * 3 = {5 * 3}")
print(f"5 / 3 = {5 / 3}")

# Logical operations
print(f"True and False = {True and False}")
print(f"True or False = {True or False}")

# Control flow
print("\n=== Control Flow ===")
if x > 0:
    print("x is positive")

# While loop
count = 0
while count < 2:
    print(f"count = {count}")
    count += 1

# For loop
for i in range(3):
    print(f"i = {i}")

# Basic function
print("\n=== Basic Functions ===")
def add(a, b):
    return a + b

print(f"add(2, 3) = {add(2, 3)}")

# Lists
print("\n=== Lists ===")
my_list = [1, 2, 3]
print(f"my_list = {my_list}")
print(f"my_list[0] = {my_list[0]}")
my_list.append(4)
print(f"after append = {my_list}")

print("\n=== Test Complete ===")
