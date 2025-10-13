# Test all Python operators

# Bitwise operators
print("=== Bitwise Operators ===")
a = 60  # 0011 1100
b = 13  # 0000 1101

print(f"a & b = {a & b}")   # AND: 0000 1100 = 12
print(f"a | b = {a | b}")   # OR:  0011 1101 = 61
print(f"a ^ b = {a ^ b}")   # XOR: 0011 0001 = 49
print(f"~a = {~a}")         # NOT: -(a+1) = -61
print(f"a << 2 = {a << 2}") # Left shift: 1111 0000 = 240
print(f"a >> 2 = {a >> 2}") # Right shift: 0000 1111 = 15

# Identity operators
print("\n=== Identity Operators ===")
x = [1, 2, 3]
y = x
z = [1, 2, 3]

print(f"x is y: {x is y}")         # True
print(f"x is z: {x is z}")         # False
print(f"x is not z: {x is not z}") # True
print(f"x == z: {x == z}")         # True

# Identity with None
val1 = None
val2 = None
print(f"val1 is None: {val1 is None}")       # True
print(f"val1 is not None: {val1 is not None}") # False

# Ternary operator (conditional expression)
print("\n=== Ternary Operator ===")
age = 18
status = "adult" if age >= 18 else "minor"
print(f"Status: {status}")

num = 5
result = "positive" if num > 0 else "negative" if num < 0 else "zero"
print(f"Number is: {result}")

# Nested ternary
x = 10
y = 20
max_val = x if x > y else y
print(f"Max value: {max_val}")

# Walrus operator (assignment expression) - Python 3.8+
print("\n=== Walrus Operator ===")
# Simple walrus operator
if (n := 10) > 5:
    print(f"n is {n} which is greater than 5")

# In list comprehension
numbers = [1, 2, 3, 4, 5]
doubled = [result for x in numbers if (result := x * 2) > 4]
print(f"Doubled values > 4: {doubled}")

# Comparison chaining
print("\n=== Comparison Chaining ===")
x = 5
print(f"1 < x < 10: {1 < x < 10}")       # True
print(f"1 < x < 4: {1 < x < 4}")         # False
print(f"x > 0 and x < 10: {x > 0 and x < 10}") # True

# Multiple comparisons
a, b, c = 1, 2, 3
print(f"a < b < c: {a < b < c}")         # True
print(f"a < b and b < c: {a < b and b < c}") # True

print("\n=== All tests completed ===")
