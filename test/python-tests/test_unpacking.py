# Test sequence unpacking and extended unpacking

# Basic unpacking
print("=== Basic Unpacking ===")
a, b = 1, 2
print(f"a={a}, b={b}")

# List unpacking
x, y, z = [1, 2, 3]
print(f"x={x}, y={y}, z={z}")

# String unpacking
a, b, c = "abc"
print(f"a={a}, b={b}, c={c}")

# Nested unpacking
print("\n=== Nested Unpacking ===")
(a, b), (c, d) = [(1, 2), (3, 4)]
print(f"a={a}, b={b}, c={c}, d={d}")

nested_list = [[1, 2], [3, [4, 5]]]
[x, y], [z, [w, v]] = nested_list
print(f"x={x}, y={y}, z={z}, w={w}, v={v}")

# Extended unpacking with * (Python 3+)
print("\n=== Extended Unpacking ===")

# Get first and rest
first, *rest = [1, 2, 3, 4, 5]
print(f"first={first}, rest={rest}")

# Get first, middle, and last
first, *middle, last = [1, 2, 3, 4, 5]
print(f"first={first}, middle={middle}, last={last}")

# Get first two and rest
a, b, *rest = [1, 2, 3, 4, 5]
print(f"a={a}, b={b}, rest={rest}")

# Get all but last
*head, tail = [1, 2, 3, 4, 5]
print(f"head={head}, tail={tail}")

# Get first, some middle, and last
first, *middle, second_last, last = [1, 2, 3, 4, 5, 6]
print(f"first={first}, middle={middle}, second_last={second_last}, last={last}")

# Unpacking with strings
print("\n=== Unpacking Strings ===")
first, *middle, last = "hello"
print(f"first={first}, middle={middle}, last={last}")

# Unpacking in function calls
print("\n=== Unpacking in Function Calls ===")
def add_three(a, b, c):
    return a + b + c

numbers = [1, 2, 3]
result = add_three(*numbers)
print(f"add_three(*[1,2,3]) = {result}")

# Dictionary unpacking
def greet(name, age):
    return f"Hello {name}, you are {age} years old"

person = {"name": "Alice", "age": 25}
message = greet(**person)
print(f"greet(**person) = {message}")

# Multiple unpacking in function calls
print("\n=== Multiple Unpacking ===")
list1 = [1, 2]
list2 = [3, 4]
combined = [*list1, *list2, 5, 6]
print(f"Combined list: {combined}")

dict1 = {"a": 1, "b": 2}
dict2 = {"c": 3, "d": 4}
combined_dict = {**dict1, **dict2}
print(f"Combined dict: {combined_dict}")

# Unpacking in comprehensions
print("\n=== Unpacking in Comprehensions ===")
pairs = [(1, 2), (3, 4), (5, 6)]
sums = [a + b for a, b in pairs]
print(f"Sums: {sums}")

# Unpacking with enumerate
print("\n=== Unpacking with enumerate ===")
words = ["hello", "world", "python"]
for i, word in enumerate(words):
    print(f"Index {i}: {word}")

# Unpacking with zip
print("\n=== Unpacking with zip ===")
names = ["Alice", "Bob", "Charlie"]
ages = [25, 30, 35]
for name, age in zip(names, ages):
    print(f"{name} is {age}")

# Unpacking in for loops
print("\n=== Unpacking in For Loops ===")
data = [(1, "a"), (2, "b"), (3, "c")]
for num, char in data:
    print(f"Number: {num}, Character: {char}")

# Ignoring values with _
print("\n=== Ignoring Values ===")
a, _, c = [1, 2, 3]
print(f"a={a}, c={c} (ignored middle value)")

first, *_, last = [1, 2, 3, 4, 5]
print(f"first={first}, last={last} (ignored middle values)")

print("\n=== All unpacking tests completed ===")
