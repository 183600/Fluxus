# Test tuple operations

# Tuple creation
print("=== Tuple Creation ===")
empty_tuple = ()
single_tuple = (1,)  # Note the comma
pair_tuple = (1, 2)
multi_tuple = (1, 2, 3, 4, 5)
mixed_tuple = (1, "hello", 3.14, True)

print(f"Empty tuple: {empty_tuple}")
print(f"Single tuple: {single_tuple}")
print(f"Pair tuple: {pair_tuple}")
print(f"Multi tuple: {multi_tuple}")
print(f"Mixed tuple: {mixed_tuple}")

# Tuple without parentheses
tuple_no_parens = 1, 2, 3, 4
print(f"Tuple without parens: {tuple_no_parens}")

# Tuple indexing and slicing
print("\n=== Tuple Indexing and Slicing ===")
numbers = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
print(f"First element: {numbers[0]}")
print(f"Last element: {numbers[-1]}")
print(f"Slice [2:5]: {numbers[2:5]}")
print(f"Slice [:3]: {numbers[:3]}")
print(f"Slice [5:]: {numbers[5:]}")
print(f"Slice [::2]: {numbers[::2]}")
print(f"Slice [::-1]: {numbers[::-1]}")

# Tuple unpacking
print("\n=== Tuple Unpacking ===")
point = (10, 20)
x, y = point
print(f"x = {x}, y = {y}")

# Multiple assignment
a, b, c = 1, 2, 3
print(f"a = {a}, b = {b}, c = {c}")

# Swap values
a, b = b, a
print(f"After swap: a = {a}, b = {b}")

# Nested tuple unpacking
nested = ((1, 2), (3, 4))
(a, b), (c, d) = nested
print(f"Nested unpacking: a={a}, b={b}, c={c}, d={d}")

# Tuple operations
print("\n=== Tuple Operations ===")
tuple1 = (1, 2, 3)
tuple2 = (4, 5, 6)

# Concatenation
combined = tuple1 + tuple2
print(f"Concatenation: {combined}")

# Repetition
repeated = tuple1 * 3
print(f"Repetition: {repeated}")

# Length
print(f"Length of tuple1: {len(tuple1)}")

# Membership
print(f"2 in tuple1: {2 in tuple1}")
print(f"10 in tuple1: {10 in tuple1}")

# Tuple methods
print("\n=== Tuple Methods ===")
numbers = (1, 2, 3, 2, 4, 2, 5)
print(f"Count of 2: {numbers.count(2)}")
print(f"Index of 3: {numbers.index(3)}")
print(f"Index of first 2: {numbers.index(2)}")

# Tuple as dictionary key (immutable)
print("\n=== Tuples as Dictionary Keys ===")
locations = {
    (0, 0): "origin",
    (1, 0): "east",
    (0, 1): "north"
}
print(f"Location at (0, 0): {locations[(0, 0)]}")
print(f"Location at (1, 0): {locations[(1, 0)]}")

# Nested tuples
print("\n=== Nested Tuples ===")
matrix = (
    (1, 2, 3),
    (4, 5, 6),
    (7, 8, 9)
)
print(f"Matrix: {matrix}")
print(f"Element [1][2]: {matrix[1][2]}")

# Tuple comparison
print("\n=== Tuple Comparison ===")
t1 = (1, 2, 3)
t2 = (1, 2, 3)
t3 = (1, 2, 4)
print(f"t1 == t2: {t1 == t2}")
print(f"t1 == t3: {t1 == t3}")
print(f"t1 < t3: {t1 < t3}")

# Convert list to tuple and vice versa
print("\n=== Type Conversion ===")
my_list = [1, 2, 3, 4]
my_tuple = tuple(my_list)
print(f"List to tuple: {my_tuple}")

back_to_list = list(my_tuple)
print(f"Tuple to list: {back_to_list}")

print("\n=== All tuple tests completed ===")
