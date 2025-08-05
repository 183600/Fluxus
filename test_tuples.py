# Test tuples
my_tuple = (1, 2, 3, 4, 5)
print(my_tuple)

# Access elements
print(my_tuple[0])
print(my_tuple[1])
print(my_tuple[-1])

# Slicing
print(my_tuple[1:3])
print(my_tuple[:3])
print(my_tuple[2:])

# Tuple operations
print(len(my_tuple))
print(3 in my_tuple)
print(10 in my_tuple)

# Tuples are immutable
# my_tuple[0] = 10  # This would cause an error

# Tuple methods
print(my_tuple.count(3))
print(my_tuple.index(4))