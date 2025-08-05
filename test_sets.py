# Test sets
my_set = {1, 2, 3, 4, 5}
print(my_set)

# Set operations
my_set.add(6)
print(my_set)

my_set.remove(3)
print(my_set)

my_set.discard(10)  # No error if not exists
print(my_set)

# Set methods
print(3 in my_set)
print(1 in my_set)
print(len(my_set))

# Another set
other_set = {4, 5, 6, 7, 8}
print(other_set)

# Set operations
print(my_set.union(other_set))
print(my_set.intersection(other_set))
print(my_set.difference(other_set))
print(my_set.symmetric_difference(other_set))