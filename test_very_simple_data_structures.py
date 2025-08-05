# Test very simple data structures
print("=== Testing Lists ===")

# List creation and access
my_list = [1, 2, 3, 4, 5]
print(my_list)
print(my_list[0])
print(my_list[1])

# List operations
my_list.append(6)
print(my_list)
popped = my_list.pop()
print("Popped:", popped)
print("List after pop:", my_list)

# List methods
print("Length:", len(my_list))

print("\n=== Testing Dictionaries ===")

# Dictionary creation and access
my_dict = {"name": "Alice", "age": 30}
print(my_dict)
print("Name:", my_dict["name"])

# Dictionary operations
my_dict["job"] = "Engineer"
print(my_dict)
removed = my_dict.pop("age")
print("Removed:", removed)
print("Dict after pop:", my_dict)

print("Keys:", list(my_dict.keys()))
print("Values:", list(my_dict.values()))

print("\n=== Testing Tuples ===")

# Tuple creation and access
my_tuple = (1, 2, 3, "a", "b")
print(my_tuple)
print(my_tuple[0])
print(my_tuple[1])

# Tuple operations
print("Length:", len(my_tuple))

# Tuple unpacking
a, b, c, d, e = my_tuple
print("Unpacked:", a, b, c, d, e)

print("\n=== Testing Sets ===")

# Set creation and operations
my_set = {1, 2, 3, 4, 5}
my_set2 = {4, 5, 6, 7, 8}
print(my_set)
print(my_set2)

# Set operations
print("Union:", my_set.union(my_set2))
print("Intersection:", my_set.intersection(my_set2))

# Set methods
my_set.add(6)
print("After add:", my_set)
my_set.remove(2)
print("After remove:", my_set)

print("Length:", len(my_set))