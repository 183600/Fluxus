# Test data structures
print("=== Testing Lists ===")

# List creation and access
my_list = [1, 2, 3, 4, 5]
print(my_list)
print(my_list[0])
print(my_list[-1])

# List slicing
print(my_list[1:4])
print(my_list[:3])
print(my_list[2:])

# List operations
my_list.append(6)
print(my_list)
my_list.insert(0, 0)
print(my_list)
my_list.remove(3)
print(my_list)
popped = my_list.pop()
print("Popped:", popped)
print("List after pop:", my_list)

# List methods
print("Length:", len(my_list))
print("Contains 2:", 2 in my_list)
print("Index of 4:", my_list.index(4))
print("Count of 2:", my_list.count(2))

print("\n=== Testing Dictionaries ===")

# Dictionary creation and access
my_dict = {"name": "Alice", "age": 30, "city": "New York"}
print(my_dict)
print("Name:", my_dict["name"])
print("Age:", my_dict.get("age"))
print("Country:", my_dict.get("country", "Unknown"))

# Dictionary operations
my_dict["job"] = "Engineer"
print(my_dict)
del my_dict["age"]
print(my_dict)
removed = my_dict.pop("city")
print("Removed:", removed)
print("Dict after pop:", my_dict)

# Dictionary methods
print("Keys:", list(my_dict.keys()))
print("Values:", list(my_dict.values()))
print("Items:", list(my_dict.items()))
print("Length:", len(my_dict))
print("Has name:", "name" in my_dict)

print("\n=== Testing Tuples ===")

# Tuple creation and access
my_tuple = (1, 2, 3, "a", "b")
print(my_tuple)
print(my_tuple[0])
print(my_tuple[-1])

# Tuple operations
print("Length:", len(my_tuple))
print("Contains 2:", 2 in my_tuple)
print("Count of 1:", my_tuple.count(1))
print("Index of 'a':", my_tuple.index("a"))

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
print("Difference:", my_set.difference(my_set2))
print("Symmetric difference:", my_set.symmetric_difference(my_set2))

# Set methods
my_set.add(6)
print("After add:", my_set)
my_set.remove(2)
print("After remove:", my_set)
my_set.discard(10)  # No error if not exists
print("After discard:", my_set)

print("Length:", len(my_set))
print("Contains 3:", 3 in my_set)