# Test dictionaries
my_dict = {"name": "Alice", "age": 25, "city": "New York"}
print(my_dict)

# Access elements
print(my_dict["name"])
print(my_dict["age"])

# Dictionary operations
my_dict["job"] = "Engineer"
print(my_dict)

del my_dict["city"]
print(my_dict)

# Dictionary methods
print("name" in my_dict)
print("city" in my_dict)
print(my_dict.keys())
print(my_dict.values())
print(my_dict.items())

# Get with default
print(my_dict.get("name"))
print(my_dict.get("country", "Unknown"))