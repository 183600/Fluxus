# Test lists
my_list = [1, 2, 3, 4, 5]
print(my_list)

# Access elements
print(my_list[0])
print(my_list[1])
print(my_list[-1])

# Slicing
print(my_list[1:3])
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
print(popped)
print(my_list)

# List methods
print(len(my_list))
print(3 in my_list)
print(10 in my_list)