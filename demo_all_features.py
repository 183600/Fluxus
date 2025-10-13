# Fluxus编译器特性演示
# 展示所有已修复的功能

# 1. 列表字面量和打印
numbers = [1, 2, 3, 4, 5]
print("Numbers:", numbers)

# 2. 嵌套列表
matrix = [[1, 2], [3, 4]]
print("Matrix:", matrix)

# 3. 列表推导式
squares = [x * x for x in numbers]
print("Squares:", squares)

# 4. 带条件的列表推导式
evens = [x for x in numbers if x % 2 == 0]
print("Evens:", evens)

# 5. 函数参数类型推断（列表参数）
def calculate_sum(items):
    return sum(items)

result = calculate_sum([10, 20, 30])
print("Sum:", result)

# 6. enumerate
fruits = ["apple", "banana", "cherry"]
print("Enumerate:")
for index, fruit in enumerate(fruits):
    print(f"  {index}: {fruit}")

# 7. zip
names = ["Alice", "Bob", "Charlie"]
ages = [25, 30, 35]
print("Zip:")
for name, age in zip(names, ages):
    print(f"  {name} is {age} years old")

# 8. 空列表
empty = []
print("Empty list:", empty)

# 9. lambda表达式（C++关键字处理）
double_ = lambda x: x * 2
print("Double of 5:", double_(5))

print("All features working!")
