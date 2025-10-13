# Test magic methods (dunder methods)

# __str__ and __repr__
print("=== __str__ and __repr__ ===")
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __str__(self):
        return f"Point({self.x}, {self.y})"
    
    def __repr__(self):
        return f"Point(x={self.x}, y={self.y})"

p = Point(3, 4)
print(f"str(p): {str(p)}")
print(f"repr(p): {repr(p)}")

# __eq__ and comparison methods
print("\n=== Comparison Magic Methods ===")
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age
    
    def __eq__(self, other):
        return self.age == other.age
    
    def __lt__(self, other):
        return self.age < other.age
    
    def __le__(self, other):
        return self.age <= other.age
    
    def __gt__(self, other):
        return self.age > other.age
    
    def __ge__(self, other):
        return self.age >= other.age
    
    def __ne__(self, other):
        return self.age != other.age

alice = Person("Alice", 25)
bob = Person("Bob", 30)
charlie = Person("Charlie", 25)

print(f"alice == charlie: {alice == charlie}")
print(f"alice != bob: {alice != bob}")
print(f"alice < bob: {alice < bob}")
print(f"bob > alice: {bob > alice}")
print(f"alice <= charlie: {alice <= charlie}")

# Arithmetic magic methods
print("\n=== Arithmetic Magic Methods ===")
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)
    
    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)
    
    def __truediv__(self, scalar):
        return Vector(self.x / scalar, self.y / scalar)
    
    def __str__(self):
        return f"Vector({self.x}, {self.y})"

v1 = Vector(1, 2)
v2 = Vector(3, 4)
v3 = v1 + v2
print(f"v1 + v2 = {v3}")

v4 = v2 - v1
print(f"v2 - v1 = {v4}")

v5 = v1 * 3
print(f"v1 * 3 = {v5}")

v6 = v2 / 2
print(f"v2 / 2 = {v6}")

# __len__ and __getitem__
print("\n=== Container Magic Methods ===")
class MyList:
    def __init__(self, items):
        self.items = items
    
    def __len__(self):
        return len(self.items)
    
    def __getitem__(self, index):
        return self.items[index]
    
    def __setitem__(self, index, value):
        self.items[index] = value
    
    def __contains__(self, item):
        return item in self.items

my_list = MyList([1, 2, 3, 4, 5])
print(f"len(my_list): {len(my_list)}")
print(f"my_list[2]: {my_list[2]}")
print(f"3 in my_list: {3 in my_list}")
print(f"10 in my_list: {10 in my_list}")

my_list[1] = 20
print(f"After setting my_list[1] = 20: {my_list[1]}")

# __call__
print("\n=== __call__ Magic Method ===")
class Multiplier:
    def __init__(self, factor):
        self.factor = factor
    
    def __call__(self, x):
        return x * self.factor

times_three = Multiplier(3)
result = times_three(5)
print(f"times_three(5) = {result}")

# __enter__ and __exit__ (context manager)
print("\n=== Context Manager Magic Methods ===")
class FileManager:
    def __init__(self, filename):
        self.filename = filename
        self.file = None
    
    def __enter__(self):
        print(f"  Opening {self.filename}")
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        print(f"  Closing {self.filename}")
        return False

with FileManager("test.txt") as fm:
    print("  Inside context")

# __iter__ and __next__ (iterator protocol)
print("\n=== Iterator Magic Methods ===")
class Counter:
    def __init__(self, start, end):
        self.current = start
        self.end = end
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.current >= self.end:
            raise StopIteration
        self.current += 1
        return self.current - 1

print("Counter from 0 to 5:")
for num in Counter(0, 5):
    print(f"  {num}")

# __hash__
print("\n=== __hash__ Magic Method ===")
class HashablePoint:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __hash__(self):
        return hash((self.x, self.y))
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

p1 = HashablePoint(1, 2)
p2 = HashablePoint(1, 2)
p3 = HashablePoint(3, 4)

point_dict = {p1: "Point 1", p3: "Point 3"}
print(f"Using hashable point as dict key: {point_dict[p2]}")

# __bool__
print("\n=== __bool__ Magic Method ===")
class Empty:
    def __init__(self, items):
        self.items = items
    
    def __bool__(self):
        return len(self.items) > 0

empty = Empty([])
not_empty = Empty([1, 2, 3])

print(f"bool(empty): {bool(empty)}")
print(f"bool(not_empty): {bool(not_empty)}")

# __format__
print("\n=== __format__ Magic Method ===")
class FormattedNumber:
    def __init__(self, value):
        self.value = value
    
    def __format__(self, format_spec):
        if format_spec == "hex":
            return hex(self.value)
        elif format_spec == "bin":
            return bin(self.value)
        else:
            return str(self.value)

num = FormattedNumber(42)
print(f"Default: {num}")
print(f"Hex: {num:hex}")
print(f"Binary: {num:bin}")

print("\n=== All magic method tests completed ===")
