# Test various function argument types

# Default arguments
print("=== Default Arguments ===")
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

print(greet("Alice"))
print(greet("Bob", "Hi"))
print(greet("Charlie", greeting="Hey"))

def power(base, exponent=2):
    return base ** exponent

print(f"power(3) = {power(3)}")
print(f"power(3, 3) = {power(3, 3)}")

# Keyword arguments
print("\n=== Keyword Arguments ===")
def describe_person(name, age, city):
    return f"{name} is {age} years old and lives in {city}"

# Positional
print(describe_person("Alice", 25, "NYC"))

# Keyword
print(describe_person(name="Bob", age=30, city="LA"))

# Mixed
print(describe_person("Charlie", city="SF", age=35))

# Variable positional arguments (*args)
print("\n=== Variable Positional Arguments (*args) ===")
def sum_all(*args):
    total = 0
    for num in args:
        total += num
    return total

print(f"sum_all(1, 2, 3) = {sum_all(1, 2, 3)}")
print(f"sum_all(1, 2, 3, 4, 5) = {sum_all(1, 2, 3, 4, 5)}")
print(f"sum_all() = {sum_all()}")

def print_args(*args):
    for i, arg in enumerate(args):
        print(f"  Arg {i}: {arg}")

print("print_args('a', 'b', 'c'):")
print_args('a', 'b', 'c')

# Variable keyword arguments (**kwargs)
print("\n=== Variable Keyword Arguments (**kwargs) ===")
def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"  {key}: {value}")

print("print_info(name='Alice', age=25, city='NYC'):")
print_info(name='Alice', age=25, city='NYC')

def build_profile(**kwargs):
    profile = {}
    for key, value in kwargs.items():
        profile[key] = value
    return profile

profile = build_profile(name="Bob", age=30, occupation="Engineer")
print(f"Profile: {profile}")

# Combining different argument types
print("\n=== Combining Argument Types ===")
def complex_function(a, b, *args, c=10, **kwargs):
    print(f"  a={a}, b={b}")
    print(f"  args={args}")
    print(f"  c={c}")
    print(f"  kwargs={kwargs}")
    return a + b + sum(args) + c + sum(kwargs.values())

print("complex_function(1, 2, 3, 4, c=5, x=6, y=7):")
result = complex_function(1, 2, 3, 4, c=5, x=6, y=7)
print(f"  Result: {result}")

# Keyword-only arguments (after *)
print("\n=== Keyword-Only Arguments ===")
def keyword_only(a, b, *, c, d):
    return a + b + c + d

# This works
print(f"keyword_only(1, 2, c=3, d=4) = {keyword_only(1, 2, c=3, d=4)}")

# Position-only arguments (before /) - Python 3.8+
print("\n=== Position-Only Arguments ===")
def position_only(a, b, /, c, d):
    return a + b + c + d

# This works
print(f"position_only(1, 2, 3, 4) = {position_only(1, 2, 3, 4)}")
print(f"position_only(1, 2, c=3, d=4) = {position_only(1, 2, c=3, d=4)}")

# Combining position-only and keyword-only
print("\n=== Position-Only and Keyword-Only ===")
def mixed_args(a, b, /, c, d, *, e, f):
    return a + b + c + d + e + f

result = mixed_args(1, 2, 3, 4, e=5, f=6)
print(f"mixed_args(1, 2, 3, 4, e=5, f=6) = {result}")

# Unpacking arguments
print("\n=== Unpacking Arguments ===")
def add_three(a, b, c):
    return a + b + c

numbers = [1, 2, 3]
print(f"add_three(*[1, 2, 3]) = {add_three(*numbers)}")

def greet_person(name, age, city):
    return f"{name}, {age}, {city}"

person = {"name": "Alice", "age": 25, "city": "NYC"}
print(f"greet_person(**person) = {greet_person(**person)}")

# Default mutable arguments (common pitfall)
print("\n=== Default Mutable Arguments ===")
def append_to(element, target=None):
    if target is None:
        target = []
    target.append(element)
    return target

list1 = append_to(1)
list2 = append_to(2)
print(f"list1: {list1}")
print(f"list2: {list2}")

# Multiple return values (tuple unpacking)
print("\n=== Multiple Return Values ===")
def get_coordinates():
    return 10, 20, 30

x, y, z = get_coordinates()
print(f"Coordinates: x={x}, y={y}, z={z}")

def divide_and_remainder(a, b):
    return a // b, a % b

quotient, remainder = divide_and_remainder(17, 5)
print(f"17 ÷ 5 = {quotient} remainder {remainder}")

print("\n=== All function argument tests completed ===")
