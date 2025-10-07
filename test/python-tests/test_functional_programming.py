# Test functional programming concepts
from functools import reduce, partial
import operator

# Map function
def test_map():
    numbers = [1, 2, 3, 4, 5]
    squares = list(map(lambda x: x**2, numbers))
    doubles = list(map(lambda x: x * 2, numbers))

    print("Original numbers:", numbers)
    print("Squares:", squares)
    print("Doubles:", doubles)

    # Map with multiple iterables
    list1 = [1, 2, 3]
    list2 = [4, 5, 6]
    sum_lists = list(map(lambda x, y: x + y, list1, list2))
    print("Sum of lists:", sum_lists)

# Filter function
def test_filter():
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    evens = list(filter(lambda x: x % 2 == 0, numbers))
    odds = list(filter(lambda x: x % 2 != 0, numbers))
    greater_than_5 = list(filter(lambda x: x > 5, numbers))

    print("Original numbers:", numbers)
    print("Even numbers:", evens)
    print("Odd numbers:", odds)
    print("Greater than 5:", greater_than_5)

# Reduce function
def test_reduce():
    numbers = [1, 2, 3, 4, 5]
    sum_all = reduce(lambda x, y: x + y, numbers)
    product = reduce(lambda x, y: x * y, numbers)
    max_value = reduce(lambda x, y: x if x > y else y, numbers)

    print("Original numbers:", numbers)
    print("Sum of all:", sum_all)
    print("Product:", product)
    print("Maximum value:", max_value)

    # With initial value
    with_initial = reduce(lambda x, y: x + y, numbers, 10)
    print("Sum with initial value 10:", with_initial)

# Partial function application
def test_partial():
    def multiply(x, y):
        return x * y

    double = partial(multiply, 2)
    triple = partial(multiply, 3)

    print("Double 5:", double(5))
    print("Triple 5:", triple(5))

    # Partial with positional arguments
    power = partial(pow, 2)  # 2^x
    print("2^3:", power(3))
    print("2^4:", power(4))

# Function composition
def compose(f, g):
    return lambda x: f(g(x))

def test_composition():
    def add_one(x):
        return x + 1

    def multiply_by_two(x):
        return x * 2

    add_then_multiply = compose(multiply_by_two, add_one)
    multiply_then_add = compose(add_one, multiply_by_two)

    print("Add then multiply (5):", add_then_multiply(5))
    print("Multiply then add (5):", multiply_then_add(5))

# Higher-order functions
def test_higher_order():
    def apply_operation(func, x, y):
        return func(x, y)

    def add(x, y):
        return x + y

    def subtract(x, y):
        return x - y

    def multiply(x, y):
        return x * y

    print("Apply add:", apply_operation(add, 5, 3))
    print("Apply subtract:", apply_operation(subtract, 5, 3))
    print("Apply multiply:", apply_operation(multiply, 5, 3))

# Currying
def curry_add(x):
    return lambda y: x + y

def test_currying():
    add5 = curry_add(5)
    add10 = curry_add(10)

    print("Add 5 to 3:", add5(3))
    print("Add 10 to 3:", add10(3))

# Pure functions (no side effects)
def pure_function(x, y):
    return x + y

def impure_function(x, y):
    # Has side effect (print statement)
    print(f"Adding {x} and {y}")
    return x + y

# Test immutability
def test_immutability():
    original = [1, 2, 3]
    modified = list(map(lambda x: x * 2, original))

    print("Original list:", original)
    print("Modified list:", modified)

# Run all tests
print("=== Map Function ===")
test_map()

print("\n=== Filter Function ===")
test_filter()

print("\n=== Reduce Function ===")
test_reduce()

print("\n=== Partial Function Application ===")
test_partial()

print("\n=== Function Composition ===")
test_composition()

print("\n=== Higher-order Functions ===")
test_higher_order()

print("\n=== Currying ===")
test_currying()

print("\n=== Immutability ===")
test_immutability()