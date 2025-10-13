# Test assert statements and assertion features

# Basic assert
print("=== Basic Assert ===")
x = 5
assert x == 5
print("Assert x == 5 passed")

assert x > 0
print("Assert x > 0 passed")

# Assert with message
print("\n=== Assert with Message ===")
age = 25
assert age >= 18, "Must be at least 18 years old"
print("Age assertion passed")

# Assert with complex expression
print("\n=== Complex Expression Assert ===")
numbers = [1, 2, 3, 4, 5]
assert len(numbers) == 5, f"Expected 5 elements, got {len(numbers)}"
print("Length assertion passed")

assert sum(numbers) == 15, "Sum should be 15"
print("Sum assertion passed")

# Assert with type checking
print("\n=== Type Checking Assert ===")
value = "hello"
assert isinstance(value, str), "Value must be a string"
print("Type assertion passed")

number = 42
assert isinstance(number, int), "Number must be an integer"
print("Number type assertion passed")

# Assert in functions
print("\n=== Assert in Functions ===")
def divide(a, b):
    assert b != 0, "Divisor cannot be zero"
    return a / b

result = divide(10, 2)
print(f"divide(10, 2) = {result}")

try:
    result = divide(10, 0)
except AssertionError as e:
    print(f"Caught expected assertion: {e}")

# Assert with boolean expressions
print("\n=== Boolean Expression Assert ===")
is_valid = True
assert is_valid, "Validation failed"
print("Validation assertion passed")

is_empty = False
assert not is_empty, "Should not be empty"
print("Empty check assertion passed")

# Multiple assertions
print("\n=== Multiple Assertions ===")
def validate_person(name, age):
    assert isinstance(name, str), "Name must be a string"
    assert len(name) > 0, "Name cannot be empty"
    assert isinstance(age, int), "Age must be an integer"
    assert age >= 0, "Age cannot be negative"
    assert age <= 150, "Age seems unrealistic"
    return True

result = validate_person("Alice", 25)
print("All person validations passed")

# Assert with membership
print("\n=== Assert with Membership ===")
valid_colors = ['red', 'green', 'blue']
color = 'red'
assert color in valid_colors, f"Color {color} not in valid colors"
print("Color membership assertion passed")

# Assert with range
print("\n=== Assert with Range ===")
temperature = 25
assert 0 <= temperature <= 100, "Temperature out of range"
print("Temperature range assertion passed")

# Assert with list operations
print("\n=== Assert with List Operations ===")
items = [1, 2, 3, 4, 5]
assert all(x > 0 for x in items), "All items must be positive"
print("All positive assertion passed")

values = [2, 4, 6, 8]
assert all(x % 2 == 0 for x in values), "All values must be even"
print("All even assertion passed")

# Assert with any
print("\n=== Assert with any ===")
numbers = [1, 3, 5, 7, 8]
assert any(x % 2 == 0 for x in numbers), "At least one even number required"
print("Any even assertion passed")

# Assert with string operations
print("\n=== Assert with String Operations ===")
text = "Hello, World!"
assert text.startswith("Hello"), "Text should start with Hello"
print("String start assertion passed")

assert "World" in text, "Text should contain World"
print("String contains assertion passed")

# Assert with dictionary
print("\n=== Assert with Dictionary ===")
person = {'name': 'Alice', 'age': 25}
assert 'name' in person, "Person must have a name"
assert 'age' in person, "Person must have an age"
print("Dictionary key assertions passed")

assert person['age'] >= 18, "Person must be an adult"
print("Dictionary value assertion passed")

# Assert with custom conditions
print("\n=== Assert with Custom Conditions ===")
def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(n ** 0.5) + 1):
        if n % i == 0:
            return False
    return True

num = 7
assert is_prime(num), f"{num} should be prime"
print(f"Prime assertion for {num} passed")

# Assert with class attributes
print("\n=== Assert with Class Attributes ===")
class Rectangle:
    def __init__(self, width, height):
        assert width > 0, "Width must be positive"
        assert height > 0, "Height must be positive"
        self.width = width
        self.height = height
    
    def area(self):
        result = self.width * self.height
        assert result > 0, "Area must be positive"
        return result

rect = Rectangle(5, 10)
area = rect.area()
print(f"Rectangle area: {area}")
print("Class assertion tests passed")

# Assert with comparison chains
print("\n=== Assert with Comparison Chains ===")
x = 5
assert 0 < x < 10, "x must be between 0 and 10"
print("Comparison chain assertion passed")

values = [1, 2, 3, 4, 5]
for v in values:
    assert 0 < v <= 5, f"Value {v} out of expected range"
print("All values in range assertion passed")

# Testing assertion failure handling
print("\n=== Testing Assertion Failure Handling ===")
def test_with_try_except():
    try:
        assert False, "This assertion should fail"
    except AssertionError as e:
        print(f"Caught assertion error: {e}")
        return "Handled"
    return "Not reached"

result = test_with_try_except()
print(f"Result: {result}")

print("\n=== All assertion tests completed ===")
