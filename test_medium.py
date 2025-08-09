# Test 1: F-strings
name = "Alice"
age = 30
print(f"Name: {name}, Age: {age}")

# Test 2: List operations
numbers = [1, 2, 3, 4, 5]
squared = [x**2 for x in numbers]
print(f"Original: {numbers}")
print(f"Squared: {squared}")

# Test 3: Dictionary operations
person = {"name": "Bob", "age": 25}
print(f"Person: {person}")
print(f"Name: {person['name']}")

# Test 4: Function with default parameters
def greet(name="World"):
    return f"Hello, {name}!"

print(greet())
print(greet("Python"))

# Test 5: Exception handling
try:
    result = 10 / 2
    print(f"Division result: {result}")
except ZeroDivisionError:
    print("Cannot divide by zero")
else:
    print("Division successful")

# Test 6: Boolean operations
a = True
b = False
print(f"a and b: {a and b}")
print(f"a or b: {a or b}")
print(f"not a: {not a}")

# Test 7: String operations
text = "Hello, World!"
print(f"Uppercase: {text.upper()}")
print(f"Lowercase: {text.lower()}")
print(f"Split: {text.split()}")

print("All tests completed!")