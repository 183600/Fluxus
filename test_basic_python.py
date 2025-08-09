# Basic Python Feature Tests

# Test 1: Basic variables and arithmetic
x = 10
y = 20
result = x + y * 2
print(f"Basic arithmetic: {result}")

# Test 2: Control flow - if statements
if x > 5:
    print("x is greater than 5")
elif x == 5:
    print("x equals 5")
else:
    print("x is less than 5")

# Test 3: Loops
for i in range(5):
    print(f"For loop iteration: {i}")

# Test 4: While loops
count = 0
while count < 3:
    print(f"While loop count: {count}")
    count += 1

# Test 5: Functions
def greet(name):
    return f"Hello, {name}!"

message = greet("Python")
print(message)

# Test 6: Lists
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
print(f"List: {numbers}")
print(f"List length: {len(numbers)}")

# Test 7: Dictionaries
person = {"name": "Alice", "age": 30, "city": "New York"}
print(f"Dictionary: {person}")
print(f"Person name: {person['name']}")

# Test 8: Basic exception handling
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Caught division by zero")
finally:
    print("Finally block executed")

print("Basic tests completed successfully!")