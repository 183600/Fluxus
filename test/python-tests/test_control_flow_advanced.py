# Test advanced control flow features

# for...else
print("=== for...else ===")
numbers = [2, 4, 6, 8, 10]
for num in numbers:
    if num % 2 != 0:
        print(f"Found odd number: {num}")
        break
else:
    print("All numbers are even")

# for...else with break
print("\n=== for...else with break ===")
numbers = [2, 4, 5, 8, 10]
for num in numbers:
    if num % 2 != 0:
        print(f"Found odd number: {num}")
        break
else:
    print("All numbers are even")

# while...else
print("\n=== while...else ===")
count = 0
while count < 3:
    print(f"Count: {count}")
    count += 1
else:
    print("While loop completed normally")

# while...else with break
print("\n=== while...else with break ===")
count = 0
while count < 10:
    if count == 5:
        print("Breaking at 5")
        break
    count += 1
else:
    print("This won't print")

# Nested loops with break
print("\n=== Nested loops ===")
for i in range(3):
    for j in range(3):
        if i == j == 1:
            print(f"Breaking at i={i}, j={j}")
            break
        print(f"i={i}, j={j}")

# Multiple conditions in while
print("\n=== While with multiple conditions ===")
x = 0
y = 10
while x < 5 and y > 5:
    print(f"x={x}, y={y}")
    x += 1
    y -= 1

# pass statement
print("\n=== pass statement ===")
for i in range(5):
    if i % 2 == 0:
        pass  # Placeholder for future code
    else:
        print(f"Odd: {i}")

# Empty class/function with pass
class EmptyClass:
    pass

def empty_function():
    pass

print("EmptyClass and empty_function created")

# Match statement (Python 3.10+)
print("\n=== Match statement (Python 3.10+) ===")
def check_value(value):
    match value:
        case 0:
            return "zero"
        case 1:
            return "one"
        case 2:
            return "two"
        case _:
            return "other"

print(f"check_value(0): {check_value(0)}")
print(f"check_value(1): {check_value(1)}")
print(f"check_value(5): {check_value(5)}")

# Match with multiple patterns
def describe_point(point):
    match point:
        case (0, 0):
            return "origin"
        case (0, y):
            return f"on y-axis at {y}"
        case (x, 0):
            return f"on x-axis at {x}"
        case (x, y):
            return f"at ({x}, {y})"

print(f"describe_point((0, 0)): {describe_point((0, 0))}")
print(f"describe_point((0, 5)): {describe_point((0, 5))}")
print(f"describe_point((3, 4)): {describe_point((3, 4))}")

# Complex if-elif-else
print("\n=== Complex if-elif-else ===")
score = 85

if score >= 90:
    grade = "A"
elif score >= 80:
    grade = "B"
elif score >= 70:
    grade = "C"
elif score >= 60:
    grade = "D"
else:
    grade = "F"

print(f"Score {score} is grade {grade}")

# Inline conditions with multiple expressions
print("\n=== Multiple inline conditions ===")
x = 15
result = (
    "small" if x < 10 else
    "medium" if x < 20 else
    "large"
)
print(f"x={x} is {result}")

# Loop with continue
print("\n=== Loop with continue ===")
for i in range(10):
    if i % 2 == 0:
        continue
    if i > 7:
        break
    print(f"Odd number: {i}")

# Enumerate with condition
print("\n=== Enumerate with condition ===")
words = ["apple", "banana", "cherry", "date"]
for i, word in enumerate(words):
    if len(word) > 5:
        print(f"Index {i}: {word} (long word)")
    else:
        print(f"Index {i}: {word}")

# try-except-else-finally
print("\n=== try-except-else-finally ===")
def divide(a, b):
    try:
        result = a / b
    except ZeroDivisionError:
        print("Cannot divide by zero")
        result = None
    else:
        print(f"Division successful: {result}")
    finally:
        print("Cleanup complete")
    return result

divide(10, 2)
print()
divide(10, 0)

# Nested try-except
print("\n=== Nested try-except ===")
try:
    try:
        x = 1 / 0
    except ZeroDivisionError:
        print("Inner exception caught")
        raise ValueError("Converted error")
except ValueError as e:
    print(f"Outer exception caught: {e}")

# Multiple exceptions
print("\n=== Multiple exceptions ===")
def process_value(value):
    try:
        result = 10 / value
        index = [1, 2, 3][value]
    except (ZeroDivisionError, IndexError) as e:
        print(f"Error: {type(e).__name__}")
        return None
    return result

process_value(0)
process_value(5)

print("\n=== All control flow tests completed ===")
