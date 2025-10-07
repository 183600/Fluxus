x = 10
y = 20
sum_result = x + y
diff = y - x
product = x * y
quotient = float(y) / float(x)  # Ensure float division with explicit types
remainder = y % x

print("Sum:", sum_result)
print("Difference:", diff)
print("Product:", product)
print("Quotient:", quotient)
print("Remainder:", remainder)

name = "Alice"
print("Name:", name)
print("Length:", len(name))

numbers = [1, 2, 3, 4, 5]
print("Numbers:", numbers)
print("First:", numbers[0])
print("Last:", numbers[4])

person = {"name": "Bob", "age": 30}
print("Person:", person)
print("Name:", person["name"])

for i in range(3):
    print("Count:", i)

def add_numbers(a, b):
    return a + b

result = add_numbers(5, 3)
print("add_numbers(5, 3):", result)

print("Test completed")