# List comprehension tests
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Basic list comprehension
squares = [x * x for x in numbers]
evens = [x for x in numbers if x % 2 == 0]
doubled_odds = [x * 2 for x in numbers if x % 2 == 1]

print("Squares:", squares)
print("Evens:", evens)
print("Doubled odds:", doubled_odds)

# Nested list comprehension
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened = [item for row in matrix for item in row]
print("Flattened matrix:", flattened)