# Test list comprehensions and generator expressions
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Basic list comprehension
squares = [x**2 for x in numbers]
even_numbers = [x for x in numbers if x % 2 == 0]
odd_numbers = [x for x in numbers if x % 2 != 0]

print("Original numbers:", numbers)
print("Squares:", squares)
print("Even numbers:", even_numbers)
print("Odd numbers:", odd_numbers)

# List comprehension with multiple conditions
divisible_by_3_or_5 = [x for x in range(1, 21) if x % 3 == 0 or x % 5 == 0]
print("Divisible by 3 or 5:", divisible_by_3_or_5)

# Nested list comprehension
matrix = [[i+j for j in range(3)] for i in range(3)]
print("Matrix:", matrix)

# Generator expressions
sum_squares = sum(x**2 for x in numbers)
max_even = max(x for x in numbers if x % 2 == 0)

print("Sum of squares:", sum_squares)
print("Max even number:", max_even)

# Dictionary comprehension
word_lengths = {word: len(word) for word in ["hello", "world", "python", "programming"]}
print("Word lengths:", word_lengths)

# Set comprehension
unique_chars = {char for word in ["hello", "world", "python"] for char in word}
print("Unique characters:", unique_chars)

# Nested loops in comprehension
pairs = [(x, y) for x in [1, 2, 3] for y in [4, 5, 6]]
print("Pairs:", pairs)

# Conditional expression in comprehension
result = ["even" if x % 2 == 0 else "odd" for x in numbers]
print("Even/odd:", result)