# Test dictionary operations
# Create dictionaries
student_scores = {"Alice": 95, "Bob": 87, "Charlie": 92}
empty_dict = {}

# Dictionary operations
student_scores["David"] = 88  # Add new key-value pair
student_scores["Alice"] = 98  # Update existing value

print("Student scores:", student_scores)
print("Alice's score:", student_scores["Alice"])

# Dictionary methods
print("Keys:", list(student_scores.keys()))
print("Values:", list(student_scores.values()))
print("Items:", list(student_scores.items()))

# Check if key exists
print("Bob in dictionary:", "Bob" in student_scores)
print("Eve in dictionary:", "Eve" in student_scores)

# Remove key
removed_score = student_scores.pop("Charlie")
print("Removed Charlie's score:", removed_score)
print("Updated dictionary:", student_scores)

# Dictionary comprehension
squares = {x: x*x for x in range(1, 6)}
print("Squares dictionary:", squares)

# Nested dictionaries
people = {
    "person1": {"name": "Alice", "age": 25, "city": "New York"},
    "person2": {"name": "Bob", "age": 30, "city": "London"}
}
print("Alice's city:", people["person1"]["city"])