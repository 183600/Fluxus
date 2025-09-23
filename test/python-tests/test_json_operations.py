# Test JSON operations
import json
import os

# Basic JSON serialization
def test_json_serialization():
    data = {
        "name": "Alice",
        "age": 30,
        "city": "New York",
        "hobbies": ["reading", "swimming", "coding"],
        "is_student": False,
        "address": {
            "street": "123 Main St",
            "city": "New York",
            "zip": "10001"
        }
    }

    # Convert to JSON string
    json_string = json.dumps(data, indent=2)
    print("JSON serialization:")
    print(json_string)

    # Convert back to Python object
    parsed_data = json.loads(json_string)
    print("\nParsed data:")
    print(f"Name: {parsed_data['name']}")
    print(f"Age: {parsed_data['age']}")
    print(f"Hobbies: {parsed_data['hobbies']}")

# JSON file operations
def test_json_file_operations():
    data = {
        "users": [
            {"id": 1, "name": "Alice", "email": "alice@example.com"},
            {"id": 2, "name": "Bob", "email": "bob@example.com"},
            {"id": 3, "name": "Charlie", "email": "charlie@example.com"}
        ],
        "settings": {
            "theme": "dark",
            "language": "en",
            "notifications": True
        }
    }

    filename = "test_data.json"

    # Write JSON to file
    with open(filename, 'w') as f:
        json.dump(data, f, indent=2)
    print(f"Data written to {filename}")

    # Read JSON from file
    with open(filename, 'r') as f:
        loaded_data = json.load(f)

    print("\nLoaded data:")
    print(f"Number of users: {len(loaded_data['users'])}")
    print(f"Theme: {loaded_data['settings']['theme']}")

    # Clean up
    os.remove(filename)

# JSON with custom objects
class Person:
    def __init__(self, name, age, city):
        self.name = name
        self.age = age
        self.city = city

    def to_dict(self):
        return {
            "name": self.name,
            "age": self.age,
            "city": self.city
        }

def test_json_custom_objects():
    people = [
        Person("Alice", 30, "New York"),
        Person("Bob", 25, "Los Angeles"),
        Person("Charlie", 35, "Chicago")
    ]

    # Convert to JSON-serializable format
    people_data = [person.to_dict() for person in people]
    json_string = json.dumps(people_data, indent=2)

    print("\nCustom objects to JSON:")
    print(json_string)

    # Parse back
    parsed_people = json.loads(json_string)
    print("\nParsed people:")
    for person in parsed_people:
        print(f"{person['name']}, {person['age']}, {person['city']}")

# JSON validation
def test_json_validation():
    valid_json = '{"name": "Alice", "age": 30}'
    invalid_json = '{"name": "Alice", "age": 30,'  # Missing closing brace

    print("\nJSON validation:")

    # Test valid JSON
    try:
        parsed = json.loads(valid_json)
        print(f"Valid JSON parsed: {parsed}")
    except json.JSONDecodeError as e:
        print(f"Valid JSON failed: {e}")

    # Test invalid JSON
    try:
        parsed = json.loads(invalid_json)
        print(f"Invalid JSON parsed: {parsed}")
    except json.JSONDecodeError as e:
        print(f"Invalid JSON failed: {e}")

# JSON pretty printing
def test_json_pretty_print():
    data = {
        "students": [
            {"name": "Alice", "grades": [90, 85, 95]},
            {"name": "Bob", "grades": [80, 75, 85]},
            {"name": "Charlie", "grades": [95, 90, 88]}
        ],
        "class_info": {
            "subject": "Mathematics",
            "teacher": "Mr. Smith",
            "room": "101"
        }
    }

    print("\nJSON pretty printing:")

    # Compact
    compact = json.dumps(data, separators=(',', ':'))
    print("Compact:")
    print(compact)

    # Pretty
    pretty = json.dumps(data, indent=2)
    print("\nPretty:")
    print(pretty)

# JSON data types
def test_json_data_types():
    data = {
        "string": "Hello World",
        "number": 42,
        "float": 3.14,
        "boolean": True,
        "null": None,
        "array": [1, 2, 3, "four", True],
        "nested": {
            "level1": {
                "level2": {
                    "deep": "value"
                }
            }
        }
    }

    json_string = json.dumps(data, indent=2)
    print("\nJSON data types:")
    print(json_string)

    parsed = json.loads(json_string)
    print("\nParsed types:")
    print(f"String: {type(parsed['string'])}")
    print(f"Number: {type(parsed['number'])}")
    print(f"Float: {type(parsed['float'])}")
    print(f"Boolean: {type(parsed['boolean'])}")
    print(f"Null: {type(parsed['null'])}")
    print(f"Array: {type(parsed['array'])}")

# Run all tests
print("=== JSON Serialization ===")
test_json_serialization()

print("\n=== JSON File Operations ===")
test_json_file_operations()

print("\n=== JSON Custom Objects ===")
test_json_custom_objects()

print("\n=== JSON Validation ===")
test_json_validation()

print("\n=== JSON Pretty Printing ===")
test_json_pretty_print()

print("\n=== JSON Data Types ===")
test_json_data_types()