# Test error handling and exceptions
import sys

# Basic try-except
def basic_try_except():
    try:
        result = 10 / 0
    except ZeroDivisionError:
        print("Cannot divide by zero!")
    except ValueError:
        print("Invalid value!")
    else:
        print("No exception occurred!")
    finally:
        print("This always runs!")

# Multiple exception handling
def multiple_exceptions():
    test_cases = [10, 0, "five", [1, 2, 3]]

    for case in test_cases:
        try:
            result = 100 / case
            print(f"100 / {case} = {result}")
        except ZeroDivisionError:
            print(f"Cannot divide 100 by {case} (zero)")
        except TypeError:
            print(f"Cannot divide 100 by {case} (wrong type)")
        except Exception as e:
            print(f"Unexpected error with {case}: {e}")

# Custom exception
class InvalidAgeError(Exception):
    def __init__(self, age):
        self.age = age
        super().__init__(f"Invalid age: {age}. Age must be between 0 and 120.")

def validate_age(age):
    if not isinstance(age, (int, float)):
        raise TypeError("Age must be a number")
    if age < 0 or age > 120:
        raise InvalidAgeError(age)
    return f"Valid age: {age}"

# Custom exception test
def test_custom_exception():
    ages = [25, -5, 150, "thirty"]

    for age in ages:
        try:
            result = validate_age(age)
            print(result)
        except TypeError as e:
            print(f"TypeError: {e}")
        except InvalidAgeError as e:
            print(f"InvalidAgeError: {e}")
        except Exception as e:
            print(f"Unexpected error: {e}")

# Nested exception handling
def nested_exceptions():
    try:
        try:
            value = int("not a number")
        except ValueError:
            print("Inner exception: Invalid integer")
            raise  # Re-raise the exception
    except ValueError:
        print("Outer exception: Caught the re-raised exception")

# Exception chaining
def exception_chaining():
    try:
        try:
            open("nonexistent_file.txt", "r")
        except FileNotFoundError as e:
            raise RuntimeError("File operation failed") from e
    except RuntimeError as e:
        print(f"Chained exception: {e}")
        if e.__cause__:
            print(f"Caused by: {e.__cause__}")

# System exceptions
def system_exceptions():
    print(f"Python version: {sys.version}")
    print(f"Platform: {sys.platform}")

    try:
        # This will cause a SystemExit
        # sys.exit(1)  # Commented out to not exit the program
        pass
    except SystemExit:
        print("Program was asked to exit")

# Run all tests
print("=== Basic Try-Except ===")
basic_try_except()

print("\n=== Multiple Exceptions ===")
multiple_exceptions()

print("\n=== Custom Exception ===")
test_custom_exception()

print("\n=== Nested Exceptions ===")
nested_exceptions()

print("\n=== Exception Chaining ===")
exception_chaining()

print("\n=== System Exceptions ===")
system_exceptions()