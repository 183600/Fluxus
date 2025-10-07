# Test exception handling
def divide_numbers(a, b):
    try:
        result = a / b
        return result
    except ZeroDivisionError:
        return "Error: Cannot divide by zero"
    except TypeError:
        return "Error: Both arguments must be numbers"
    except Exception as e:
        return f"Error: {str(e)}"

def get_list_element(lst, index):
    try:
        return lst[index]
    except IndexError:
        return "Error: Index out of range"
    except TypeError:
        return "Error: First argument must be a list"

# Test exception handling
print("Testing divide_numbers:")
print(divide_numbers(10, 2))    # Normal case
print(divide_numbers(10, 0))    # Zero division
print(divide_numbers(10, "2"))  # Type error

print("\nTesting get_list_element:")
my_list = [1, 2, 3, 4, 5]
print(get_list_element(my_list, 2))      # Normal case
print(get_list_element(my_list, 10))     # Index error
print(get_list_element("not a list", 0)) # Type error

# Multiple exception handling
def process_data(data):
    try:
        if not isinstance(data, dict):
            raise TypeError("Data must be a dictionary")

        value = data.get("value")
        if value is None:
            raise ValueError("Value key not found")

        if value < 0:
            raise ValueError("Value cannot be negative")

        return value * 2

    except TypeError as e:
        return f"Type error: {e}"
    except ValueError as e:
        return f"Value error: {e}"
    except Exception as e:
        return f"Unexpected error: {e}"

print("\nTesting process_data:")
print(process_data({"value": 5}))
print(process_data({"value": -3}))
print(process_data({"key": "no_value"}))
print(process_data("not a dict"))

# Finally block
def demonstrate_finally():
    try:
        print("Inside try block")
        result = 10 / 2
        return result
    except:
        print("Inside except block")
        return 0
    finally:
        print("Inside finally block (always executes)")

print("\nTesting finally block:")
print(demonstrate_finally())