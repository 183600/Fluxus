# Exception handling tests

def divide_numbers(a, b):
    try:
        result = a / b
        return result
    except ZeroDivisionError:
        print("Error: Cannot divide by zero!")
        return None
    except TypeError:
        print("Error: Invalid types for division!")
        return None

# Test normal division
result1 = divide_numbers(10, 2)
print("10 / 2 =", result1)

# Test division by zero
result2 = divide_numbers(10, 0)
print("10 / 0 =", result2)

# Test with finally block
def safe_divide(a, b):
    try:
        return a / b
    except ZeroDivisionError:
        print("Caught division by zero")
        return 0
    finally:
        print("Finally block executed")

result3 = safe_divide(20, 4)
print("20 / 4 =", result3)